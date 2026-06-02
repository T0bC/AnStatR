# Cliff's Delta functions extracted from Rallfun-v43.R
# Only the functions needed for cidmulv2_labelled are included here
# to avoid stack overflow when loading the full 95,000-line Rallfun library.
#
# Original source: Rand Wilcox's Rallfun-v43.R
# Functions: elimna, binomci, binomcipv, cid, cidv2, bmp, cidmulv2_labelled

#' Remove rows with missing values
#' @keywords internal
elimna <- function(m) {
DONE <- FALSE
if (is.list(m) && is.matrix(m)) {
  z <- pool.a.list(m)
  m <- matrix(z, ncol = ncol(m))
  DONE <- TRUE
}
if (!DONE) {
  if (is.list(m) && is.matrix(m[[1]])) {
    for (j in 1:length(m)) m[[j]] <- na.omit(m[[j]])
    e <- m
    DONE <- TRUE
  }
}
if (!DONE) {
  if (is.list(m) && is.null(dim(m))) {
    for (j in 1:length(m)) m[[j]] <- as.vector(na.omit(m[[j]]))
    e <- m
    DONE <- TRUE
  }
}
if (!DONE) {
  m <- as.matrix(m)
  ikeep <- c(1:nrow(m))
  for (i in 1:nrow(m)) if (sum(is.na(m[i, ]) >= 1)) ikeep[i] <- 0
  e <- m[ikeep[ikeep >= 1], ]
}
e
}

#' Binomial confidence interval using Pratt's method
#' @keywords internal
binomci <- function(x = sum(y), nn = length(y), y = NULL, n = NA, alpha = .05) {
if (!is.null(y)) {
  y <- elimna(y)
  nn <- length(y)
}
if (nn == 1) stop("Something is wrong: number of observations is only 1")
n <- nn
if (x != n && x != 0) {
  z <- qnorm(1 - alpha / 2)
  A <- ((x + 1) / (n - x))^2
  B <- 81 * (x + 1) * (n - x) - 9 * n - 8
  C <- (0 - 3) * z * sqrt(9 * (x + 1) * (n - x) * (9 * n + 5 - z^2) + n + 1)
  D <- 81 * (x + 1)^2 - 9 * (x + 1) * (2 + z^2) + 1
  E <- 1 + A * ((B + C) / D)^3
  upper <- 1 / E
  A <- (x / (n - x - 1))^2
  B <- 81 * x * (n - x - 1) - 9 * n - 8
  C <- 3 * z * sqrt(9 * x * (n - x - 1) * (9 * n + 5 - z^2) + n + 1)
  D <- 81 * x^2 - 9 * x * (2 + z^2) + 1
  E <- 1 + A * ((B + C) / D)^3
  lower <- 1 / E
}
if (x == 0) {
  lower <- 0
  upper <- 1 - alpha^(1 / n)
}
if (x == 1) {
  upper <- 1 - (alpha / 2)^(1 / n)
  lower <- 1 - (1 - alpha / 2)^(1 / n)
}
if (x == n - 1) {
  lower <- (alpha / 2)^(1 / n)
  upper <- (1 - alpha / 2)^(1 / n)
}
if (x == n) {
  lower <- alpha^(1 / n)
  upper <- 1
}
phat <- x / n
list(phat = phat, ci = c(lower, upper), n = n)
}

#' Binomial CI with p-value
#' @keywords internal
binomcipv <- function(x = sum(y), nn = length(y), y = NULL, n = NA,
                      alpha = .05, nullval = .5) {
if (is.logical(y)) {
  y <- elimna(y)
  temp <- rep(0, length(y))
  temp[y] <- 1
  y <- temp
}
res <- binomci(x = x, nn = nn, y = y, alpha = alpha)
ci <- res$ci
alph <- c(1:99) / 100
for (i in 1:99) {
  irem <- i
  chkit <- binomci(x = x, nn = nn, y = y, alpha = alph[i])$ci
  if (chkit[1] > nullval || chkit[2] < nullval) break
}
p.value <- irem / 100
if (p.value <= .1) {
  iup <- (irem + 1) / 100
  alph <- seq(.001, iup, .001)
  for (i in 1:length(alph)) {
    p.value <- alph[i]
    chkit <- binomci(x = x, nn = nn, y = y, alpha = alph[i])$ci
    if (chkit[1] > nullval || chkit[2] < nullval) break
  }
}
if (p.value <= .001) {
  alph <- seq(.0001, .001, .0001)
  for (i in 1:length(alph)) {
    p.value <- alph[i]
    chkit <- binomci(x = x, nn = nn, y = y, alpha = alph[i])$ci
    if (chkit[1] > nullval || chkit[2] < nullval) break
  }
}
list(n = nn, phat = res$phat, ci = res$ci, p.value = p.value)
}

#' Cliff's method for two independent groups
#' @keywords internal
cid <- function(x, y, alpha = .05, plotit = FALSE, pop = 0, fr = .8,
                rval = 15, xlab = "", ylab = "") {
x <- x[!is.na(x)]
y <- y[!is.na(y)]
if (length(x) * length(y) > 10^6) {
  stop("Use bmp with a large sample size.")
}
m <- outer(x, y, FUN = "-")
msave <- m
m <- sign(m)
d <- mean(m)
phat <- (1 - d) / 2
flag <- TRUE
if (phat == 0 || phat == 1) flag <- FALSE
q0 <- sum(msave == 0) / length(msave)
qxly <- sum(msave < 0) / length(msave)
qxgy <- sum(msave > 0) / length(msave)
c.sum <- matrix(c(qxly, q0, qxgy), nrow = 1, ncol = 3)
dimnames(c.sum) <- list(NULL, c("P(X<Y)", "P(X=Y)", "P(X>Y)"))
if (flag) {
  sigdih <- sum((m - d)^2) / (length(x) * length(y) - 1)
  di <- NA
  for (i in 1:length(x)) {
    di[i] <- sum(x[i] > y) / length(y) - sum(x[i] < y) / length(y)
  }
  dh <- NA
  for (i in 1:length(y)) {
    dh[i] <- sum(y[i] > x) / length(x) - sum(y[i] < x) / length(x)
  }
  sdi <- stats::var(di)
  sdh <- stats::var(dh)
  sh <- ((length(y) - 1) * sdi + (length(x) - 1) * sdh + sigdih) /
    (length(x) * length(y))
  zv <- stats::qnorm(alpha / 2)
  cu <- (d - d^3 - zv * sqrt(sh) * sqrt((1 - d^2)^2 + zv^2 * sh)) /
    (1 - d^2 + zv^2 * sh)
  cl <- (d - d^3 + zv * sqrt(sh) * sqrt((1 - d^2)^2 + zv^2 * sh)) /
    (1 - d^2 + zv^2 * sh)
}
if (!flag) {
  sh <- NULL
  nm <- max(c(length(x), length(y)))
  if (phat == 1) bci <- binomci(nm, nm, alpha = alpha)
  if (phat == 0) bci <- binomci(0, nm, alpha = alpha)
}
if (flag) pci <- c((1 - cu) / 2, (1 - cl) / 2)
if (!flag) {
  pci <- bci$ci
  cl <- 1 - 2 * pci[2]
  cu <- 1 - 2 * pci[1]
}
list(
  n1 = length(x), n2 = length(y), cl = cl, cu = cu, d = d,
  sqse.d = sh, phat = phat, summary.dvals = c.sum, ci.p = pci
)
}

#' Brunner-Munzel heteroscedastic analog of WMW test
#' @keywords internal
bmp <- function(x, y, alpha = .05, crit = NA, plotit = FALSE, pop = 0,
                fr = .8, xlab = "", ylab = "") {
x <- x[!is.na(x)]
y <- y[!is.na(y)]
n1 <- length(x)
n2 <- length(y)
N <- n1 + n2
n1p1 <- n1 + 1
flag1 <- c(1:n1)
flag2 <- c(n1p1:N)
R <- rank(c(x, y))
R1 <- mean(R[flag1])
R2 <- mean(R[flag2])
Rg1 <- rank(x)
Rg2 <- rank(y)
S1sq <- sum((R[flag1] - Rg1 - R1 + (n1 + 1) / 2)^2) / (n1 - 1)
S2sq <- sum((R[flag2] - Rg2 - R2 + (n2 + 1) / 2)^2) / (n2 - 1)
sig1 <- S1sq / n2^2
sig2 <- S2sq / n1^2
se <- sqrt(N) * sqrt(N * (sig1 / n1 + sig2 / n2))
bmtest <- (R2 - R1) / se
phat <- (R2 - (n2 + 1) / 2) / n1
flag <- TRUE
if (phat == 0 || phat == 1) flag <- FALSE
dhat <- 1 - 2 * phat
df <- (S1sq / n2 + S2sq / n1)^2 /
  ((S1sq / n2)^2 / (n1 - 1) + (S2sq / n1)^2 / (n2 - 1))
sig <- 2 * (1 - stats::pt(abs(bmtest), df))
if (is.na(crit)) vv <- stats::qt(alpha / 2, df)
if (!is.na(crit)) vv <- crit
ci.p <- c(phat + vv * se / N, phat - vv * se / N)
ci.p[1] <- max(0, ci.p[1])
ci.p[2] <- min(1, ci.p[2])
dval <- matrix(0, 1, 3)
for (i in 1:n1) {
  for (j in 1:n2) {
    id <- sign(x[i] - y[j]) + 2
    dval[1, id] <- dval[1, id] + 1
  }
}
dval <- dval / (n1 * n2)
dimnames(dval) <- list(NULL, c("P(X<Y)", "P(X=Y)", "P(X>Y)"))
if (!flag) {
  nm <- max(c(length(x), length(y)))
  if (phat == 1) A <- binomcipv(nm, nm, alpha = alpha)
  if (phat == 0) A <- binomcipv(0, nm, alpha = alpha)
  ci.p <- A$ci
  sig <- A$p.value
}
list(
  n1 = n1, n2 = n2, test.stat = bmtest, phat = phat, dhat = dhat,
  s.e. = se / N, p.value = sig, ci.p = ci.p, df = df, summary.dval = dval
)
}

#' p-value for Cliff's analog of WMW test
#' @keywords internal
cidv2 <- function(x, y, alpha = .05, plotit = FALSE, pop = 0, fr = .8,
                  rval = 15, xlab = "", ylab = "") {
if (length(x) * length(y) > 10^6) {
  stop("Use bmp with a large sample size.")
}
nullval <- 0
ci <- cid(x, y, alpha = alpha, plotit = plotit, pop = pop, fr = fr, rval = rval)
FLAG <- TRUE
if (ci$phat == 0 || ci$phat == 1) FLAG <- FALSE
if (FLAG) {
  alph <- c(1:99) / 100
  for (i in 1:99) {
    irem <- i
    chkit <- cid(x, y, alpha = alph[i], plotit = FALSE)
    if (chkit[[3]] > nullval || chkit[[4]] < nullval) break
  }
  p.value <- irem / 100
  if (p.value <= .01) {
    iup <- (irem + 1) / 100
    alph <- seq(.001, iup, .001)
    for (i in 1:length(alph)) {
      p.value <- alph[i]
      chkit <- cid(x, y, alpha = alph[i], plotit = FALSE, xlab = xlab, ylab = ylab)
      if (chkit[[3]] > nullval || chkit[[4]] < nullval) break
    }
  }
  if (p.value <= .001) {
    alph <- seq(.0001, .001, .0001)
    for (i in 1:length(alph)) {
      p.value <- alph[i]
      chkit <- cid(x, y, alpha = alph[i], plotit = FALSE)
      if (chkit[[3]] > nullval || chkit[[4]] < nullval) break
    }
  }
  phat <- (1 - ci$d) / 2
  pci <- c((1 - ci$cu) / 2, (1 - ci$cl) / 2)
  d.ci <- c(ci$cl, ci$cu)
  dval <- cid(x, y)$summary.dvals
}
if (!FLAG) {
  D <- bmp(x, y)
  p.value <- D$p.value
  d.ci <- NA
  pci <- D$ci.p
  phat <- D$phat
  dval <- ci$summary.dvals
}
list(
  n1 = length(elimna(x)), n2 = length(elimna(y)), d.hat = ci$d,
  d.ci = d.ci, p.value = p.value, p.hat = phat, p.ci = pci,
  summary.dvals = dval
)
}

#' Cliff's method for all pairs of J independent groups with labels
#'
#' Perform Cliff's method for all pairs of J independent groups.
#' The familywise type I error probability is controlled via
#' Hochberg's method.
#'
#' @param data Data frame containing the data
#' @param alpha Significance level (default 0.05)
#' @param gcode Column name for numeric group codes
#' @param glab Column name for character group labels
#' @param dp Column name for the dependent variable
#' @param CI.FWE Logical, use FWE-adjusted CIs (default FALSE)
#' @return List with n (group sizes), test (results), summary.dvals
#' @export
# MODIFIED: Derived from cidmulv2. Added gcode/glab/dp parameters so output
# preserves actual group labels instead of numeric group indices, making it
# clear which groups were compared.
cidmulv2_labelled <- function(data, alpha = .05, gcode, glab, dp,
                               CI.FWE = FALSE) {
codes <- data[[gcode]]
labs <- as.character(data[[glab]])
y <- data[[dp]]

if (is.null(codes) || is.null(labs) || is.null(y)) {
  stop("Must supply gcode, glab and dp column names.")
}

code_levels <- unique(codes)
f_codes <- factor(codes, levels = code_levels)
L_code <- split(y, f_codes)
code2lab <- sapply(code_levels, function(cc) labs[which(codes == cc)[1]])

x <- L_code
J <- length(x)
CC <- (J^2 - J) / 2

test <- data.frame(
  Group.A = character(CC),
  Group.B = character(CC),
  p.hat = numeric(CC),
  p.ci.lower = numeric(CC),
  p.ci.upper = numeric(CC),
  p.value = numeric(CC),
  p.crit = numeric(CC),
  stringsAsFactors = FALSE
)
csum <- data.frame(
  Group.A = character(CC),
  Group.B = character(CC),
  P.XltY = numeric(CC),
  P.XeqY = numeric(CC),
  P.XgtY = numeric(CC),
  stringsAsFactors = FALSE
)

for (j in seq_len(J)) x[[j]] <- stats::na.omit(x[[j]])

jcom <- 0
for (j in seq_len(J)) {
  for (k in seq_len(J)) {
    if (j < k) {
      jcom <- jcom + 1
      tmp <- cidv2(x[[j]], x[[k]], alpha, plotit = FALSE)
      sumd <- cid(x[[j]], x[[k]])$summary.dvals

      test$Group.A[jcom] <- code2lab[j]
      test$Group.B[jcom] <- code2lab[k]
      test$p.hat[jcom] <- tmp$p.hat
      test$p.ci.lower[jcom] <- tmp$p.ci[1]
      test$p.ci.upper[jcom] <- tmp$p.ci[2]
      test$p.value[jcom] <- tmp$p.value

      csum$Group.A[jcom] <- code2lab[j]
      csum$Group.B[jcom] <- code2lab[k]
      csum[jcom, 3:5] <- sumd
    }
  }
}

crits <- alpha / seq_len(CC)
o <- order(-test$p.value)
test$p.crit[o] <- crits

if (CI.FWE) {
  jcom <- 0
  for (j in seq_len(J)) {
    for (k in seq_len(J)) {
      if (j < k) {
        jcom <- jcom + 1
        tmp <- cidv2(x[[j]], x[[k]], alpha = test$p.crit[jcom], plotit = FALSE)
        test$p.ci.lower[jcom] <- tmp$p.ci[1]
        test$p.ci.upper[jcom] <- tmp$p.ci[2]
      }
    }
  }
}

n <- sapply(x, length)
names(n) <- code2lab

list(n = n, test = test, summary.dvals = csum)
}
