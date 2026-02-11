winvar <- function (x, tr = 0.2, na.rm = FALSE, STAND = NULL, ...) 
{
  if (na.rm) 
    x <- x[!is.na(x)]
  y <- sort(x)
  n <- length(x)
  ibot <- floor(tr * n) + 1
  itop <- length(x) - ibot + 1
  xbot <- y[ibot]
  xtop <- y[itop]
  y <- ifelse(y <= xbot, xbot, y)
  y <- ifelse(y >= xtop, xtop, y)
  winvar <- var(y)
  winvar
}

listm<-function(x)
{
  if(is.null(dim(x)))stop("The argument x must be a matrix or data frame")
  y<-list()
  for(j in 1:ncol(x))y[[j]]<-x[,j]
  y
}

smmcrit <- function (nuhat, C) 
{
  if (C - round(C) != 0) 
    stop("The number of contrasts, C, must be an  integer")
  if (C >= 29) 
    stop("C must be less than or equal to 28")
  if (C <= 0) 
    stop("C must be greater than or equal to 1")
  if (nuhat < 2) 
    stop("The degrees of freedom must be greater than or equal to 2")
  if (C == 1) 
    smmcrit <- qt(0.975, nuhat)
  if (C >= 2) {
    C <- C - 1
    m1 <- matrix(0, 20, 27)
    m1[1, ] <- c(5.57, 6.34, 6.89, 7.31, 7.65, 7.93, 8.17, 
                 8.83, 8.57, 8.74, 8.89, 9.03, 9.16, 9.28, 9.39, 9.49, 
                 9.59, 9.68, 9.77, 9.85, 9.92, 10, 10.07, 10.13, 10.2, 
                 10.26, 10.32)
    m1[2, ] <- c(3.96, 4.43, 4.76, 5.02, 5.23, 5.41, 5.56, 
                 5.69, 5.81, 5.92, 6.01, 6.1, 6.18, 6.26, 6.33, 6.39, 
                 6.45, 6.51, 6.57, 6.62, 6.67, 6.71, 6.76, 6.8, 6.84, 
                 6.88, 6.92)
    m1[3, ] <- c(3.38, 3.74, 4.01, 4.2, 4.37, 4.5, 4.62, 
                 4.72, 4.82, 4.89, 4.97, 5.04, 5.11, 5.17, 5.22, 5.27, 
                 5.32, 5.37, 5.41, 5.45, 5.49, 5.52, 5.56, 5.59, 5.63, 
                 5.66, 5.69)
    m1[4, ] <- c(3.09, 3.39, 3.62, 3.79, 3.93, 4.04, 4.14, 
                 4.23, 4.31, 4.38, 4.45, 4.51, 4.56, 4.61, 4.66, 4.7, 
                 4.74, 4.78, 4.82, 4.85, 4.89, 4.92, 4.95, 4.98, 5, 
                 5.03, 5.06)
    m1[5, ] <- c(2.92, 3.19, 3.39, 3.54, 3.66, 3.77, 3.86, 
                 3.94, 4.01, 4.07, 4.13, 4.18, 4.23, 4.28, 4.32, 4.36, 
                 4.39, 4.43, 4.46, 4.49, 4.52, 4.55, 4.58, 4.6, 4.63, 
                 4.65, 4.68)
    m1[6, ] <- c(2.8, 3.06, 3.24, 3.38, 3.49, 3.59, 3.67, 
                 3.74, 3.8, 3.86, 3.92, 3.96, 4.01, 4.05, 4.09, 4.13, 
                 4.16, 4.19, 4.22, 4.25, 4.28, 4.31, 4.33, 4.35, 4.38, 
                 4.39, 4.42)
    m1[7, ] <- c(2.72, 2.96, 3.13, 3.26, 3.36, 3.45, 3.53, 
                 3.6, 3.66, 3.71, 3.76, 3.81, 3.85, 3.89, 3.93, 3.96, 
                 3.99, 4.02, 4.05, 4.08, 4.1, 4.13, 4.15, 4.18, 4.19, 
                 4.22, 4.24)
    m1[8, ] <- c(2.66, 2.89, 3.05, 3.17, 3.27, 3.36, 3.43, 
                 3.49, 3.55, 3.6, 3.65, 3.69, 3.73, 3.77, 3.8, 3.84, 
                 3.87, 3.89, 3.92, 3.95, 3.97, 3.99, 4.02, 4.04, 4.06, 
                 4.08, 4.09)
    m1[9, ] <- c(2.61, 2.83, 2.98, 3.1, 3.19, 3.28, 3.35, 
                 3.41, 3.47, 3.52, 3.56, 3.6, 3.64, 3.68, 3.71, 3.74, 
                 3.77, 3.79, 3.82, 3.85, 3.87, 3.89, 3.91, 3.94, 3.95, 
                 3.97, 3.99)
    m1[10, ] <- c(2.57, 2.78, 2.93, 3.05, 3.14, 3.22, 3.29, 
                  3.35, 3.4, 3.45, 3.49, 3.53, 3.57, 3.6, 3.63, 3.66, 
                  3.69, 3.72, 3.74, 3.77, 3.79, 3.81, 3.83, 3.85, 3.87, 
                  3.89, 3.91)
    m1[11, ] <- c(2.54, 2.75, 2.89, 3.01, 3.09, 3.17, 3.24, 
                  3.29, 3.35, 3.39, 3.43, 3.47, 3.51, 3.54, 3.57, 3.6, 
                  3.63, 3.65, 3.68, 3.7, 3.72, 3.74, 3.76, 3.78, 3.8, 
                  3.82, 3.83)
    m1[12, ] <- c(2.49, 2.69, 2.83, 2.94, 3.02, 3.09, 3.16, 
                  3.21, 3.26, 3.3, 3.34, 3.38, 3.41, 3.45, 3.48, 3.5, 
                  3.53, 3.55, 3.58, 3.59, 3.62, 3.64, 3.66, 3.68, 3.69, 
                  3.71, 3.73)
    m1[13, ] <- c(2.46, 2.65, 2.78, 2.89, 2.97, 3.04, 3.09, 
                  3.15, 3.19, 3.24, 3.28, 3.31, 3.35, 3.38, 3.4, 3.43, 
                  3.46, 3.48, 3.5, 3.52, 3.54, 3.56, 3.58, 3.59, 3.61, 
                  3.63, 3.64)
    m1[14, ] <- c(2.43, 2.62, 2.75, 2.85, 2.93, 2.99, 3.05, 
                  3.11, 3.15, 3.19, 3.23, 3.26, 3.29, 3.32, 3.35, 3.38, 
                  3.4, 3.42, 3.44, 3.46, 3.48, 3.5, 3.52, 3.54, 3.55, 
                  3.57, 3.58)
    m1[15, ] <- c(2.41, 2.59, 2.72, 2.82, 2.89, 2.96, 3.02, 
                  3.07, 3.11, 3.15, 3.19, 3.22, 3.25, 3.28, 3.31, 3.33, 
                  3.36, 3.38, 3.39, 3.42, 3.44, 3.46, 3.47, 3.49, 3.5, 
                  3.52, 3.53)
    m1[16, ] <- c(2.38, 2.56, 2.68, 2.77, 2.85, 2.91, 2.97, 
                  3.02, 3.06, 3.09, 3.13, 3.16, 3.19, 3.22, 3.25, 3.27, 
                  3.29, 3.31, 3.33, 3.35, 3.37, 3.39, 3.4, 3.42, 3.43, 
                  3.45, 3.46)
    m1[17, ] <- c(2.35, 2.52, 2.64, 2.73, 2.8, 2.87, 2.92, 
                  2.96, 3.01, 3.04, 3.07, 3.11, 3.13, 3.16, 3.18, 3.21, 
                  3.23, 3.25, 3.27, 3.29, 3.3, 3.32, 3.33, 3.35, 3.36, 
                  3.37, 3.39)
    m1[18, ] <- c(2.32, 2.49, 2.6, 2.69, 2.76, 2.82, 2.87, 
                  2.91, 2.95, 2.99, 3.02, 3.05, 3.08, 3.09, 3.12, 3.14, 
                  3.17, 3.18, 3.2, 3.22, 3.24, 3.25, 3.27, 3.28, 3.29, 
                  3.31, 3.32)
    m1[19, ] <- c(2.29, 2.45, 2.56, 2.65, 2.72, 2.77, 2.82, 
                  2.86, 2.9, 2.93, 2.96, 2.99, 3.02, 3.04, 3.06, 3.08, 
                  3.1, 3.12, 3.14, 3.16, 3.17, 3.19, 3.2, 3.21, 3.23, 
                  3.24, 3.25)
    m1[20, ] <- c(2.24, 2.39, 2.49, 2.57, 2.63, 2.68, 2.73, 
                  2.77, 2.79, 2.83, 2.86, 2.88, 2.91, 2.93, 2.95, 2.97, 
                  2.98, 3.01, 3.02, 3.03, 3.04, 3.06, 3.07, 3.08, 3.09, 
                  3.11, 3.12)
    if (nuhat >= 200) 
      smmcrit <- m1[20, C]
    if (nuhat < 200) {
      nu <- c(2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 14, 16, 
              18, 20, 24, 30, 40, 60, 200)
      temp <- abs(nu - nuhat)
      find <- order(temp)
      if (temp[find[1]] == 0) 
        smmcrit <- m1[find[1], C]
      if (temp[find[1]] != 0) {
        if (nuhat > nu[find[1]]) {
          smmcrit <- m1[find[1], C] - (1/nu[find[1]] - 
                                         1/nuhat) * (m1[find[1], C] - m1[find[1] + 
                                                                           1, C])/(1/nu[find[1]] - 1/nu[find[1] + 1])
        }
        if (nuhat < nu[find[1]]) {
          smmcrit <- m1[find[1] - 1, C] - (1/nu[find[1] - 
                                                  1] - 1/nuhat) * (m1[find[1] - 1, C] - m1[find[1], 
                                                                                           C])/(1/nu[find[1] - 1] - 1/nu[find[1]])
        }
      }
    }
  }
  smmcrit
}

smmcrit01 <- function (nuhat, C) 
{
  if (C - round(C) != 0) 
    stop("The number of contrasts, C, must be an  integer")
  if (C >= 29) 
    stop("C must be less than or equal to 28")
  if (C <= 0) 
    stop("C must be greater than or equal to 1")
  if (nuhat < 2) 
    stop("The degrees of freedom must be greater than or equal to 2")
  if (C == 1) 
    smmcrit01 <- qt(0.995, nuhat)
  if (C >= 2) {
    C <- C - 1
    m1 <- matrix(0, 20, 27)
    m1[1, ] <- c(12.73, 14.44, 15.65, 16.59, 17.35, 17.99, 
                 18.53, 19.01, 19.43, 19.81, 20.15, 20.46, 20.75, 
                 20.99, 20.99, 20.99, 20.99, 20.99, 22.11, 22.29, 
                 22.46, 22.63, 22.78, 22.93, 23.08, 23.21, 23.35)
    m1[2, ] <- c(7.13, 7.91, 8.48, 8.92, 9.28, 9.58, 9.84, 
                 10.06, 10.27, 10.45, 10.61, 10.76, 10.9, 11.03, 11.15, 
                 11.26, 11.37, 11.47, 11.56, 11.65, 11.74, 11.82, 
                 11.89, 11.97, 12.07, 12.11, 12.17)
    m1[3, ] <- c(5.46, 5.99, 6.36, 6.66, 6.89, 7.09, 7.27, 
                 7.43, 7.57, 7.69, 7.8, 7.91, 8.01, 8.09, 8.17, 8.25, 
                 8.32, 8.39, 8.45, 8.51, 8.57, 8.63, 8.68, 8.73, 8.78, 
                 8.83, 8.87)
    m1[4, ] <- c(4.7, 5.11, 5.39, 5.63, 5.81, 5.97, 6.11, 
                 6.23, 6.33, 6.43, 6.52, 6.59, 6.67, 6.74, 6.81, 6.87, 
                 6.93, 6.98, 7.03, 7.08, 7.13, 7.17, 7.21, 7.25, 7.29, 
                 7.33, 7.36)
    m1[5, ] <- c(4.27, 4.61, 4.85, 5.05, 5.2, 5.33, 5.45, 
                 5.55, 5.64, 5.72, 5.79, 5.86, 5.93, 5.99, 6.04, 6.09, 
                 6.14, 6.18, 6.23, 6.27, 6.31, 6.34, 6.38, 6.41, 6.45, 
                 6.48, 6.51)
    m1[6, ] <- c(3.99, 4.29, 4.51, 4.68, 4.81, 4.93, 5.03, 
                 5.12, 5.19, 5.27, 5.33, 5.39, 5.45, 5.5, 5.55, 5.59, 
                 5.64, 5.68, 5.72, 5.75, 5.79, 5.82, 5.85, 5.88, 5.91, 
                 5.94, 5.96)
    m1[7, ] <- c(3.81, 4.08, 4.27, 4.42, 4.55, 4.65, 4.74, 
                 4.82, 4.89, 4.96, 5.02, 5.07, 5.12, 5.17, 5.21, 5.25, 
                 5.29, 5.33, 5.36, 5.39, 5.43, 5.45, 5.48, 5.51, 5.54, 
                 5.56, 5.59)
    m1[8, ] <- c(3.67, 3.92, 4.1, 4.24, 4.35, 4.45, 4.53, 
                 4.61, 4.67, 4.73, 4.79, 4.84, 4.88, 4.92, 4.96, 5.01, 
                 5.04, 5.07, 5.1, 5.13, 5.16, 5.19, 5.21, 5.24, 5.26, 
                 5.29, 5.31)
    m1[9, ] <- c(3.57, 3.8, 3.97, 4.09, 4.2, 4.29, 4.37, 
                 4.44, 4.5, 4.56, 4.61, 4.66, 4.69, 4.74, 4.78, 4.81, 
                 4.84, 4.88, 4.91, 4.93, 4.96, 4.99, 5.01, 5.03, 5.06, 
                 5.08, 5.09)
    m1[10, ] <- c(3.48, 3.71, 3.87, 3.99, 4.09, 4.17, 4.25, 
                  4.31, 4.37, 4.42, 4.47, 4.51, 4.55, 4.59, 4.63, 4.66, 
                  4.69, 4.72, 4.75, 4.78, 4.8, 4.83, 4.85, 4.87, 4.89, 
                  4.91, 4.93)
    m1[11, ] <- c(3.42, 3.63, 3.78, 3.89, 0.99, 4.08, 4.15, 
                  4.21, 4.26, 4.31, 4.36, 4.4, 4.44, 4.48, 4.51, 4.54, 
                  4.57, 4.59, 4.62, 4.65, 4.67, 4.69, 4.72, 4.74, 4.76, 
                  4.78, 4.79)
    m1[12, ] <- c(3.32, 3.52, 3.66, 3.77, 3.85, 3.93, 3.99, 
                  0.05, 4.1, 4.15, 4.19, 4.23, 4.26, 4.29, 4.33, 4.36, 
                  4.39, 4.41, 4.44, 4.46, 4.48, 4.5, 4.52, 4.54, 4.56, 
                  4.58, 4.59)
    m1[13, ] <- c(3.25, 3.43, 3.57, 3.67, 3.75, 3.82, 3.88, 
                  3.94, 3.99, 4.03, 4.07, 4.11, 4.14, 4.17, 4.19, 4.23, 
                  4.25, 4.28, 4.29, 4.32, 4.34, 4.36, 4.38, 4.39, 4.42, 
                  4.43, 4.45)
    m1[14, ] <- c(3.19, 3.37, 3.49, 3.59, 3.68, 3.74, 3.8, 
                  3.85, 3.89, 3.94, 3.98, 4.01, 4.04, 4.07, 4.1, 4.13, 
                  4.15, 4.18, 4.19, 4.22, 4.24, 4.26, 4.28, 4.29, 4.31, 
                  4.33, 4.34)
    m1[15, ] <- c(3.15, 3.32, 3.45, 3.54, 3.62, 3.68, 3.74, 
                  3.79, 3.83, 3.87, 3.91, 3.94, 3.97, 3.99, 4.03, 4.05, 
                  4.07, 4.09, 4.12, 4.14, 4.16, 4.17, 4.19, 4.21, 4.22, 
                  4.24, 4.25)
    m1[16, ] <- c(3.09, 3.25, 3.37, 3.46, 3.53, 3.59, 3.64, 
                  3.69, 3.73, 3.77, 3.8, 3.83, 3.86, 3.89, 3.91, 3.94, 
                  3.96, 3.98, 4, 4.02, 4.04, 4.05, 4.07, 4.09, 4.1, 
                  4.12, 4.13)
    m1[17, ] <- c(3.03, 3.18, 3.29, 3.38, 3.45, 3.5, 3.55, 
                  3.59, 3.64, 3.67, 3.7, 3.73, 3.76, 3.78, 3.81, 3.83, 
                  3.85, 3.87, 3.89, 3.91, 3.92, 3.94, 3.95, 3.97, 3.98, 
                  4, 4.01)
    m1[18, ] <- c(2.97, 3.12, 3.22, 3.3, 3.37, 3.42, 3.47, 
                  3.51, 3.55, 3.58, 3.61, 3.64, 3.66, 3.68, 3.71, 3.73, 
                  3.75, 3.76, 3.78, 3.8, 3.81, 3.83, 3.84, 3.85, 3.87, 
                  3.88, 3.89)
    m1[19, ] <- c(2.91, 3.06, 3.15, 3.23, 3.29, 3.34, 3.38, 
                  3.42, 3.46, 3.49, 3.51, 3.54, 3.56, 3.59, 3.61, 3.63, 
                  3.64, 3.66, 3.68, 3.69, 3.71, 3.72, 3.73, 3.75, 3.76, 
                  3.77, 3.78)
    m1[20, ] <- c(2.81, 2.93, 3.02, 3.09, 3.14, 3.19, 3.23, 
                  3.26, 3.29, 3.32, 3.34, 3.36, 3.38, 3.4, 0.42, 0.44, 
                  3.45, 3.47, 3.48, 3.49, 3.5, 3.52, 3.53, 3.54, 3.55, 
                  3.56, 3.57)
    if (nuhat >= 200) 
      smmcrit01 <- m1[20, C]
    if (nuhat < 200) {
      nu <- c(2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 14, 16, 
              18, 20, 24, 30, 40, 60, 200)
      temp <- abs(nu - nuhat)
      find <- order(temp)
      if (temp[find[1]] == 0) 
        smmcrit01 <- m1[find[1], C]
      if (temp[find[1]] != 0) {
        if (nuhat > nu[find[1]]) {
          smmcrit01 <- m1[find[1], C] - (1/nu[find[1]] - 
                                           1/nuhat) * (m1[find[1], C] - m1[find[1] + 
                                                                             1, C])/(1/nu[find[1]] - 1/nu[find[1] + 1])
        }
        if (nuhat < nu[find[1]]) {
          smmcrit01 <- m1[find[1] - 1, C] - (1/nu[find[1] - 
                                                    1] - 1/nuhat) * (m1[find[1] - 1, C] - m1[find[1], 
                                                                                             C])/(1/nu[find[1] - 1] - 1/nu[find[1]])
        }
      }
    }
  }
  smmcrit01
}

smmvalv2 <- function (dfvec, iter = 10000, alpha = 0.05, SEED = TRUE) 
{
  if (SEED) 
    set.seed(1)
  dfv <- length(dfvec)/sum(1/dfvec)
  vals <- NA
  tvals <- NA
  J <- length(dfvec)
  z = matrix(nrow = iter, ncol = J)
  for (j in 1:J) z[, j] = rt(iter, dfvec[j])
  vals = apply(z, 1, max)
  vals <- sort(vals)
  ival <- round((1 - alpha) * iter)
  qval <- vals[ival]
  qval
}

fac2list <- function (x, g) 
{
  g = as.data.frame(g)
  L = ncol(g)
  g = listm(g)
  for (j in 1:L) g[[j]] = as.factor(g[[j]])
  g = matl(g)
  Lp1 = L + 1
  if (L > 4) 
    stop("Can have at most 4 factors")
  if (L == 1) {
    res = selby(cbind(x, g), 2, 1)
    group.id = res$grpn
    res = res$x
  }
  if (L > 1) {
    res = selby2(cbind(x, g), c(2:Lp1), 1)
    group.id = res$grpn
    res = res$x
  }
  res
}

lincon1 <- function (x, con = 0, tr = 0.2, alpha = 0.05, pr = TRUE, crit = NA, 
                     SEED = TRUE, KB = FALSE) 
{
  if (tr == 0.5) 
    stop("Use the R function medpb to compare medians")
  if (is.data.frame(x)) 
    x = as.matrix(x)
  if (KB) 
    stop("Use the function kbcon")
  flag <- T
  if (alpha != 0.05 && alpha != 0.01) 
    flag <- F
  if (is.matrix(x)) 
    x <- listm(x)
  if (!is.list(x)) 
    stop("Data must be stored in a matrix or in list mode.")
  con <- as.matrix(con)
  J <- length(x)
  sam = NA
  h <- vector("numeric", J)
  w <- vector("numeric", J)
  xbar <- vector("numeric", J)
  for (j in 1:J) {
    xx <- !is.na(x[[j]])
    val <- x[[j]]
    x[[j]] <- val[xx]
    sam[j] = length(x[[j]])
    h[j] <- length(x[[j]]) - 2 * floor(tr * length(x[[j]]))
    w[j] <- ((length(x[[j]]) - 1) * winvar(x[[j]], tr))/(h[j] * 
                                                           (h[j] - 1))
    xbar[j] <- mean(x[[j]], tr)
  }
  if (sum(con^2) == 0) {
    CC <- (J^2 - J)/2
    psihat <- matrix(0, CC, 6)
    dimnames(psihat) <- list(NULL, c("Group", "Group", "psihat", 
                                     "ci.lower", "ci.upper", "p.value"))
    test <- matrix(NA, CC, 6)
    dimnames(test) <- list(NULL, c("Group", "Group", "test", 
                                   "crit", "se", "df"))
    jcom <- 0
    for (j in 1:J) {
      for (k in 1:J) {
        if (j < k) {
          jcom <- jcom + 1
          test[jcom, 3] <- abs(xbar[j] - xbar[k])/sqrt(w[j] + 
                                                         w[k])
          sejk <- sqrt(w[j] + w[k])
          test[jcom, 5] <- sejk
          psihat[jcom, 1] <- j
          psihat[jcom, 2] <- k
          test[jcom, 1] <- j
          test[jcom, 2] <- k
          psihat[jcom, 3] <- (xbar[j] - xbar[k])
          df <- (w[j] + w[k])^2/(w[j]^2/(h[j] - 1) + 
                                   w[k]^2/(h[k] - 1))
          test[jcom, 6] <- df
          psihat[jcom, 6] <- 2 * (1 - pt(test[jcom, 3], 
                                         df))
          if (!KB) {
            if (CC > 28) 
              flag = F
            if (flag) {
              if (alpha == 0.05) 
                crit <- smmcrit(df, CC)
              if (alpha == 0.01) 
                crit <- smmcrit01(df, CC)
            }
            if (!flag || CC > 28) 
              crit <- smmvalv2(dfvec = rep(df, CC), alpha = alpha, 
                               SEED = SEED)
          }
          if (KB) 
            crit <- sqrt((J - 1) * (1 + (J - 2)/df) * 
                           qf(1 - alpha, J - 1, df))
          test[jcom, 4] <- crit
          psihat[jcom, 4] <- (xbar[j] - xbar[k]) - crit * 
            sejk
          psihat[jcom, 5] <- (xbar[j] - xbar[k]) + crit * 
            sejk
        }
      }
    }
  }
  if (sum(con^2) > 0) {
    if (nrow(con) != length(x)) {
      stop("The number of groups does not match the number of contrast coefficients.")
    }
    psihat <- matrix(0, ncol(con), 5)
    dimnames(psihat) <- list(NULL, c("con.num", "psihat", 
                                     "ci.lower", "ci.upper", "p.value"))
    test <- matrix(0, ncol(con), 5)
    dimnames(test) <- list(NULL, c("con.num", "test", "crit", 
                                   "se", "df"))
    df <- 0
    for (d in 1:ncol(con)) {
      psihat[d, 1] <- d
      psihat[d, 2] <- sum(con[, d] * xbar)
      sejk <- sqrt(sum(con[, d]^2 * w))
      test[d, 1] <- d
      test[d, 2] <- sum(con[, d] * xbar)/sejk
      df <- (sum(con[, d]^2 * w))^2/sum(con[, d]^4 * w^2/(h - 
                                                            1))
      if (flag) {
        if (alpha == 0.05) 
          crit <- smmcrit(df, ncol(con))
        if (alpha == 0.01) 
          crit <- smmcrit01(df, ncol(con))
      }
      if (!flag) 
        crit <- smmvalv2(dfvec = rep(df, ncol(con)), 
                         alpha = alpha, SEED = SEED)
      test[d, 3] <- crit
      test[d, 4] <- sejk
      test[d, 5] <- df
      psihat[d, 3] <- psihat[d, 2] - crit * sejk
      psihat[d, 4] <- psihat[d, 2] + crit * sejk
      psihat[d, 5] <- 2 * (1 - pt(abs(test[d, 2]), df))
    }
  }
  list(n = sam, test = test, psihat = psihat)
}

con2way <- function (J, K) 
{
  JK <- J * K
  Ja <- (J^2 - J)/2
  Ka <- (K^2 - K)/2
  JK <- J * K
  conA <- matrix(0, nrow = JK, ncol = Ja)
  ic <- 0
  for (j in 1:J) {
    for (jj in 1:J) {
      if (j < jj) {
        ic <- ic + 1
        mat <- matrix(0, nrow = J, ncol = K)
        mat[j, ] <- 1
        mat[jj, ] <- 0 - 1
        conA[, ic] <- t(mat)
      }
    }
  }
  conB <- matrix(0, nrow = JK, ncol = Ka)
  ic <- 0
  for (k in 1:K) {
    for (kk in 1:K) {
      if (k < kk) {
        ic <- ic + 1
        mat <- matrix(0, nrow = J, ncol = K)
        mat[, k] <- 1
        mat[, kk] <- 0 - 1
        conB[, ic] <- t(mat)
      }
    }
  }
  conAB <- matrix(0, nrow = JK, ncol = Ka * Ja)
  ic <- 0
  for (j in 1:J) {
    for (jj in 1:J) {
      if (j < jj) {
        for (k in 1:K) {
          for (kk in 1:K) {
            if (k < kk) {
              ic <- ic + 1
              mat <- matrix(0, nrow = J, ncol = K)
              mat[j, k] <- 1
              mat[j, kk] <- 0 - 1
              mat[jj, k] <- 0 - 1
              mat[jj, kk] <- 1
            }
            conAB[, ic] <- t(mat)
          }
        }
      }
    }
  }
  list(conA = conA, conB = conB, conAB = conAB)
}

selby <- function (m, grpc, coln) 
{
  if (is.null(dim(m))) 
    stop("Data must be stored in a matrix or data frame")
  if (is.na(grpc[1])) 
    stop("The argument grpc is not specified")
  if (is.na(coln[1])) 
    stop("The argument coln is not specified")
  if (length(grpc) != 1) 
    stop("The argument grpc must have length 1")
  x <- vector("list")
  grpn <- sort(unique(m[, grpc]))
  it <- 0
  for (ig in 1:length(grpn)) {
    for (ic in 1:length(coln)) {
      it <- it + 1
      flag <- (m[, grpc] == grpn[ig])
      x[[it]] <- m[flag, coln[ic]]
    }
  }
  list(x = x, grpn = grpn)
}

selby2 <- function (m, grpc, coln = NA) 
{
  if (is.na(coln)) 
    stop("The argument coln is not specified")
  if (length(grpc) > 4) 
    stop("The argument grpc must have length less than or equal to 4")
  x <- vector("list")
  ic <- 0
  if (length(grpc) == 2) {
    cat1 <- selby(m, grpc[1], coln)$grpn
    cat2 <- selby(m, grpc[2], coln)$grpn
    for (i1 in 1:length(cat1)) {
      for (i2 in 1:length(cat2)) {
        temp <- NA
        it <- 0
        for (i in 1:nrow(m)) {
          if (sum(m[i, c(grpc[1], grpc[2])] == c(cat1[i1], 
                                                 cat2[i2])) == 2) {
            it <- it + 1
            temp[it] <- m[i, coln]
          }
        }
        if (!is.na(temp[1])) {
          ic <- ic + 1
          x[[ic]] <- temp
          if (ic == 1) 
            grpn <- matrix(c(cat1[i1], cat2[i2]), 1, 
                           2)
          if (ic > 1) 
            grpn <- rbind(grpn, c(cat1[i1], cat2[i2]))
        }
      }
    }
  }
  if (length(grpc) == 3) {
    cat1 <- selby(m, grpc[1], coln)$grpn
    cat2 <- selby(m, grpc[2], coln)$grpn
    cat3 <- selby(m, grpc[3], coln)$grpn
    x <- vector("list")
    ic <- 0
    for (i1 in 1:length(cat1)) {
      for (i2 in 1:length(cat2)) {
        for (i3 in 1:length(cat3)) {
          temp <- NA
          it <- 0
          for (i in 1:nrow(m)) {
            if (sum(m[i, c(grpc[1], grpc[2], grpc[3])] == 
                    c(cat1[i1], cat2[i2], cat3[i3])) == 3) {
              it <- it + 1
              temp[it] <- m[i, coln]
            }
          }
          if (!is.na(temp[1])) {
            ic <- ic + 1
            x[[ic]] <- temp
            if (ic == 1) 
              grpn <- matrix(c(cat1[i1], cat2[i2], cat3[i3]), 
                             1, 3)
            if (ic > 1) 
              grpn <- rbind(grpn, c(cat1[i1], cat2[i2], 
                                    cat3[i3]))
          }
        }
      }
    }
  }
  if (length(grpc) == 4) {
    cat1 <- selby(m, grpc[1], coln)$grpn
    cat2 <- selby(m, grpc[2], coln)$grpn
    cat3 <- selby(m, grpc[3], coln)$grpn
    cat4 <- selby(m, grpc[4], coln)$grpn
    x <- vector("list")
    ic <- 0
    for (i1 in 1:length(cat1)) {
      for (i2 in 1:length(cat2)) {
        for (i3 in 1:length(cat3)) {
          for (i4 in 1:length(cat4)) {
            temp <- NA
            it <- 0
            for (i in 1:nrow(m)) {
              if (sum(m[i, c(grpc[1], grpc[2], grpc[3], 
                             grpc[4])] == c(cat1[i1], cat2[i2], cat3[i3], 
                                            cat4[i4])) == 4) {
                it <- it + 1
                temp[it] <- m[i, coln]
              }
            }
            if (!is.na(temp[1])) {
              ic <- ic + 1
              x[[ic]] <- temp
              if (ic == 1) 
                grpn <- matrix(c(cat1[i1], cat2[i2], 
                                 cat3[i3], cat4[i4]), 1, 4)
              if (ic > 1) 
                grpn <- rbind(grpn, c(cat1[i1], cat2[i2], 
                                      cat3[i3], cat4[i4]))
            }
          }
        }
      }
    }
  }
  list(x = x, grpn = grpn)
}

matl <- function (x) 
{
  J = length(x)
  nval = NA
  for (j in 1:J) nval[j] = length(x[[j]])
  temp <- matrix(NA, ncol = J, nrow = max(nval))
  for (j in 1:J) temp[1:nval[j], j] <- x[[j]]
  temp
}

mcp2atm<-function (formula, data, tr = 0.2, ...) 
{
  if (missing(data)) {
    mf <- model.frame(formula)
  }
  else {
    mf <- model.frame(formula, data)
  }
  cl <- match.call()
  J <- nlevels(mf[, 2])
  K <- nlevels(mf[, 3])
  alpha = 0.05
  grp = NA
  op = F
  JK <- J * K
  nfac <- tapply(mf[, 1], list(mf[, 2], mf[, 3]), length, simplify = FALSE)
  nfac1 <- nfac[unique(mf[, 2]), unique(mf[, 3])]
  data <- na.omit(data[variable.names(mf)])
  data <- data[order(mf[, 2], mf[, 3]), ]
  data$row <- unlist(alply(nfac1, 1, sequence), use.names = FALSE)
  dataMelt <- melt(data, id = c("row", colnames(mf)[2], colnames(mf)[3]), 
                   measured = mf[, 1])
  dataWide <- cast(dataMelt, as.formula(paste(colnames(dataMelt)[1], 
                                              "~", colnames(mf)[2], "+", colnames(mf)[3])), fun.aggregate = mean)
  dataWide$row <- NULL
  x <- fac2list(mf[, 1], mf[, 2:3])
  if (!is.na(grp[1])) {
    yy <- x
    x <- list()
    for (j in 1:length(grp)) x[[j]] <- yy[[grp[j]]]
  }
  for (j in 1:JK) {
    xx <- x[[j]]
    x[[j]] <- xx[!is.na(xx)]
  }
  for (j in 1:JK) {
    temp <- x[[j]]
    temp <- temp[!is.na(temp)]
    x[[j]] <- temp
  }
  temp <- con2way(J, K)
  conA <- temp$conA
  conB <- temp$conB
  conAB <- temp$conAB
  if (!op) {
    Factor.A <- lincon1(x, con = conA, tr = tr, alpha = alpha)
    Factor.B <- lincon1(x, con = conB, tr = tr, alpha = alpha)
    Factor.AB <- lincon1(x, con = conAB, tr = tr, alpha = alpha)
  }
  All.Tests <- NA
  if (op) {
    Factor.A <- NA
    Factor.B <- NA
    Factor.AB <- NA
    con <- cbind(conA, conB, conAB)
    All.Tests <- lincon1(x, con = con, tr = tr, alpha = alpha)
  }
  cnamesA <- colnames(mf)[2]
  dnamesA <- paste0(cnamesA, 1:ncol(conA))
  cnamesB <- colnames(mf)[3]
  dnamesB <- paste0(cnamesB, 1:ncol(conB))
  colnames(conB) <- dnamesB
  dnamesAB <- apply(expand.grid(dnamesA, dnamesB), 1, function(ss) paste(ss[1], 
                                                                         ss[2], sep = ":"))
  contrasts <- as.data.frame(cbind(conA, conB, conAB))
  colnames(contrasts) <- c(dnamesA, dnamesB, dnamesAB)
  rownames(contrasts) <- colnames(dataWide)
  outA <- list(psihat = Factor.A[[3]][, "psihat"], conf.int = Factor.A[[3]][, 
                                                                            c("ci.lower", "ci.upper")], p.value = Factor.A[[3]][, 
                                                                                                                                "p.value"])
  outB <- list(psihat = Factor.B[[3]][, "psihat"], conf.int = Factor.B[[3]][, 
                                                                            c("ci.lower", "ci.upper")], p.value = Factor.B[[3]][, 
                                                                                                                                "p.value"])
  outAB <- list(psihat = Factor.AB[[3]][, "psihat"], conf.int = Factor.AB[[3]][, 
                                                                               c("ci.lower", "ci.upper")], p.value = Factor.AB[[3]][, 
                                                                                                                                    "p.value"])
  effects <- list(outA, outB, outAB)
  names(effects) <- c(colnames(mf)[2:3], paste(colnames(mf)[2], 
                                               colnames(mf)[3], sep = ":"))
  result <- list(effects = effects, contrasts = contrasts, 
                 call = cl)
  class(result) <- "mcp"
  result
}

mcp2atm_TM <- function (formula, data, tr = 0.2, ...) 
{
  if (missing(data)) {
    mf <- model.frame(formula)
  }
  else {
    mf <- model.frame(formula, data)
  }
  cl <- match.call()
  J <- nlevels(mf[, 2])
  K <- nlevels(mf[, 3])
  alpha = 0.05
  grp = NA
  op = F
  JK <- J * K
  nfac <- tapply(mf[, 1], list(mf[, 2], mf[, 3]), length, simplify = FALSE)
  nfac1 <- nfac[unique(mf[, 2]), unique(mf[, 3])]
  data <- na.omit(data[variable.names(mf)])
  data <- data[order(mf[, 2], mf[, 3]), ]
  data$row <- unlist(alply(nfac1, 1, sequence), use.names = FALSE)
  dataMelt <- melt(data, id = c("row", colnames(mf)[2], colnames(mf)[3]), 
                   measured = mf[, 1])
  dataWide <- cast(dataMelt, as.formula(paste(colnames(dataMelt)[1], 
                                              "~", colnames(mf)[2], "+", colnames(mf)[3])), fun.aggregate = mean)
  dataWide$row <- NULL
  x <- fac2list(mf[, 1], mf[, 2:3])
  if (!is.na(grp[1])) {
    yy <- x
    x <- list()
    for (j in 1:length(grp)) x[[j]] <- yy[[grp[j]]]
  }
  for (j in 1:JK) {
    xx <- x[[j]]
    x[[j]] <- xx[!is.na(xx)]
  }
  for (j in 1:JK) {
    temp <- x[[j]]
    temp <- temp[!is.na(temp)]
    x[[j]] <- temp
  }
  temp <- con2way(J, K)
  
  conA <- temp$conA
  conB <- temp$conB
  conAB <- temp$conAB
  
  if (!op) {
    Factor.A <- lincon1(x, con = conA, tr = tr, alpha = alpha)
    Factor.B <- lincon1(x, con = conB, tr = tr, alpha = alpha)
    Factor.AB <- lincon1(x, con = conAB, tr = tr, alpha = alpha)
  }
  All.Tests <- NA
  if (op) {
    Factor.A <- NA
    Factor.B <- NA
    Factor.AB <- NA
    con <- cbind(conA, conB, conAB)
    All.Tests <- lincon1(x, con = con, tr = tr, alpha = alpha)
  }
  
  dnamesA_X <- levels(mf[, 2])[1:ncol(conA)]
  dnamesB_X <- levels(mf[, 3])[1:ncol(conB)]

  conB_X <- conB
  dnamesAB_X <- apply(expand.grid(dnamesA_X, dnamesB_X), 1, function(ss) paste(ss[1], 
                                                                         ss[2], sep = ":"))
  contrasts <- as.data.frame(cbind(conA, conB, conAB))
  colnames(contrasts) <- c(dnamesA_X, dnamesB_X, dnamesAB_X)
  rownames(contrasts) <- colnames(dataWide)
  
  outA <- list(psihat = Factor.A[[3]][, "psihat"], conf.int = Factor.A[[3]][, 
                                                                            c("ci.lower", "ci.upper")], p.value = Factor.A[[3]][, 
                                                                                                                                "p.value"])
  outB <- list(psihat = Factor.B[[3]][, "psihat"], conf.int = Factor.B[[3]][, 
                                                                            c("ci.lower", "ci.upper")], p.value = Factor.B[[3]][, 
                                                                                                                                "p.value"])
  outAB <- list(psihat = Factor.AB[[3]][, "psihat"], conf.int = Factor.AB[[3]][, 
                                                                               c("ci.lower", "ci.upper")], p.value = Factor.AB[[3]][, 
                                                                                                                                    "p.value"])
  effects <- list(outA, outB, outAB)
  
  names(effects) <- c(colnames(mf)[2:3], paste(colnames(mf)[2], 
                                               colnames(mf)[3], sep = ":"))
  result <- list(effects = effects, contrasts = contrasts, 
                 call = cl)
  class(result) <- "mcp"
  result
}