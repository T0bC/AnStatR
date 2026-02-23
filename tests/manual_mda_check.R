# Manual MDA check — run this interactively in the R console
# from the project root (c:/Users/meissnerto/Desktop/TexAn2.0)
#
# Usage:  source("tests/manual_mda_check.R")

box::purge_cache()
box::use(app/logic/lda/lda)

# --- 1. Build test data (same as test-lda.R) ---
set.seed(123)
data <- data.frame(
  species = rep(c("A", "B", "C"), each = 15),
  site = rep(c("X", "Y", "Z"), 15),
  m1 = c(rnorm(15, mean = 0), rnorm(15, mean = 3), rnorm(15, mean = 6)),
  m2 = c(rnorm(15, mean = 0), rnorm(15, mean = 2), rnorm(15, mean = 4)),
  m3 = rnorm(45),
  stringsAsFactors = FALSE
)

# --- 2. Try run_mda and inspect the error ---
cat("=== run_mda() ===\n")
res <- lda$run_mda(data, c("m1", "m2", "m3"), "species")
cat("success:", res$success, "\n")
if (!res$success) {
  cat("ERROR message:", res$error$message, "\n")
  cat("ERROR class:", class(res$error), "\n")
  # Print full error object
  print(res$error)
}
if (res$success) {
  cat("analysis_type:", res$result$analysis_type, "\n")
  cat("n:", res$result$n, "\n")
  cat("n_groups:", res$result$n_groups, "\n")
  cat("scores dim:", dim(res$result$scores), "\n")
  cat("posterior dim:", dim(res$result$posterior), "\n")
  cat("confusion accuracy:", res$result$confusion$accuracy, "\n")
  cat("proportion_of_trace:\n")
  print(res$result$proportion_of_trace)
  cat("sub_prior:\n")
  print(res$result$sub_prior)
  cat("scaling:\n")
  print(res$result$scaling)
}

# --- 3. Try raw mda::mda() directly to isolate the issue ---
cat("\n=== raw mda::mda() ===\n")
tryCatch({
  grouping <- as.factor(data$species)
  numeric_data <- data[, c("m1", "m2", "m3"), drop = FALSE]
  fit_data <- cbind(numeric_data, .grouping. = grouping)
  mda_obj <- mda::mda(.grouping. ~ ., data = fit_data, subclasses = 3, iter = 5)
  cat("mda fit succeeded!\n")
  cat("class:", class(mda_obj), "\n")
  cat("dimension:", mda_obj$dimension, "\n")
  cat("percent.explained:", mda_obj$percent.explained, "\n")
  cat("sub.prior:", mda_obj$sub.prior, "\n")
  cat("deviance:", mda_obj$deviance, "\n")

  # predict types
  pred_class <- predict(mda_obj, numeric_data)
  cat("predict (class) head:", head(as.character(pred_class)), "\n")

  pred_post <- predict(mda_obj, numeric_data, type = "posterior")
  cat("predict (posterior) dim:", dim(pred_post), "\n")

  pred_var <- predict(mda_obj, numeric_data, type = "variates")
  cat("predict (variates) dim:", dim(pred_var), "\n")
  cat("predict (variates) is NULL:", is.null(pred_var), "\n")

  # coef
  coefs <- coef(mda_obj)
  cat("coef class:", class(coefs), "\n")
  cat("coef dim:", dim(coefs), "\n")

  # means
  cat("means class:", class(mda_obj$means), "\n")
  cat("means dim:", dim(mda_obj$means), "\n")
}, error = function(e) {
  cat("RAW mda::mda() FAILED:", conditionMessage(e), "\n")
})
