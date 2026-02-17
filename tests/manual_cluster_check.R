# Manual clustering sanity check
# Change the settings below and run interactively to inspect results.

library(readxl)
library(cluster)
library(dbscan)

# ── Settings (change these) ──────────────────────────────────────────────────
file_path <- "data/test/PCA_TestData_group_size.xlsx"
measure_cols <- c("value1", "value2", "value3", "value4",
                  "value5", "value6", "value7", "value8")
meta_cols <- c("group", "size")

algorithm <- "kmeans"        # "kmeans", "hierarchical", "dbscan"
n_clusters <- 2
metric <- "euclidean"        # "euclidean", "manhattan"
hc_method <- "ward.D2"       # ward.D2, single, complete, average, mcquitty, median, centroid
scale_center <- TRUE
scale_sd <- TRUE
dbscan_eps <- 0.5
dbscan_minPts <- 5

# ── Load & clean ─────────────────────────────────────────────────────────────
raw <- as.data.frame(read_excel(file_path))
cat("Raw:", nrow(raw), "rows,", ncol(raw), "cols\n")

# Drop rows with NA in measure cols
complete <- complete.cases(raw[, measure_cols])
cleaned <- raw[complete, ]
cat("After NA removal:", nrow(cleaned), "rows\n")

# ── Scale ────────────────────────────────────────────────────────────────────
num_data <- as.matrix(cleaned[, measure_cols])
if (scale_center || scale_sd) {
  num_data <- scale(num_data, center = scale_center, scale = scale_sd)
}
cat("Scaled:", nrow(num_data), "x", ncol(num_data), "\n\n")

# ── Cluster ──────────────────────────────────────────────────────────────────
if (algorithm == "kmeans") {
  d <- if (metric == "manhattan") dist(num_data, method = "manhattan") else NULL
  if (is.null(d)) {
    fit <- kmeans(num_data, centers = n_clusters, nstart = 25)
  } else {
    fit <- pam(num_data, k = n_clusters, metric = "manhattan")
  }
  cat("── K-Means / PAM ──\n")
  print(str(fit))
  clusters <- if (inherits(fit, "pam")) fit$clustering else fit$cluster

} else if (algorithm == "hierarchical") {
  d <- dist(num_data, method = metric)
  hc <- hclust(d, method = hc_method)
  clusters <- cutree(hc, k = n_clusters)
  cat("── Hierarchical (", hc_method, ") ──\n")
  print(str(hc))

} else if (algorithm == "dbscan") {
  d <- dist(num_data, method = metric)
  db <- dbscan::dbscan(d, eps = dbscan_eps, minPts = dbscan_minPts)
  clusters <- db$cluster
  cat("── DBSCAN ──\n")
  print(str(db))
}

cat("\nCluster assignments:\n")
print(table(clusters))

# ── Profile: raw-data means per cluster ──────────────────────────────────────
raw_numeric <- as.matrix(cleaned[, measure_cols])
valid <- clusters > 0
for (k in sort(unique(clusters[valid]))) {
  cat(sprintf("\nCluster %d (n=%d) — raw means:\n", k, sum(clusters == k)))
  print(round(colMeans(raw_numeric[clusters == k, , drop = FALSE]), 3))
}
cat("\nOverall raw means:\n")
print(round(colMeans(raw_numeric[valid, , drop = FALSE]), 3))

# ── Membership preview ───────────────────────────────────────────────────────
membership <- cleaned[, c(meta_cols, measure_cols)]
membership$Cluster <- clusters
cat("\nMembership (first 10 rows):\n")
print(head(membership, 10))
