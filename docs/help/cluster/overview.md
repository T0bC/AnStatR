#### Cluster Analysis

Unsupervised method that partitions observations into groups based on similarity in their measurement variables — without requiring predefined group labels. The module supports raw data, PCA scores, and LDA scores as input.

##### Data Selection

Configure columns in the **Data Selection** sidebar tab:

- **Descriptive (metadata) columns** — Select columns such as `SAMPLE_ID`, `SPECIES`, `SITE`, or `TOOTH_TYPE` that identify your specimens. These columns are **not** used in the clustering computation but are preserved in the membership table, carried into the heatmap row annotations, and included in the Excel download. Selecting the right metadata columns is essential for interpreting which specimens ended up in which cluster.
- **Measurement columns** — Numeric variables included in the clustering. Click **Select all** to include all detected measurement columns. Rows with missing values in any selected measurement column are automatically excluded. Do **not** include categorical or text columns in the measurement selection — only pure numeric measurements belong there.

If the **Statistics** tab has computed a parameter screening ranking (see the Plotting tab's parameter screening mode) and the data source is **raw measurements**, an **Apply recommended parameters** banner appears above the measurement-column selector. Clicking it replaces the current selection with the parameters the Statistics tab identified as good group separators. The banner is not shown when clustering on PCA or LDA scores, since recommendations are raw measurement column names and do not map onto score dimensions.

**Data source alternatives** — Instead of raw measurements, you can cluster on:

- **PCA Scores** — the individual coordinate scores from a prior PCA run (`Dim.1`, `Dim.2`, …). Select enough dimensions to cover ≥ 90% cumulative variance. Scaling is skipped automatically (PCA scores are already mean-centred)
- **LDA Scores** — the discriminant axis scores from a prior LDA run (model-fitting mode, not LOO-CV). Only available after running LDA (not QDA) in the LDA tab

##### Clustering Settings

Configure algorithm and distance parameters in the **Clustering Settings** tab:

| Algorithm | Distance Metrics | When to Use |
|-----------|-----------------|-------------|
| **K-Means** (euclidean) | Euclidean | Default; fast; assumes roughly spherical, equal-sized clusters |
| **K-Means (PAM)** (manhattan) | Manhattan | More robust to outliers than K-Means; non-Gaussian cluster shapes |
| **Hierarchical** | Euclidean or Manhattan | Reveals nested structure; choose linkage method (Ward's D2 recommended) |
| **DBSCAN** | Euclidean or Manhattan | Arbitrary shapes; identifies noise points; k ignored (eps auto-computed) |

The **Number of clusters (k)** is auto-set to the median of three internal methods (Elbow/WSS, Silhouette, Gap statistic) on first run. Override manually at any time. DBSCAN ignores k entirely.

##### Scaling and Normalisation

**Data Scaling** (raw data only; skipped for PCA/LDA scores):

- **Scale & Center (recommended)** — z-score standardisation (mean = 0, SD = 1). Ensures variables with different units or magnitudes contribute equally. Without this, a single large-scale variable can dominate all distance calculations
- **Center only** — subtracts the mean but preserves original variance. Use only when all variables share the same unit and variance differences are scientifically meaningful
- **No scaling** — raw values. Only appropriate if data is already on a common scale

**Normalize skewed variables** — optionally transforms variables with |skewness| > 2 using `bestNormalize` before clustering. Reduces outlier influence but changes the measurement scale. See Details tab for full implications.

##### Key Result

After clicking **Run Clustering**, the most important result is the **Cluster Biplot** (open by default) — a scatter plot projecting all specimens onto the two selected dimensions, colour-coded by cluster membership. The degree of separation between clusters directly reflects how distinct the groups are in the chosen variable space.

The **Cluster Results** accordion contains the algorithm summary banner, cluster sizes, quality metrics (Average Silhouette Width, BSS/TSS, Total Within-SS), and the **Cluster Profile** table showing per-cluster variable means against the overall mean — this is the key table for characterising what distinguishes each cluster.

##### Exporting for Prediction

A **Download RDS (for Prediction)** button appears in the Cluster Results accordion when the clustering run used **K-Means or K-Means (PAM)** on **raw measurement data**. The exported bundle can be loaded in the **Prediction** tab to classify new/unknown samples by nearest centroid (K-Means) or nearest medoid (PAM) — the same rule the algorithm itself uses internally.

The button does not appear for:

- **Hierarchical clustering or DBSCAN** — neither has a principled, well-defined rule for placing a new observation into an existing clustering (hierarchical has no `predict` concept; DBSCAN's `eps`/`minPts` are tuned per training dataset)
- **PCA Scores or LDA Scores** as the data source — exporting a cluster built on scores would require bundling the upstream PCA/LDA model as well, which is not currently supported
