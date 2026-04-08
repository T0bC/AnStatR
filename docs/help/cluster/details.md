#### Cluster Analysis — Technical Reference

##### Requirements

**Data Structure**

| Requirement | Raw Data | PCA Scores | LDA Scores |
|-------------|----------|-----------|------------|
| **Min. observations** | ≥ k + 1 (must exceed cluster count) | Same | Same |
| **Min. measurement columns** | ≥ 1 numeric | All Dim.X columns | All LD columns |
| **Data type** | Numeric only | Numeric only | Numeric only |
| **Missing values** | Rows with NAs in measurement columns excluded automatically | NAs not expected (PCA already cleaned) | NAs not expected |
| **Max. clusters (k)** | min(n − 1, 10) | same | same |

Where n = number of observations after NA removal.

**Descriptive (Metadata) Columns**

Metadata columns are passed through untouched — they do not influence distances, cluster assignments, or any quality metrics. They are preserved in:
- The **Cluster Membership** table (original data + `Cluster` assignment column)
- The **Heatmap** row side-colour annotations (select via **Row Side Colors**)
- The **Excel download** (Membership sheet)

Select all columns that carry specimen identity, provenance, or group information (e.g., `SAMPLE_ID`, `SPECIES`, `SITE`, `PERIOD`). Omitting them does not affect cluster computation but makes results harder to interpret and export.

##### Technical Specifications

<details>
<summary><strong>K-Means Algorithm</strong></summary>

K-Means is implemented via `stats::kmeans()` with `nstart = 25`. The algorithm partitions n observations into k clusters by minimising the total within-cluster sum of squares (WCSS):

$$\underset{C}{\arg\min} \sum_{k=1}^{K} \sum_{\mathbf{x}_i \in C_k} \|\mathbf{x}_i - \boldsymbol{\mu}_k\|^2$$

where $\boldsymbol{\mu}_k$ is the centroid of cluster $k$. The `nstart = 25` setting runs 25 random initialisations and keeps the best solution, substantially reducing sensitivity to initialisation. K-Means uses Euclidean distance exclusively and assumes clusters are roughly **convex and of similar size**. It is the fastest algorithm and works well for moderately sized datasets with compact, isotropic clusters.

**References**: MacQueen (1967); Hartigan & Wong (1979).

</details>

<details>
<summary><strong>K-Means (PAM) — Partitioning Around Medoids</strong></summary>

When the **manhattan** distance metric is selected together with the K-Means algorithm, the app automatically switches to PAM (`cluster::pam()`, `nstart = 10`). Unlike K-Means, PAM represents each cluster by an actual observation (the **medoid** — the most centrally located real data point) rather than an abstract centroid. The objective is to minimise:

$$\sum_{k=1}^{K} \sum_{\mathbf{x}_i \in C_k} d(\mathbf{x}_i, \mathbf{m}_k)$$

where $d$ is Manhattan distance and $\mathbf{m}_k$ is the medoid of cluster $k$.

PAM is more **robust to outliers** than K-Means because a single extreme point cannot shift the cluster centre — only a genuine majority of the cluster members can. The medoids table in the **Cluster Results** panel identifies the representative specimens.

**References**: Kaufman & Rousseeuw (1990).

</details>

<details>
<summary><strong>Hierarchical Clustering</strong></summary>

Hierarchical clustering is implemented via `stats::dist()` + `stats::hclust()` + `stats::cutree()`. It builds an **agglomerative dendrogram** by successively merging the two closest clusters until all observations are in a single group, then cuts the tree at the desired height to produce k clusters.

The linkage method controls how "distance between clusters" is defined when clusters contain more than one point:

| Linkage | Definition | Properties |
|---------|-----------|------------|
| **Ward's D2** (default, `ward.D2`) | Minimises total within-cluster variance at each merge | Produces compact, roughly equal-sized clusters; most widely used for morphometric data |
| **Single** | Distance of the two *closest* points | Susceptible to chaining; useful for detecting elongated clusters |
| **Complete** | Distance of the two *farthest* points | Produces tight, compact clusters; sensitive to outliers |
| **Average (UPGMA)** | Mean pairwise distance between all point pairs | Good general-purpose compromise |
| **McQuitty (WPGMA)** | Weighted average; equal weight to each cluster regardless of size | Useful when cluster sizes differ substantially |
| **Median (WPGMC)** | Median of sub-cluster centroids | Can produce inversions (non-monotone height sequence) |
| **Centroid (UPGMC)** | Unweighted centroid distance | Also susceptible to inversions |

**Ward's D2** is strongly recommended for morphometric and texture analysis data. It minimises the within-cluster variance increase at each merge step and tends to produce visually clean, interpretable cluster structures (Murtagh & Legendre, 2014).

The **Height Range** in the Cluster Results panel shows the min/max dendrogram heights. Large jumps in height suggest natural cluster boundaries — look for the largest height gap when choosing k.

**References**: Ward (1963); Murtagh & Legendre (2014).

</details>

<details>
<summary><strong>DBSCAN — Density-Based Spatial Clustering</strong></summary>

DBSCAN (`dbscan::dbscan()`) identifies clusters as **dense regions** separated by low-density areas. It does not require pre-specifying k; instead, it requires two parameters:

- **eps** (ε) — neighbourhood radius: a point is a *core point* if at least `minPts` other points lie within distance ε
- **minPts** — minimum neighbourhood size to qualify as a core point

Both parameters are **auto-computed**:
- `minPts` = max(3, min(round(ln(n)), p + 1)), where n = observations, p = variables. This scales sensibly with dataset size
- `eps` is estimated from the sorted k-nearest-neighbour distance curve using a Kneedle-style knee-point detection on the normalised curve

**Noise points** (cluster label = 0) are observations that do not belong to any dense region. They appear as a "Noise" row in the Cluster Sizes table with a warning badge.

DBSCAN is appropriate when:
- Cluster shapes are **non-convex** or irregular
- A small fraction of observations are genuine outliers that should not be forced into clusters
- The number of clusters is unknown a priori

It is **not appropriate** when data density is relatively uniform across the dataset or when all observations should be assigned to a cluster (consider K-Means or Hierarchical instead).

**References**: Ester et al. (1996); Hahsler, Piekenbrock & Doran (2019).

</details>

<details>
<summary><strong>Distance Metrics</strong></summary>

All algorithms except K-Means (which internally uses squared Euclidean distances in its centroid update) expose the distance metric choice:

| Metric | Formula | Properties |
|--------|---------|-----------|
| **Euclidean** (default) | $\sqrt{\sum_j (x_{ij} - x_{kj})^2}$ | Standard "as-the-crow-flies" distance; sensitive to scale differences; works best with Scale & Center applied |
| **Manhattan** | $\sum_j \vert x_{ij} - x_{kj}\vert$ | Measures "city-block" distance; more robust to outliers and non-normal distributions; automatically triggers PAM for K-Means |

With **Scale & Center** applied, both metrics yield comparable results. Without scaling, Euclidean distances are dominated by high-variance variables; Manhattan distances are somewhat less sensitive but still affected.

</details>

##### Clusterability and Optimal Cluster Selection

<details>
<summary><strong>Hopkins Statistic</strong></summary>

The Hopkins statistic H (computed via the `hopkins` package) assesses whether the data has a non-random, clusterable structure before committing to a specific k. It compares the nearest-neighbour distances of m randomly sampled real data points against m randomly generated points from a uniform distribution over the data range:

$$H = \frac{\sum_{i=1}^{m} u_i^d}{\sum_{i=1}^{m} u_i^d + \sum_{i=1}^{m} w_i^d}$$

where $u_i$ are distances from random points to their nearest data point and $w_i$ are distances from sampled data points to their nearest data point.

| H value | Interpretation |
|---------|---------------|
| **≥ 0.75** | Highly clusterable — strong non-random structure; clustering likely to produce meaningful results |
| **0.50 – 0.74** | Moderately clusterable — some structure present; results should be interpreted with caution |
| **< 0.50** | Not clusterable — data appears uniformly distributed; cluster analysis may not be reliable |

The sample fraction m used is 10% of n when n > 100, else 5% (minimum 1). The Hopkins statistic is most reliable with n > 100 and fewer than 10 dimensions. Interpret with caution for small datasets or high-dimensional data (warnings are shown automatically).

**References**: Hopkins & Skellam (1954); Cross & Jain (1982); Lawson & Jurs (1990).

</details>

<details>
<summary><strong>Optimal Number of Clusters</strong></summary>

Three complementary methods are computed over k = 2…10 (or n − 1 if smaller); the **median** of their three recommended k values is used as the automatic suggestion:

**1. Elbow / WSS (Within-cluster Sum of Squares)**
Plots total WCSS against k. The optimal k is identified at the "elbow" — the point of maximum second-order curvature (Kneedle method on the normalised WCSS curve). A clear elbow indicates a natural cluster boundary.

**2. Silhouette**
For each k, computes the average silhouette width across all observations (see below). The k with the **maximum average silhouette** is recommended. Values near 1 indicate tight, well-separated clusters; near 0 indicate overlapping boundaries; negative values indicate possible misassignment.

**3. Gap Statistic**
Compares observed WCSS to expected WCSS under a reference uniform distribution (bootstrapped with B = 50 resamples via `cluster::clusGap()`). The optimal k is identified using the *firstSEmax* rule (Tibshirani, Walther & Hastie, 2001): the smallest k such that Gap(k) ≥ Gap(k+1) − SE(k+1).

The optimal clusters plot shows all three methods as interactive faceted panels; the recommended k for each method is marked in red. The median k is automatically applied to the **Number of clusters** input.

**References**: Tibshirani, Walther & Hastie (2001); Rousseeuw (1987); Thorndike (1953).

</details>

##### Data Interpretation — Cluster Quality Metrics

<details>
<summary><strong>Average Silhouette Width</strong></summary>

The silhouette width of observation i is computed via `cluster::silhouette()`:

$$s(i) = \frac{b(i) - a(i)}{\max\{a(i), b(i)\}}$$

where $a(i)$ is the mean distance from i to all other members of its cluster and $b(i)$ is the mean distance from i to the nearest neighbouring cluster.

| Avg. Silhouette | Interpretation |
|-----------------|---------------|
| **≥ 0.71** | **Strong** — cluster structure well-defined |
| **0.51 – 0.70** | **Reasonable** — cluster structure exists with some overlap |
| **0.26 – 0.50** | **Weak** — cluster structure exists but with substantial overlap; interpret cautiously |
| **< 0.26** | **No structure** — data may not cluster meaningfully; verify Hopkins statistic |

**References**: Rousseeuw (1987).

</details>

<details>
<summary><strong>BSS / TSS</strong></summary>

BSS/TSS (Between-cluster Sum of Squares / Total Sum of Squares) measures the proportion of total variance explained by the cluster structure. Values closer to 1.0 indicate that most variance is *between* clusters rather than within them. This is the multivariate analogue of R² in regression.

- **BSS/TSS > 0.70** suggests a strong cluster separation
- **BSS/TSS < 0.30** suggests the cluster solution captures little of the total variance; the clusters may not be meaningful

Note: BSS/TSS tends to increase monotonically with k even for random data. Always inspect the Silhouette and the biplot alongside it.

</details>

<details>
<summary><strong>Total Within-SS</strong></summary>

The total within-cluster sum of squares (computed from the raw — unscaled — data) is the objective function minimised by K-Means. Lower values indicate tighter, more compact clusters. Use it to compare solutions on the same dataset. Comparing across different datasets or scalings is not meaningful.

</details>

<details>
<summary><strong>Cluster Profile (Variable Means)</strong></summary>

The **Cluster Profile** table shows the mean of each measurement variable for each cluster, computed on the **raw (unscaled)** data regardless of the scaling used for clustering. An **Overall** mean row at the bottom provides a reference.

To characterise a cluster: identify variables where the cluster mean deviates most from the overall mean. Large positive deviations indicate the cluster is enriched for that trait; large negative deviations indicate it is depleted. These patterns form the biological or archaeological interpretation of what each cluster represents.

</details>

##### Scaling Implications for Clustering

<details>
<summary><strong>Scaling Decisions and Their Effect on Distance Matrices</strong></summary>

Clustering algorithms operate on a **pairwise distance matrix**. The choice of scaling directly determines which variables dominate that matrix:

| Scaling | Effect on Distance Matrix | Recommended When |
|---------|--------------------------|-----------------|
| **Scale & Center** | All variables contribute equally (unit variance); distance matrix is rotation-invariant in standardised space | Variables have different units (mm, %, counts, µm²…) — the default for morphometric and texture analysis |
| **Center only** | Variables with higher raw variance have proportionally greater influence on distances | All variables share the same unit and variance differences are scientifically meaningful |
| **No scaling** | Distance dominated by variables with large absolute values | Data already on a common scale; using PCA or LDA scores as input |

**Critical implication**: without scaling, a single variable with a range of 0–100 can completely dominate the distance matrix while a variable with a range of 0–1 (even if equally important biologically) contributes negligibly. This produces clusters that reflect only the high-variance variable regardless of the others. **Scale & Center is almost always appropriate for mixed-unit measurement data.**

When using **PCA scores** or **LDA scores** as input, scaling is automatically skipped because dimension-reduction steps already standardise the feature space.

</details>

<details>
<summary><strong>Data Normalisation (Skewness Correction)</strong></summary>

The **Normalize skewed variables** option uses the `bestNormalize` package (Peterson & Cavanaugh, 2020) to transform variables with |skewness| > 2 before clustering. Candidate transformations (Box-Cox, Yeo-Johnson, log, square-root, ordered quantile normalisation) are evaluated automatically; the best one is selected by minimising the Pearson P/df statistic.

Clustering algorithms using Euclidean or Manhattan distances implicitly treat the data as if it were drawn from a symmetric distribution. Heavily right-skewed variables produce extreme distance values for outliers, pulling cluster centroids towards them and distorting the partition. Normalisation mitigates this but:

- Changes the measurement scale — variable means in the Cluster Profile table will reflect the transformed scale, not the original units
- Is not necessary when using PCA scores (PCA itself is sensitive to skewness but the transformation was applied before PCA if enabled there)
- Should only be enabled when outliers likely represent **measurement error** rather than genuine biological signal

A skewness warning banner is shown automatically after running clustering when skewed columns are detected but normalisation is disabled.

</details>

##### Display Options

Configure visualisations in the **Display Options** sidebar tab:

###### Cluster Biplot

| Control | Options | Effect |
|---------|---------|--------|
| **Reduction Method** | PCA (default), Raw Data | PCA projects all measurement variables into two dimensions via PCA; Raw uses two selected measurement columns directly as axes |
| **Dim.X / Dim.Y** | Dim.1, Dim.2, … (PCA mode) or measurement column names (Raw mode) | Select which axes are shown on the scatter plot |
| **Group Biplot** | Any metadata column or `CLUSTER` | Colour-codes points by the selected variable; default is `CLUSTER` |
| **Show Ellipses / Hulls** | On/Off | Overlays 95% confidence ellipses or convex hulls around each group defined by the Group Biplot selection |
| **Use Convex Hull** | On/Off | When ellipses are enabled: switches from 95% ellipse to convex hull |
| **Point Alpha** | Fixed (0.25–1.0) or Contribution | Transparency; Contribution scales alpha by each point's contribution to Dim.1 |
| **Point Size** | Fixed (1–10) or Contribution | Size; Contribution scales size by each point's contribution to Dim.1 |

###### Heatmap

The heatmap renders observations as rows and measurement variables as columns, with cell colours encoding z-scaled variable values. The dendrogram on the left shows the hierarchical structure of the data (always recomputed via hierarchical clustering regardless of the algorithm used for clustering).

| Control | Options | Effect |
|---------|---------|--------|
| **Show Labels** | On/Off | Shows row labels on the heatmap |
| **Label Column** | Any selected metadata column | Metadata column to use as row labels; row numbers shown if empty |
| **Seriation (leaf ordering)** | OLO (optimal), GW (fast heuristic), Mean, None | Controls how dendrogram leaves are reordered for visual clarity; **OLO** (Optimal Leaf Ordering) minimises the sum of adjacent distances and gives the most interpretable arrangement |
| **Row Side Colors** | Any selected metadata column(s) | Coloured annotation bars alongside heatmap rows — useful for visually confirming whether metadata groups align with cluster structure |

###### Plot Export

**Width (cm)** and **Height (cm)** set the dimensions for SVG and PNG downloads. A width of 16 cm corresponds to a standard Word document page width; 10 cm height is a sensible default for portrait-format figures.

##### Best Practices

- **Always run with Scale & Center** unless all variables share the same unit and you have a specific reason to preserve variance differences
- **Inspect the Hopkins statistic first** — a value < 0.5 suggests the data has no clustering structure; proceeding with arbitrary k produces meaningless clusters
- **Use the Optimal Clusters panel** to guide k selection, but treat the median recommendation as a starting point rather than a definitive answer
- **Compare the Cluster Biplot and Heatmap** — the biplot shows separation in reduced space; the heatmap shows per-variable patterns. Consistent patterns across both confirm a robust cluster solution
- **Examine the Cluster Profile table** carefully — it is the primary tool for interpreting what each cluster represents biologically or archaeologically
- **For high-dimensional data** (many variables), prefer clustering on PCA scores (≥ 90% variance) to avoid the curse of dimensionality
- **For data with known groups**, cluster on LDA scores — discriminant axes maximise group separation, making clusters in LD space more interpretable
- **Download the Excel file** after every run — it contains both the full membership table and the cluster profile sheet for reporting

**References**

- Cross, G. R., & Jain, A. K. (1982). Measurement of clustering tendency. *Theory and Applications of Digital Information Processing*, 315–320.
- Ester, M., Kriegel, H.-P., Sander, J., & Xu, X. (1996). A density-based algorithm for discovering clusters in large spatial databases with noise. *Proceedings of KDD-96*, 226–231.
- Hahsler, M., Piekenbrock, M., & Doran, D. (2019). dbscan: Fast density-based clustering with R. *Journal of Statistical Software*, 91(1), 1–30.
- Hartigan, J. A., & Wong, M. A. (1979). Algorithm AS 136: A K-means clustering algorithm. *Journal of the Royal Statistical Society: Series C*, 28(1), 100–108.
- Hopkins, B., & Skellam, J. G. (1954). A new method for determining the type of distribution of plant individuals. *Annals of Botany*, 18(2), 213–227.
- Kaufman, L., & Rousseeuw, P. J. (1990). *Finding Groups in Data: An Introduction to Cluster Analysis*. Wiley.
- Lawson, R. G., & Jurs, P. C. (1990). New index for clustering tendency and its application to chemical problems. *Journal of Chemical Information and Computer Sciences*, 30(1), 36–41.
- MacQueen, J. (1967). Some methods for classification and analysis of multivariate observations. *Proceedings of the 5th Berkeley Symposium*, 1, 281–297.
- Murtagh, F., & Legendre, P. (2014). Ward's hierarchical agglomerative clustering method: which algorithms implement Ward's criterion? *Journal of Classification*, 31(3), 274–295.
- Peterson, R. A., & Cavanaugh, J. E. (2020). Ordered quantile normalization. *Journal of Applied Statistics*, 47(13-15), 2312–2327.
- Rousseeuw, P. J. (1987). Silhouettes: a graphical aid to the interpretation and validation of cluster analysis. *Journal of Computational and Applied Mathematics*, 20, 53–65.
- Thorndike, R. L. (1953). Who belongs in the family? *Psychometrika*, 18(4), 267–276.
- Tibshirani, R., Walther, G., & Hastie, T. (2001). Estimating the number of clusters in a data set via the gap statistic. *Journal of the Royal Statistical Society: Series B*, 63(2), 411–423.
- Ward, J. H. (1963). Hierarchical grouping to optimize an objective function. *Journal of the American Statistical Association*, 58(301), 236–244.
