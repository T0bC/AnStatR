#### Frequently Asked Questions

<details>
<summary>Which clustering algorithm should I use for my data?</summary>

The right choice depends on the expected shape and size of your clusters:

| Algorithm | Best for | Avoid when |
|-----------|---------|------------|
| **K-Means** (euclidean) | Compact, roughly spherical clusters of similar size; large datasets | Clusters differ strongly in size, shape, or density; outliers are present |
| **K-Means (PAM)** (manhattan) | Same as K-Means but more robust to outliers; non-Gaussian shapes | Very large datasets (PAM is slower) |
| **Hierarchical + Ward's D2** | Compact clusters; exploratory analysis where k is uncertain; revealing nested structure | Very large datasets (O(n²) memory); strongly non-convex clusters |
| **DBSCAN** | Irregular or non-convex cluster shapes; data with genuine noise/outlier points; unknown k | Data with uniform density; all points should be assigned to a cluster |

**Practical advice**: start with K-Means (euclidean) with Scale & Center enabled. Inspect the Cluster Biplot and Cluster Profile. If clusters appear elongated or unequal in the biplot, try Hierarchical (Ward's D2). If the data contains clear outlier points that should not be forced into clusters, try DBSCAN.

</details>

<details>
<summary>What should I include in the Descriptive (metadata) columns?</summary>

Metadata columns are everything that *identifies* a specimen but is not a numeric measurement you want to cluster on. Typical examples:

- **Sample identifiers**: `SAMPLE_ID`, `SPECIMEN_NO`, `LAB_CODE`
- **Biological/archaeological context**: `SPECIES`, `TAXON`, `SITE`, `PERIOD`, `LAYER`, `TOOTH_TYPE`, `SKELETAL_ELEMENT`
- **Provenance or treatment**: `LOCATION`, `TREATMENT`, `ANALYST`

Including the right metadata columns matters because:
- They appear as labels in the Cluster Membership table and Excel download
- They can be used as **Row Side Colors** in the heatmap to visually test whether a known group variable aligns with the discovered clusters
- They can be used as **Group Biplot** color-coding to overlay known groupings on the cluster scatter plot

Do **not** include metadata columns in the measurement column selection — only purely numeric measurements belong there. Categorical columns in the measurement selection will cause an error.

</details>

<details>
<summary>Why is Scale & Center recommended and what happens if I skip it?</summary>

Clustering algorithms compute pairwise distances between observations. If your variables are on very different scales (e.g., one variable ranges 0–5 µm, another ranges 0–500 µm²), the large-scale variable will completely dominate all distance calculations. Cluster assignments will reflect only that variable, regardless of the others.

**Scale & Center** (z-score standardisation: subtract mean, divide by standard deviation) puts all variables on a common scale with mean = 0 and SD = 1. Every variable then contributes equally to distances.

Without scaling:
- A single high-variance variable can define all clusters on its own
- The Cluster Profile table will look like the cluster structure mirrors just that one variable
- Biologically or archaeologically meaningful variation in other variables is effectively ignored

The only cases where you might skip scaling are:
- All variables are in the same unit and the variance differences are scientifically meaningful (e.g., you want larger-variance traits to have more weight)
- You are clustering on PCA scores or LDA scores, which are already standardised (scaling is skipped automatically in this case)

</details>

<details>
<summary>How should I interpret the Hopkins statistic?</summary>

The Hopkins statistic H tests whether your data has a non-random clustering structure at all, before you commit to a specific k. Values range from 0 to 1:

- **H ≥ 0.75**: The data departs strongly from a uniform random distribution — clustering is likely to produce meaningful, replicable groups
- **H 0.50–0.74**: Some structure exists, but results should be interpreted carefully
- **H < 0.50**: The data resembles a uniform random distribution — the clustering algorithm will partition the data but the partitions are unlikely to correspond to real groups

If Hopkins < 0.5 but you have strong domain knowledge that groups exist, consider:
- Reviewing your variable selection — irrelevant variables add noise
- Switching to PCA scores as input to suppress noise dimensions
- Using a different distance metric

The Hopkins statistic is most reliable with n > 100. For small datasets (n ≤ 100) a warning is shown automatically.

</details>

<details>
<summary>How is the optimal number of clusters determined?</summary>

Three methods are computed and their recommended k values are combined into a median:

- **Elbow (WSS)**: identifies the k where the reduction in total within-cluster sum of squares shows maximum deceleration (the "elbow" of the curve)
- **Silhouette**: selects the k with the highest average silhouette width — a direct measure of how well each observation fits its assigned cluster
- **Gap Statistic**: compares observed WCSS to a bootstrapped uniform reference; selects the smallest k where Gap(k) ≥ Gap(k+1) − SE(k+1)

The **median** of the three recommended k values is auto-set in the Number of clusters input. You can override it manually at any time. If all three methods agree on the same k, that value is highly reliable. If they disagree, inspect the optimal clusters plot and use domain knowledge to choose.

Note: the Gap statistic uses B = 50 bootstrap resamples and can take noticeable time on large datasets. A spinner notification appears during computation.

</details>

<details>
<summary>What does the Average Silhouette Width tell me about my cluster solution?</summary>

The silhouette width of each observation measures how similar it is to its own cluster compared to the nearest other cluster, on a scale from −1 to +1:

- **Near +1**: the observation is well-matched to its cluster and poorly matched to neighbours — clearly belongs where it was assigned
- **Near 0**: the observation sits on the boundary between two clusters — assignment is ambiguous
- **Negative**: the observation may have been assigned to the wrong cluster

The **Average Silhouette Width** (shown in the Cluster Quality panel) aggregates this across all observations:

| Value | Interpretation |
|-------|---------------|
| ≥ 0.71 | **Strong** structure — confident assignment |
| 0.51–0.70 | **Reasonable** structure |
| 0.26–0.50 | **Weak** structure — interpret with caution |
| < 0.26 | **No structure** — clusters may be artificial |

If the average silhouette is consistently below 0.26 across all values of k in the Optimal Clusters plot, the data likely has no real cluster structure (confirm with the Hopkins statistic).

</details>

<details>
<summary>DBSCAN found no clusters or classified all points as noise — what can I do?</summary>

DBSCAN auto-computes both `eps` (neighbourhood radius) and `minPts` from the data. If the density of your data is too uniform, the auto-computed eps may be too small to connect any points into a dense region.

Possible remedies:

1. **Enable Scale & Center** — if not already active. Unscaled data can produce distance distributions that make the eps estimation unreliable
2. **Switch to a different algorithm** — if DBSCAN finds no structure, K-Means or Hierarchical may still partition the data (though whether the partitions are meaningful is a separate question)
3. **Reduce the variable set** — in high dimensions the k-nearest-neighbour distance distribution becomes increasingly uniform, making the eps knee point flat and hard to detect
4. **Use PCA scores** — reduces dimensionality and concentrates the data structure into fewer dimensions, making density estimation more reliable

DBSCAN is most effective when there are genuinely dense regions separated by sparse areas. If your data is fairly uniformly distributed in measurement space, DBSCAN is not the right choice.

</details>

<details>
<summary>Can I cluster on PCA or LDA scores instead of raw measurements?</summary>

Yes, and this is approach is sometimes used. Conduct PCA or LDA teen switch **Data Source** in the Data Selection tab to **PCA Scores** or **LDA Scores**.

**Clustering on PCA scores** is useful when:
- The number of raw variables is large (curse of dimensionality — distances become less informative in high-dimensional space)
- Variables are highly correlated (redundant dimensions inflate k-nearest-neighbour distances)
- You want to suppress noise dimensions while retaining the main variance structure

Select enough PCA dimensions to cover ≥ 90% of cumulative variance (the app shows a recommendation). Scaling is automatically skipped for PCA/LDA scores.

**Clustering on LDA scores** is useful when:
- You already have group labels and want to find sub-groupings within the discriminant space
- You want clusters that are meaningful with respect to the group structure already captured by LDA

For LDA scores: run LDA in **model-fitting mode** (not LOO-CV) and use **LDA** (not QDA) in the LDA tab. QDA does not produce discriminant axis scores.

</details>

<details>
<summary>What does the Cluster Profile table show and how do I interpret it?</summary>

The Cluster Profile table in the **Cluster Results** panel shows the **mean of each measurement variable** for each cluster, computed on the **raw unscaled** data — regardless of the scaling used for clustering. An **Overall** mean row provides the grand mean for comparison.

To interpret a cluster: scan across each row and identify variables where the cluster mean is substantially higher or lower than the overall mean. Clusters that are high on certain traits and low on others represent distinct phenotypic or functional profiles.

Example interpretation pattern:
- Cluster 1 has higher values for abrasion-related textures and lower values for complexity metrics → could represent a distinct wear regime
- Cluster 2 is average on all variables → may represent a transitional or generalist group

The Cluster Profile is the key table for biological and archaeological interpretation — the Biplot shows *separation*, but the Profile tells you *what the separation means*.

</details>

<details>
<summary>What does the Silhouette Plot show and how do I read it?</summary>

The Silhouette Plot visualises the silhouette width of every individual observation in your cluster solution. Each horizontal bar represents one observation:

- **Bar width** = silhouette width (ranges from −1 to +1)
- **Bar direction**: extends right for positive values, left for negative values
- **Bar colour**: coded by cluster assignment
- **Observations are grouped** by cluster, with clusters stacked vertically

**How to interpret individual bars**:
- **Wide bars extending right** (silhouette near +1): the observation is well-matched to its cluster and far from neighbouring clusters
- **Narrow bars** (silhouette near 0): the observation sits on the boundary between two clusters
- **Bars extending left** (negative silhouette): the observation may be misassigned — it is closer to a neighbouring cluster than to its own

**How to interpret cluster patterns**:
- **Cluster with consistently wide bars**: a tight, cohesive group with clear separation from other clusters
- **Cluster with mixed bar widths**: a heterogeneous group; consider whether it should be split further
- **Many negative bars across clusters**: the overall cluster solution is weak; verify with the Hopkins statistic and consider a different k

Use the **Sort by** control to reorder observations within each cluster — sorting by metadata group can reveal whether known biological or archaeological groupings align with the computed cluster boundaries.

</details>

<details>
<summary>When should I use the Silhouette Plot vs. the Average Silhouette metric?</summary>

The **Average Silhouette Width** is a single number summarising overall cluster quality — useful for comparing different k values quickly in the Optimal Clusters panel.

The **Silhouette Plot** shows individual-level detail and is essential when:

- **Diagnosing cluster problems**: low average silhouette can be driven by a few poorly assigned observations or by systematic issues across all clusters — the plot reveals which
- **Identifying outliers**: observations with strongly negative silhouette are candidates for being noise points or measurement errors
- **Checking metadata alignment**: sorting by a metadata column (e.g., `SITE` or `SPECIES`) lets you visually verify whether known groupings align with cluster boundaries
- **Reporting**: the plot provides a publication-quality figure showing cluster quality to accompany the numerical summary

**Recommended workflow**: run clustering, check the Average Silhouette in the Cluster Quality panel, then open the Silhouette Plot to investigate any clusters with low or borderline values. Export the plot (PNG/SVG) for documentation.

</details>

<details>
<summary>What are the "Sort by" options in the Silhouette Plot for?</summary>

The **Sort by** control determines how observations are ordered within each cluster:

| Sort Option | Effect | Best Used When |
|-------------|--------|---------------|
| **Silhouette Width** (default) | Orders observations from best-assigned (widest bar) to worst-assigned (narrowest/negative) | Quickly identifying which observations contribute to low cluster quality |
| **Metadata Group** | Orders observations by the selected metadata column | Checking whether known groupings (e.g., all specimens from Site A) cluster together within the computed clusters |

When **Metadata Group** is selected, choose the grouping variable in the **Metadata Group** dropdown. This is particularly useful for archaeological or biological validation — for example, verifying that specimens from the same provenance or taxon tend to have similar silhouette patterns within their assigned cluster.

The sort order affects only the visual arrangement; silhouette values themselves are unchanged. Use sorting interactively to explore patterns, then export the final arrangement for reporting.

</details>

<details>
<summary>My clusters look well-separated in the biplot but the silhouette is low — why?</summary>

The Cluster Biplot projects the data into two dimensions (via PCA by default). If the two plotted dimensions capture only a fraction of the total variance, the visual separation in the biplot may not reflect true separation in the full measurement space — and vice versa.

A cluster solution can look clean in 2D but have low silhouette because:
- The two PCA dimensions shown explain, say, 60% of variance; the remaining 40% contains within-cluster scatter that the biplot hides
- The clusters are separated on a dimension that is not one of the two plotted dimensions

Steps to investigate:
- Check how much variance Dim.1 + Dim.2 explain (visible in the PCA tab if you ran PCA first)
- Try switching the biplot axes to Dim.3 + Dim.4 — additional separation may appear
- If silhouette is consistently low across all k values in the Optimal Clusters plot, the data may not have a strong cluster structure regardless of what the 2D projection suggests

</details>

<details>
<summary>What are edge cases I should be aware of?</summary>

Several data configurations produce errors or unexpected behaviour:

- **Fewer than 2 rows after NA removal** — if many rows are removed due to missing values, the app reports an error. Deselect columns with many NAs or use imputation before loading
- **Only one unique value in a measurement column** — zero-variance columns are meaningless for distance computation. The app will report a "constant columns" error. Remove such columns from the measurement selection
- **k ≥ n** — the number of clusters cannot equal or exceed the number of observations. The app enforces k ≤ n − 1 automatically, but running with very high k on a small dataset is not meaningful
- **DBSCAN with highly uniform data** — all points classified as noise (cluster 0). See the DBSCAN FAQ entry above
- **Hierarchical with median/centroid linkage** — these methods can produce **inversions** (a merge at a higher height than a later merge), which makes the dendrogram visually non-monotone. This is a known property of these linkages, not a bug. Switch to Ward's D2 or Average linkage to avoid inversions
- **PCA scores not yet available** — if you select PCA Scores as data source before running PCA, an info banner appears. Run PCA in the PCA tab first, then return here
- **LDA scores unavailable** — LDA scores require a model-fitting run (not LOO-CV) using LDA (not QDA). If the LDA tab used QDA or LOO-CV, scores are not produced. The app shows a specific warning explaining which condition applies
- **Very large datasets (n > 1000) with Gap statistic** — bootstrapping 50 resamples on large datasets can take 30–60 seconds. A spinner notification is shown; the result is cached for subsequent runs with the same data and scaling

</details>

<details>
<summary>Which R packages power the Cluster module computation?</summary>

The cluster analysis module uses the following packages for statistical computation and visualisation:

| Package | Purpose | Citation |
|---------|---------|---------|
| **stats** (base R) | K-Means (`kmeans()`), hierarchical clustering (`hclust()`, `cutree()`), distance matrices (`dist()`) | R Core Team (2024). *R: A Language and Environment for Statistical Computing*. R Foundation for Statistical Computing |
| **cluster** | PAM (`pam()`), silhouette widths (`silhouette()`), Gap statistic (`clusGap()`, `maxSE()`) | Maechler, M., Rousseeuw, P., Struyf, A., Hubert, M., & Hornik, K. (2023). *cluster: Cluster Analysis Basics and Extensions*. R package version 2.1.x |
| **dbscan** | DBSCAN (`dbscan()`), k-nearest-neighbour distances (`kNNdist()`) | Hahsler, M., Piekenbrock, M., & Doran, D. (2019). dbscan: Fast density-based clustering with R. *Journal of Statistical Software*, 91(1), 1–30. doi:10.18637/jss.v091.i01 |
| **hopkins** | Hopkins clusterability statistic (`hopkins()`) | Ripley, B., & Maechler, M. (2022). *hopkins: Calculate Hopkins Statistic to Assess Clustering Tendency*. R package |
| **bestNormalize** | Optional skewness correction before clustering | Peterson, R. A., & Cavanaugh, J. E. (2020). Ordered quantile normalization. *Journal of Applied Statistics*, 47(13–15), 2312–2327. doi:10.1080/02664763.2019.1630372 |
| **ggplot2** | Optimal clusters plot, cluster biplot | Wickham, H. (2016). *ggplot2: Elegant Graphics for Data Analysis*. Springer |
| **ggiraph** | Interactive SVG plots with hover tooltips | Gohel, D., & Skintzos, P. (2024). *ggiraph: Make 'ggplot2' Graphics Interactive*. R package |
| **plotly** | Interactive heatmap with dendrogram | Sievert, C. (2020). *Interactive Web-Based Data Visualization with R, plotly, and shiny*. Chapman and Hall/CRC |
| **DT** | Interactive cluster membership and profile tables | Xie, Y., Cheng, J., & Tan, X. (2024). *DT: A Wrapper of the JavaScript Library 'DataTables'*. R package |

Additional supporting packages: **openxlsx** (Excel export), **bslib** (UI layout), **shiny** (application framework).

</details>
