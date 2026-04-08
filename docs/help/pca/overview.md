#### Principal Component Analysis (PCA)

Reduce data dimensionality by transforming correlated variables into a smaller set of uncorrelated principal components while preserving maximum variance.

##### Data Selection

Configure your analysis in the **Data Selection** sidebar tab:

- **Descriptive (metadata) columns** — Select columns like `SAMPLE_ID`, `SPECIES`, `SITE`, or `PERIOD` that describe your samples. These are not used in the PCA computation but enable the **Dimension-Metadata Correlation** plot and colorize biplots by group
- **Measurement columns** — Select numeric variables to include in the PCA (minimum 2 required). Click **Select all** to quickly select all measurement columns

##### Scaling and Preprocessing

Choose a **Data Scaling** method to ensure fair contribution from all variables:

- **Scale & Center (recommended)** — Z-score standardization (mean=0, SD=1). Essential when variables have different units or scales
- **Center only** — Subtract mean while preserving original variance. Use when all variables share the same unit and variance differences are meaningful
- **No scaling** — Use raw data only if already preprocessed to the same scale

**Data Normalization** — Enable **Normalize skewed variables** to transform highly skewed data (|skewness| > 2) using the bestNormalize package. This reduces outlier influence but changes data distribution. See Details tab for comprehensive normalization guidance.

##### Key Results

After clicking **Compute PCA**, results appear in collapsible panels:

- **KMO Measure** — Overall KMO displayed with classification (e.g., "0.779 Middling"). Individual Variable KMO table identifies specific variables to improve. Values ≥ 0.8 indicate excellent suitability; values < 0.5 suggest PCA may be inappropriate
- **Optimal Number of Components** — Recommendations via Kaiser criterion, Elbow method, and Parallel Analysis
- **PCA Results** — The **Eigenvalues & Variance** table is the primary reference for component importance, showing variance explained and cumulative percentages

The **Biplot** visualizes individuals and variables simultaneously, with options for grouping by metadata and dimension selection.
