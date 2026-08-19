#### Principal Component Analysis (PCA)

Reduce data dimensionality by transforming correlated variables into a smaller set of uncorrelated principal components while preserving maximum variance.

##### Data Selection

Configure your analysis in the **Data Selection** sidebar tab:

- **Descriptive (metadata) columns** — Select columns like `SAMPLE_ID`, `SPECIES`, `SITE`, or `PERIOD` that describe your samples. These are not used in the PCA computation but enable the **Dimension-Metadata Correlation** plot and colorize biplots by group
- **Measurement columns** — Select numeric variables to include in the PCA (minimum 2 required). Click **Select all** to quickly select all measurement columns

If the **Statistics** tab has computed a parameter screening ranking (see the Plotting tab's parameter screening mode), an **Apply recommended parameters** banner appears above the measurement-column selector. Clicking it replaces the current selection with the parameters the Statistics tab identified as good group separators — you can still add or remove columns manually afterward.

##### Analysis Type

Choose between three methods in the **Analysis Settings** tab:

| Method | What it optimizes | Key Requirement | Best For |
|--------|-------------------|-----------------|----------|
| **PCA** (Standard) | Variance explained per component | None | Default; general-purpose dimensionality reduction |
| **sPCA** (Sparse) | Variance explained, restricted to a chosen number of variables per component | **keepX** setting per component | Identifying which variables actually define each component, especially with many measurement parameters |
| **IPCA** (Independent) | Statistical independence between components (via ICA) | None | Exploring structure that variance-maximizing PCA/sPCA may not separate — components are not variance-ranked |

**sPCA** works like PCA but forces each component's loadings to use only a limited number of variables (the **keepX** setting), directly answering "which parameters actually drive this component?" — see the **Selected Variables** results panel. Use **Optimise variable selection** in the Analysis Settings tab to let cross-validation choose keepX rather than guessing.

**IPCA** replaces variance maximization with independent component analysis (ICA): it looks for components that are statistically independent, not just uncorrelated. Because IPCA components are not ordered by variance explained, contribution and cos² (quality of representation) are not shown for IPCA — see the Details tab.

##### Scaling and Preprocessing

Choose a **Data Scaling** method to ensure fair contribution from all variables:

- **Scale & Center (recommended)** — Z-score standardization (mean=0, SD=1). Essential when variables have different units or scales
- **Center only** — Subtract mean while preserving original variance. Use when all variables share the same unit and variance differences are meaningful
- **No scaling** — Use raw data only if already preprocessed to the same scale

**Data Normalization** — Enable **Normalize skewed variables** to transform highly skewed data (|skewness| > 2) using the bestNormalize package. This reduces outlier influence but changes data distribution. See Details tab for comprehensive normalization guidance.

**IPCA note**: IPCA always centers data internally regardless of this setting — **Center only** and **No scaling** behave identically for IPCA. Only the **Scale & Center** vs. non-scaled choice has an effect.

##### Key Results

After clicking **Compute PCA**, results appear in collapsible panels:

- **KMO Measure** — Overall KMO displayed with classification (e.g., "0.779 Middling"). Individual Variable KMO table identifies specific variables to improve. Values ≥ 0.8 indicate excellent suitability; values < 0.5 suggest PCA may be inappropriate
- **Optimal Number of Components** — Recommendations via Kaiser criterion, Elbow method, and Parallel Analysis
- **PCA Results** — The **Eigenvalues & Variance** table is the primary reference for component importance, showing variance explained and cumulative percentages (for IPCA, cumulative variance is not a meaningful ranking — see Details tab)

The **Biplot** visualizes individuals and variables simultaneously, with options for grouping by metadata and dimension selection.
