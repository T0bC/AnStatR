#### PCA Requirements and Technical Reference

##### Requirements

**Data Structure**

| Requirement | Specification | Notes |
|-------------|---------------|-------|
| **Minimum variables** | 2 numeric columns | More variables recommended for meaningful dimensionality reduction |
| **Minimum observations** | 3 rows | At least n > p for full rank covariance matrix |
| **Data type** | Numeric only | Categorical data must be encoded or used as metadata |
| **Missing values** | Rows with NAs excluded | Automatic removal; ensure sufficient data remains |

**Metadata Columns**

Descriptive columns (e.g., `SAMPLE_ID`, `SPECIES`, `SITE`, `PERIOD`) serve two purposes in PCA:

1. **Visualization grouping** — Colorize biplot points by category for pattern detection
2. **Dimension-metadata correlation** — Statistical relationships between PC scores and metadata variables

The PCA computation itself ignores metadata; it operates purely on measurement columns.

##### Technical Specifications

<details>
<summary><strong>PCA Computation Method</strong></summary>

The application uses `stats::prcomp()` from the R stats package, which performs PCA via singular value decomposition (SVD) of the data matrix. This approach is numerically stable and computationally efficient compared to eigen-decomposition of the covariance matrix.

The mathematical formulation follows:

**X** = **U** **D** **V**ᵀ

Where:
- **X** is the n × p centered (and optionally scaled) data matrix
- **U** contains the left singular vectors (individual scores)
- **D** is the diagonal matrix of singular values
- **V** contains the right singular vectors (variable loadings)

Principal component scores are computed as **X** **V**, and eigenvalues are the squared singular values divided by (n-1).

</details>

<details>
<summary><strong>Scaling Implications</strong></summary>

Scaling decisions fundamentally change the PCA solution and interpretation:

| Scaling | Covariance Structure | Use Case | Risk |
|---------|---------------------|----------|------|
| **Scale & Center** | Correlation matrix | Variables on different scales; different units | High-variance variables may lose dominance |
| **Center only** | Covariance matrix | Same units; variance carries information | High-variance variables dominate components |
| **None** | Raw cross-products | Already standardized data | Arbitrary scale differences bias results |

**Key insight**: With "Scale & Center", variables contribute equally to component formation regardless of original variance. With "Center only", high-variance variables exert stronger influence on the principal components.

</details>

<details>
<summary><strong>Data Normalization</strong></summary>

The **bestNormalize** package automatically selects optimal transformations for skewed variables (|skewness| > 2). Candidate transformations include:

| Method | Formula | Best For |
|--------|---------|----------|
| **Box-Cox** | (x^λ - 1) / λ | Continuous positive data |
| **Yeo-Johnson** | Generalized Box-Cox | Data with zero/negative values |
| **Log** | log(x) | Right-skewed, multiplicative data |
| **Square-root** | √x | Mild right skew, count data |

**When to normalize**: Enable when outliers likely represent measurement error rather than true signal. Normalization reduces leverage of extreme values but alters the data distribution—interpret loadings with caution when transformations are applied.

</details>

##### Data Interpretation

**Correlation Matrix**

The **Correlation Matrix** heatmap displays Pearson correlations between all measurement variables. Use this diagnostic to identify data structure issues before interpreting PCA results:

| Pattern | Interpretation | PCA Implication |
|---------|----------------|-----------------|
| **Near-perfect correlations (r > 0.95)** | Redundant variables | Remove one to prevent singular matrix errors; redundant variables don't add information |
| **Strong correlations (0.7 < r < 0.95)** | Related measurements | Expected for PCA; these variables will likely load on the same component |
| **Weak correlations (r < 0.3)** | Independent variables | May form separate components or represent noise |
| **Mixed correlation structure** | Diverse variable relationships | PCA will extract multiple components to capture different correlation clusters |

**Conclusion for PCA**: The correlation matrix predicts how many meaningful components PCA will extract. If most correlations are weak, expect many components with low variance each. If variables cluster into strongly correlated blocks, expect fewer components capturing those block structures.

**Eigenvalues and Variance**

The eigenvalue table is the primary reference for component importance:

| Statistic | Interpretation | Decision Guidance |
|-----------|----------------|-----------------|
| **Eigenvalue** | Variance captured by component | Kaiser criterion: retain components with λ > 1 |
| **Variance %** | Proportion of total variance | Cumulative target typically 70-90% |
| **Cumulative %** | Running total of explained variance | Stop when adding components yields diminishing returns |

**Kaiser-Guttman Rule**: Components with eigenvalues exceeding 1.0 explain more variance than the average original variable (standardized data), justifying retention.

**Variable Results**

| Metric | Definition | Interpretation |
|--------|-----------|----------------|
| **Coordinates** | Correlation between variable and component | High absolute values indicate strong relationship |
| **Contributions (%)** | Variable's share of component variance | Values > 1/p indicate above-average contribution |
| **Cos²** | Squared coordinate (quality of representation) | Sum across components indicates how well variable is represented |

**Individual Results**

| Metric | Definition | Interpretation |
|--------|-----------|----------------|
| **Coordinates** | PC scores (position in reduced space) | Visualized in biplot; relative positions show similarity |
| **Contributions (%)** | Individual's influence on component direction | High values indicate leverage points or outliers |
| **Cos²** | Quality of individual representation | Near 1 = well-represented; near 0 = poorly represented |

**Visualization Panels**

Configure plots in the **PCA Plotting Controls** sidebar tab:

| Control | Options | Effect |
|---------|---------|--------|
| **Biplot Layer** | Individuals, Variables (Loadings), Combined | Toggle which elements appear in the biplot |
| **Group Biplot** | Metadata columns | Color-code points by descriptive variable for pattern detection |
| **Convex Hull** | On/Off | Replace 95% confidence ellipses with minimum bounding polygons |
| **Dim.X / Dim.Y / Dim.Z** | Component selection | Choose which principal components map to each axis |
| **Point Alpha / Size** | Fixed values or "Contribution" | Vary transparency/size by individual contribution to Dim.1 |

**Variable Contributions Plot**

The **Variable Contributions** jitter plot displays contribution percentages across all retained dimensions. Variables with consistently high contributions (> 1/p, where p = number of variables) are the primary drivers of your PCA structure. This plot reveals:

- Which measurements define each principal component
- Variables that contribute across multiple dimensions (general importance)
- Variables with narrow, focused contributions (specific to one component)

**Individual Contributions Plot**

The **Individual Contributions** plot shows how much each observation influences the direction of each principal component. High-contribution individuals act as "anchor points" that pull component axes toward them. Use this to:

- Identify outliers that may warrant investigation
- Detect clusters where boundary individuals have elevated contributions
- Verify that no single observation dominates multiple components

##### Quality Assurance

**Kaiser-Meyer-Olkin (KMO) Measure**

The UI reports the overall KMO measure with a classification badge (e.g., **"KMO Measure — 0.779 Middling"**) and an **Individual Variable KMO** table listing per-variable MSA values for targeted diagnostics.

| KMO Value | Classification | Badge Color | Action |
|-----------|---------------|-------------|--------|
| ≥ 0.90 | Marvelous | Green | Proceed with confidence |
| 0.80-0.89 | Meritorious | Green | Suitable for PCA |
| 0.70-0.79 | Middling | Yellow | Acceptable; monitor results |
| 0.60-0.69 | Mediocre | Yellow | Marginal; consider variable selection |
| 0.50-0.59 | Miserable | Red | Questionable; review variable correlations |
| < 0.50 | Unacceptable | Red | Do not proceed with PCA |

Check the **Individual Variable KMO** table for specific variables with low MSA (< 0.5). Removing these variables often improves the overall KMO measure.

**Variance Explained Thresholds**

| Components Retained | Cumulative Variance | Interpretation |
|---------------------|---------------------|----------------|
| 2 | 50-60% | Minimal acceptable for visualization |
| 2-3 | 60-80% | Typical for moderately correlated data |
| 3-4 | 70-90% | Good retention for most analyses |
| 4+ | 80-95% | High-dimensional data or strong correlations |

##### Best Practices

- **Check KMO before interpreting results** — Values below 0.5 indicate PCA is inappropriate for your data structure
- **Use Scale & Center as default** — Ensures no single variable dominates due to measurement scale
- **Select meaningful metadata** — Descriptive columns enable richer visualization and correlation analysis
- **Validate component count** — Compare Kaiser, Elbow, and Parallel Analysis recommendations; avoid over/under-extraction
- **Inspect biplot layer by layer** — Examine "Individuals" and "Variables" separately before combined view
- **Download full results** — The Excel export contains all coordinates, contributions, and cos² for external validation
- **Handle missing data proactively** — Review which rows are excluded; systematic missingness may bias results
