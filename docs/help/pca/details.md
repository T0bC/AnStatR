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

**Recommended parameters (parameter screening mode)**

When the **Statistics** tab has computed a parameter screening ranking (see the Plotting tab's **Disable plots (parameter screening mode)**), an **Apply recommended parameters** banner appears above the measurement-column selector. It applies the union of parameters that ranked among the top group separators across all pairwise comparisons. This is purely a selection shortcut — clicking it calls the same column-selection mechanism as manual selection, so recommended and manually chosen columns are treated identically by the PCA computation and by the saved `.rds` bundle (see the **Prediction** module for how the bundle's `numeric_cols` are used to align new data). See the **Statistics** module's Details tab for how the ranking is computed.

##### Technical Specifications

<details>
<summary><strong>PCA Computation Method</strong></summary>

Standard PCA uses `mixOmics::pca()`, which performs PCA via singular value decomposition (SVD) of the data matrix — numerically equivalent to `stats::prcomp()` for non-sparse PCA. sPCA and IPCA also run through mixOmics (`spca()` and `ipca()` respectively), so all three methods share one computational engine.

The mathematical formulation for standard PCA follows:

**X** = **U** **D** **V**ᵀ

Where:
- **X** is the n × p centered (and optionally scaled) data matrix
- **U** contains the left singular vectors (individual scores)
- **D** is the diagonal matrix of singular values
- **V** contains the right singular vectors (variable loadings)

Principal component scores are computed as **X** **V**, and eigenvalues are the squared singular values divided by (n-1).

</details>

<details>
<summary><strong>Sparse PCA (sPCA)</strong></summary>

sPCA fits components one at a time via an iterative NIPALS-style procedure. For each component, a LASSO-style penalty restricts the loading vector to the **keepX** number of non-zero variables, then the data is deflated (the component's contribution removed) before fitting the next component. This means later components' scores depend on the specific sequence of prior components, not just a single matrix projection.

**Choosing keepX**: use the **Optimise variable selection** button in the Analysis Settings tab to run `mixOmics::tune.spca()`, which performs repeated cross-validation over a grid of candidate keepX values and reports the choice that best reproduces the un-penalized components. This mirrors sPLS-DA's keepX tuning in the LDA module. Manually chosen keepX values are marked as untuned in the results until tuning has run.

**Selected Variables**: the non-zero loadings per component are listed in the **Selected Variables** results panel — this is the direct answer to "which variables define this component?"

</details>

<details>
<summary><strong>Independent PCA (IPCA)</strong></summary>

IPCA (`mixOmics::ipca()`) applies Independent Component Analysis (ICA) after an initial PCA-based whitening step, seeking components that are statistically independent — a stronger condition than the uncorrelated-but-possibly-dependent components PCA/sPCA produce. This can reveal structure (e.g., non-Gaussian source signals) that variance maximization does not separate.

**Algorithm**: choose **Deflation** (components extracted one at a time via FastICA, generally more stable for smaller samples) or **Parallel** (all components extracted simultaneously, can be faster on larger datasets but sometimes less stable).

**Important differences from PCA/sPCA**:
- IPCA always centers the data internally; the app's scaling options only control whether variance-scaling is additionally applied
- Components are **not ranked by variance explained** — component order is arbitrary, driven by the ICA algorithm's convergence, not a hierarchy of importance
- **Contribution % and cos² are not computed for IPCA** and do not appear in the results tables, Excel export, or biplot legends — these are FactoMineR-style statistics that presuppose variance-ranked components

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

The Eigenvalues & Variance table is the primary reference for component importance (PCA/sPCA):

| Statistic | Interpretation | Decision Guidance |
|-----------|----------------|-----------------|
| **Variance %** | Proportion of total variance captured by the component | Cumulative target typically 70-90% |
| **Cumulative %** | Running total of explained variance | Stop when adding components yields diminishing returns |

**Kaiser-Guttman Rule** (PCA/sPCA only): components whose eigenvalue exceeds 1.0 (standardized data) explain more variance than the average original variable, justifying retention. See the **Optimal Number of Components** panel.

**IPCA**: the same table is shown, but the percentages reflect each independent component's own variance in fitted order — not a ranking. Do not apply the Kaiser rule or a cumulative-variance target to IPCA; the table is labeled "not ranked" for this reason.

**Variable Results**

| Metric | Definition | Interpretation | Available for |
|--------|-----------|----------------|----------------|
| **Coordinates / Loadings** | Correlation between variable and component (PCA/sPCA) or raw loading (IPCA) | High absolute values indicate strong relationship | All methods |
| **Contributions (%)** | Variable's share of component variance | Values > 1/p indicate above-average contribution | PCA, sPCA |
| **Cos²** | Squared coordinate (quality of representation) | Sum across components indicates how well variable is represented | PCA, sPCA |

For sPCA, variables with a zero loading on a component (excluded by **keepX**) are not part of that component at all — see the **Selected Variables** panel for the explicit non-zero list per component.

**Individual Results**

| Metric | Definition | Interpretation | Available for |
|--------|-----------|----------------|----------------|
| **Coordinates / Scores** | Component scores (position in reduced space) | Visualized in biplot; relative positions show similarity | All methods |
| **Contributions (%)** | Individual's influence on component direction | High values indicate leverage points or outliers | PCA, sPCA |
| **Cos²** | Quality of individual representation | Near 1 = well-represented; near 0 = poorly represented | PCA, sPCA |

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
- **Download full results** — The Excel export contains all coordinates, contributions, and cos² for external validation (IPCA exports omit contribution/cos² sheets, since they do not apply)
- **Handle missing data proactively** — Review which rows are excluded; systematic missingness may bias results
- **Start with standard PCA** — Only switch to sPCA when you specifically need a short variable list, or to IPCA when you specifically want to test for independent (not just uncorrelated) structure; both change what the components mean
- **Always tune keepX before reporting sPCA results** — An untuned, hand-picked keepX is marked as such in the results; use **Optimise variable selection** so the count is chosen by cross-validation, not guesswork
