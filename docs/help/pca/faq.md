#### Frequently Asked Questions

<details>
<summary>Why is my KMO value low or NaN?</summary>

The UI displays the overall KMO measure with a classification label (e.g., **"KMO Measure — 0.779 Middling"**) and an **Individual Variable KMO** table showing per-variable sampling adequacy values.

Low KMO values indicate your data may be unsuitable for PCA. Common causes:

- **High correlations between variables** — PCA assumes some but not perfect correlation. Remove variables with |r| > 0.95
- **Constant or near-constant columns** — Zero variance prevents computation. Remove columns with SD ≈ 0
- **Too few observations** — Ensure n > p (observations exceed variables)
- **Sparse data** — Many missing values reduce effective sample size

The KMO statistic measures how closely variables are related to each other (without being too closely related). Check the **Individual Variable KMO** table—variables with MSA values below 0.5 are candidates for removal to improve overall sampling adequacy.

</details>

<details>
<summary>Should I use Scale & Center or Center only?</summary>

Use **Scale & Center** (z-score) when:
- Variables have different units (e.g., mm, degrees, counts)
- Measurement scales differ by orders of magnitude
- You want equal contribution regardless of original variance

Use **Center only** when:
- All variables share the same unit
- Variance differences carry meaningful information
- You want high-variance variables to have stronger influence

**Example**: For texture measurements (all in micrometers), center-only preserves the relative importance of rougher vs. smoother surfaces. For mixed units (texture + chemical composition percentages), always use Scale & Center.

</details>

<details>
<summary>How many components should I retain?</summary>

The application provides three objective criteria:

| Method | Rule | When to Override |
|--------|------|------------------|
| **Kaiser** | Eigenvalue > 1 | When many components barely exceed 1.0 |
| **Elbow** | Scree plot inflection | Subjective; verify visually |
| **Parallel** | Exceeds random data | Conservative; may under-extract |

Practical guidelines:
- **Minimum**: Retain enough components to explain ≥ 60% cumulative variance
- **Visualization**: 2-3 components for plots; additional components for analysis
- **Interpretability**: Fewer components are easier to interpret meaningfully

Consider your analytical goals: dimensionality reduction for visualization requires fewer components than capturing complex structure for downstream analysis.

</details>

<details>
<summary>My biplot shows overlapping points—how do I improve it?</summary>

Overlapping points indicate either:

1. **Many similar samples** — Genuine data structure; consider:
   - Adjusting **Point Alpha** to "Contribution" to reduce opacity of low-contribution points
   - Using **Convex Hull** instead of 95% ellipse for cleaner group boundaries
   - Exporting at higher resolution for manual inspection

2. **Insufficient variance captured** — First two components don't separate groups well:
   - Try different dimension combinations (Dim.1 vs Dim.3, Dim.2 vs Dim.3)
   - Use the **3D Biplot** for three-dimensional perspective
   - Check the **Eigenvalues & Variance** table for component strength

3. **No meaningful groups exist** — Data may be homogeneous; verify with the **Variable Contributions** plot to confirm PCA structure.

</details>

<details>
<summary>What does the Dimension-Metadata Correlation plot show?</summary>

The **Eigencorrelation** plot visualizes Pearson correlations between PC scores (individual positions in component space) and your selected metadata columns. It answers: "Do my descriptive variables explain the PCA structure?"

| Correlation | Interpretation |
|-------------|----------------|
| Strong positive (r > 0.5) | Metadata values increase with PC scores |
| Strong negative (r < -0.5) | Metadata values decrease with PC scores |
| Weak (absolute r < 0.3) | Little linear relationship |

Categorical metadata is automatically converted to numeric (factor levels) for correlation computation. Significance stars indicate statistical reliability: *** p<0.001, ** p<0.01, * p<0.05.

**Use case**: If `SITE` correlates strongly with Dim.1, your samples separate primarily by location.

</details>

<details>
<summary>When should I enable normalization?</summary>

Enable **Normalize skewed variables** when:
- Skewness warning appears for specific columns
- Outliers are likely measurement errors (e.g., instrument malfunctions)
- Extreme values dominate the PCA solution
- Variables show heavy-tailed distributions

**Avoid normalization** when:
- Outliers represent real extreme cases (e.g., genuine ultra-rough surfaces)
- You need to preserve original measurement interpretability
- Effect sizes in original units are meaningful for your research

Normalization applies bestNormalize-selected transformations before scaling. The transformation parameters are saved in the RDS export for reproducibility.

</details>

<details>
<summary>Why do I get "singular matrix" errors?</summary>

Singular correlation matrices occur when variables are perfectly correlated or constant. Solutions:

1. **Remove redundant variables** — Delete one variable from each perfectly correlated pair (|r| = 1.0)
2. **Check for constant columns** — Remove columns where all values are identical
3. **Verify numeric data** — Ensure no text or factor columns were accidentally selected as measurements
4. **Reduce variable count** — If p ≥ n, remove variables until p < n

Use the **Correlation Matrix** plot to identify highly correlated pairs (> 0.95) before running PCA.

</details>

<details>
<summary>How do I interpret variable contributions?</summary>

Variable contributions indicate which original measurements define each principal component:

- **Sum of contributions** across all variables for a given dimension equals 100%
- **Average contribution** = 100% / (number of variables)
- **Values > average** indicate variables contributing above expectation to that component

**Practical interpretation**:
- High contribution + positive coordinate → Variable loads positively on this dimension
- High contribution + negative coordinate → Variable loads negatively
- Low contribution everywhere → Variable is redundant or noise

Use the **Variable Contributions** jitter plot to identify variables with consistent high contributions across multiple dimensions—these are your key discriminators.

</details>

<details>
<summary>Why are my individual contributions so uneven?</summary>

Uneven individual contributions (some points with very high %, most with low) typically indicate:

- **Outliers** — Extreme observations pull component directions toward them
- **Clusters** — Well-separated groups create high-contribution boundary points
- **Data errors** — Verify high-contribution individuals aren't measurement mistakes

High individual contributions (cos² near 1 on specific dimensions) warrant investigation. Check the **Individual Contributions** plot and cross-reference with metadata—consistent patterns may reveal meaningful subgroups.

</details>

<details>
<summary>What is the difference between contributions and cos²?</summary>

Both metrics assess quality but answer different questions:

| Metric | Question Answered | Range | Sum Across |
|--------|-------------------|-------|------------|
| **Contributions (%)** | How much does this item influence the component? | 0-100% | Components = varies |
| **Cos²** | How well is this item represented by the component? | 0-1 | Components = 1 (perfect representation) |

**Contributions** measure influence on the solution; high-contribution variables/individuals define where the component points. **Cos²** measures fit; high cos² means the component captures most of that item's variance.

A variable can have low contribution (little influence) but high cos² (well-represented) if it aligns with but doesn't drive the component direction.

</details>

<details>
<summary>Which R packages power the PCA computation?</summary>

| Package | Purpose | Citation |
|---------|---------|----------|
| **psych** | KMO measure and factor analysis utilities | Revelle, W. (2026). *psych: Procedures for Psychological, Psychometric, and Personality Research*. <https://CRAN.R-project.org/package=psych> |
| **ggiraph** | Interactive SVG graphics | Gohel, D., & Skintzos, P. (2026). *ggiraph: Make 'ggplot2' Graphics Interactive*. <https://doi.org/10.32614/CRAN.package.ggiraph> |
| **ggplot2** | Plot generation and styling | Wickham, H. (2016). *ggplot2: Elegant Graphics for Data Analysis*. Springer. <https://ggplot2.tidyverse.org> |
| **ggrepel** | Non-overlapping text labels | Slowikowski, K. (2026). *ggrepel: Automatically Position Non-Overlapping Text Labels with 'ggplot2'*. <https://doi.org/10.32614/CRAN.package.ggrepel> |
| **plotly** | 3D biplot rendering | Sievert, C. (2020). *Interactive Web-Based Data Visualization with R, plotly, and shiny*. Chapman and Hall/CRC. <https://plotly-r.com> |
| **scales** | Plot scales and colour utilities | Wickham, H., Pedersen, T. L., & Seidel, D. (2025). *scales: Scale Functions for Visualization*. <https://doi.org/10.32614/CRAN.package.scales> |

</details>
