#### Frequently Asked Questions

<details>
<summary>What does the "Apply recommended parameters" banner do?</summary>

It appears when the **Statistics** tab has computed a parameter screening ranking (enabled via the Plotting tab's **Disable plots (parameter screening mode)** checkbox, followed by **Compute Statistics**). Clicking it replaces the current measurement-column selection with the parameters that ranked among the top group separators. You can still add or remove columns manually afterward — the applied selection is not locked.

There is no functional difference between recommended and manually selected parameters: both flow through the same `measureVar` selection used by the PCA computation and by the saved bundle. If the banner does not appear, either screening mode has not been used yet, or no parameters were ranked (e.g., the Statistics results have not been computed).

</details>

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

Categorical metadata with 3+ unordered levels is converted to an arbitrary 1/2/3… numeric code before computing r. A **low** correlation against such a column is a weaker "no effect" signal than the same low r against an ordered or numeric variable — the arbitrary level ordering can hide a real group effect. Treat a **high** r as a strong signal regardless of column type; treat a low r on a multi-level unordered categorical column with more caution.

</details>

<details>
<summary>When should I residualize by a metadata column?</summary>

Use the **Residualize by** dropdown (Data Selection sidebar, below the measurement columns) when a metadata variable you are not directly interested in — a confound such as `SITE`, batch, or collection date — is suspected to dominate the measurements and mask the signal you actually care about.

**How to decide which column**: run PCA once without residualizing, then check the **Eigencorrelation** plot (see above) for which metadata column correlates most strongly with the top components. A high correlation there is the signal to residualize by; remember that a low correlation against a multi-level unordered categorical column is weaker evidence of "no effect" than it looks.

**Ordering**: residualizing runs before scaling, on the original measurement units — it does not replace Scale & Center. Group-mean subtraction removes location shifts only; Scale & Center still standardizes variance afterward and should generally stay enabled.

**What it cannot do**: it does not fix unequal variance between measurement columns (units in mm vs. percentages still need Scale & Center), and it cannot separate a confound from your variable of interest if the two are perfectly correlated in your sample — for example, if a species was only ever collected at one site, no adjustment can tell species and site apart.

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

For **sPCA**, a related but distinct error — "keepX invalid or incomplete" — means the number of keepX values does not match the number of components, or one is missing. Use the dynamically generated keepX input boxes in the Analysis Settings tab (one per component) rather than editing the count separately.

</details>

<details>
<summary>How do I interpret variable contributions?</summary>

Not applicable to IPCA — see "Why don't IPCA results show Contribution % or Cos²?" above. For PCA and sPCA, variable contributions indicate which original measurements define each principal component:

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

Not applicable to IPCA — see "Why don't IPCA results show Contribution % or Cos²?" above. For PCA and sPCA, uneven individual contributions (some points with very high %, most with low) typically indicate:

- **Outliers** — Extreme observations pull component directions toward them
- **Clusters** — Well-separated groups create high-contribution boundary points
- **Data errors** — Verify high-contribution individuals aren't measurement mistakes

High individual contributions (cos² near 1 on specific dimensions) warrant investigation. Check the **Individual Contributions** plot and cross-reference with metadata—consistent patterns may reveal meaningful subgroups.

</details>

<details>
<summary>What is the difference between contributions and cos²?</summary>

Applies to PCA and sPCA only — see "Why don't IPCA results show Contribution % or Cos²?" above. Both metrics assess quality but answer different questions:

| Metric | Question Answered | Range | Sum Across |
|--------|-------------------|-------|------------|
| **Contributions (%)** | How much does this item influence the component? | 0-100% | Components = varies |
| **Cos²** | How well is this item represented by the component? | 0-1 | Components = 1 (perfect representation) |

**Contributions** measure influence on the solution; high-contribution variables/individuals define where the component points. **Cos²** measures fit; high cos² means the component captures most of that item's variance.

A variable can have low contribution (little influence) but high cos² (well-represented) if it aligns with but doesn't drive the component direction.

</details>

<details>
<summary>Should I use PCA, sPCA, or IPCA?</summary>

Start with standard **PCA** — it is the default, well-understood, and sufficient for most dimensionality-reduction and visualization needs.

Switch to **sPCA** when you specifically need to know which subset of your variables defines each component — for example, 40+ computed surface-texture parameters where you want to report the handful that actually matter per axis. sPCA trades a small amount of variance explained for a short, interpretable variable list per component (the **keepX** setting). Always run **Optimise variable selection** before reporting the list, so the count is chosen by cross-validation rather than guessed.

Switch to **IPCA** when your research question is about statistically *independent* sources of variation rather than variance-ranked ones — for example, testing whether two or more distinct underlying processes (rather than a smooth gradient) generated your measurements. This is a different analytical question from PCA/sPCA, not simply a "better" version of them: IPCA components are not ordered by importance, and contribution/cos² are not available for them.

If you are unsure, compute standard PCA first, inspect the biplot and variable contributions, and only switch methods once you have a specific reason (an unmanageable variable list, or a hypothesis about independent sources) that PCA cannot address.

</details>

<details>
<summary>Why don't IPCA results show Contribution % or Cos²?</summary>

Contribution % and cos² (quality of representation) are derived from having components ranked by variance explained — "how much of this component's variance does this variable/individual account for?" IPCA components are not variance-ranked; ICA optimizes for statistical independence, and component order reflects the algorithm's convergence, not a hierarchy of importance. Reporting a "contribution to component 1" would imply component 1 is somehow the most important, which is not a meaningful claim for IPCA.

Loadings and scores are still available and interpretable — the biplot, results tables, and Excel export all work for IPCA, just without the contribution/cos² columns/plots that PCA and sPCA provide.

</details>

<details>
<summary>What should I report in a paper or thesis?</summary>

Report enough that a reader can judge the result without re-running it. The minimum is **what you ran**, **how much variance the shown components capture** (or, for IPCA, how they were extracted), and **which variables drove the structure you interpret**.

**Always report these numbers**

| What | Where to find it | Why it is needed |
|------|------------------|------------------|
| Method and software | — | e.g. "sPCA (mixOmics 6.x, R 4.x)" — see the package table below for citations |
| n observations, number of variables | PCA Results panel / Summary | Reviewers need the sample-size-to-variable ratio to judge overfitting/stability risk |
| Preprocessing | Data Selection sidebar | Scaling/centring method, normalization, how missing values were handled (and grouping column, if residualized) |
| Variance explained by the components shown | Eigenvalues & Variance table | Readers cannot judge a 2D projection's adequacy without this |
| Number of components retained and why | Optimal Number of Components panel | State whether Kaiser, Elbow, or Parallel Analysis (or a combination) justified the count |

**Add these depending on method**

- **sPCA** — **keepX per component** and **whether it was tuned** (results are marked "not tuned" otherwise); the Selected Variables list for each component you interpret
- **IPCA** — the ICA algorithm (**Deflation** or **Parallel**); explicitly state that components are not variance-ranked, so readers do not assume Dim.1 is "the most important" the way they would for PCA

**Which plots to show**

1. **The biplot** — state which dimensions are shown and their variance explained (from the axis labels); for IPCA, state that axis order is arbitrary
2. **The Variable Contributions plot**, when "which parameters matter" is the question (PCA/sPCA only — not applicable to IPCA)
3. **The Selected Variables list**, as a table rather than a figure, for sPCA

**The two most common mistakes**

- **Treating an untuned sPCA keepX as a considered result.** A hand-picked keepX has not been validated to be the right sparsity level — always run **Optimise variable selection** and report that it was used.
- **Applying PCA intuitions to IPCA.** Describing "Dim.1" as the most important IPCA component, or reporting cumulative variance as if it were a meaningful stopping rule, misrepresents what ICA does. State plainly that IPCA components are unordered.

A defensible one-paragraph summary follows this shape (placeholders in `CAPITALS` — substitute your own values):

> We performed METHOD (mixOmics R package) on N observations across P measurement variables, after SCALING_METHOD. NCOMP components were retained based on CRITERION, together explaining CUM_VARIANCE% of total variance. [For sPCA: Sparse variable selection (keepX = KEEPX_VALUES per component, tuned by REPEATS repeats of FOLDS-fold cross-validation over the grid GRID using `mixOmics::tune.spca()`) identified VARIABLE_LIST as the primary drivers of DIM_NAME.] [For IPCA: Components were extracted via ICA_ALGORITHM ICA and are not ranked by variance explained.]

Every placeholder above corresponds to a number the app reports — none of them should be estimated or omitted.

</details>

<details>
<summary>How do I justify my keepX in a paper?</summary>

"We kept 10 variables per component" invites the obvious question: *why ten?* If the answer is "it seemed reasonable," a reviewer is right to be sceptical — the variable list is the main result of an sPCA, and an arbitrary sparsity level makes that list arbitrary too.

Run **Optimise variable selection** and report the tuning, not just the outcome. Three things make the choice defensible:

1. **The value and how it was chosen** — the keepX per component, plus the method: cross-validation maximising the correlation between the cross-validated and full-data component (`mixOmics::tune.spca()`).
2. **The cross-validation settings** — folds, repeats and the candidate grid. These are stated verbatim beneath the **keepX Tuning Evidence** plot, in a form you can paste into a methods section. The grid matters especially: only values in it could have been chosen, so a reader needs to know what was on the table.
3. **The stability actually achieved** — the correlation value at the chosen keepX, from the tuning plot. A selection tuned to a correlation of 0.95 is a much stronger claim than one tuned to 0.55, and reporting only "keepX was tuned" hides that difference.

Two honest caveats worth including when they apply:

- If the stability curve **plateaus** well before the selected value, say so and consider reporting the smaller keepX instead — a shorter variable list at equivalent stability is a better result, and choosing it deliberately is more defensible than accepting the automatic pick without comment.
- If the correlation is **low or erratic** across the whole grid, the variable selection is not reproducible on your sample size. Report the list as provisional rather than as a finding. This is a limitation of the data, not a failure of the analysis, and stating it plainly is far safer than having a reviewer infer it.

Do not tune keepX, dislike the resulting variable list, and then hand-pick a different value without saying so. If you override the tuned value for a practical reason — needing a shorter list for a follow-up assay, say — state the tuned value alongside the one you used and give the reason.

</details>

<details>
<summary>Which R packages power the PCA computation?</summary>

| Package | Purpose | Citation |
|---------|---------|----------|
| **mixOmics** | PCA, sPCA, and IPCA computation | Rohart, F., Gautier, B., Singh, A., & Lê Cao, K.-A. (2017). *mixOmics: An R package for 'omics feature selection and multiple data integration*. *PLOS Computational Biology*, 13(11), e1005752. <https://doi.org/10.1371/journal.pcbi.1005752> |
| **psych** | KMO measure and factor analysis utilities | Revelle, W. (2026). *psych: Procedures for Psychological, Psychometric, and Personality Research*. <https://CRAN.R-project.org/package=psych> |
| **ggiraph** | Interactive SVG graphics | Gohel, D., & Skintzos, P. (2026). *ggiraph: Make 'ggplot2' Graphics Interactive*. <https://doi.org/10.32614/CRAN.package.ggiraph> |
| **ggplot2** | Plot generation and styling | Wickham, H. (2016). *ggplot2: Elegant Graphics for Data Analysis*. Springer. <https://ggplot2.tidyverse.org> |
| **ggrepel** | Non-overlapping text labels | Slowikowski, K. (2026). *ggrepel: Automatically Position Non-Overlapping Text Labels with 'ggplot2'*. <https://doi.org/10.32614/CRAN.package.ggrepel> |
| **plotly** | 3D biplot rendering | Sievert, C. (2020). *Interactive Web-Based Data Visualization with R, plotly, and shiny*. Chapman and Hall/CRC. <https://plotly-r.com> |
| **scales** | Plot scales and colour utilities | Wickham, H., Pedersen, T. L., & Seidel, D. (2025). *scales: Scale Functions for Visualization*. <https://doi.org/10.32614/CRAN.package.scales> |

</details>
