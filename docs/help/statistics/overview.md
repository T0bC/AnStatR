#### Statistics

Run omnibus tests and pairwise post-hoc comparisons for one-, two-, or three-way factorial designs. Data selection, outlier filtering, and trimming are inherited from the **Plotting** tab. Click **Compute Statistics** to run the analysis.

##### Statistical Approach

Select the testing framework in the **Options** sidebar tab (gear icon):

- **Robust Tests** *(default)* — Trimmed-means ANOVA (`WRS2::t1way` / `t2way` / `t3way`). Best choice for most real-world data; handles outliers and non-normal distributions gracefully.
- **Parametric Tests** — Classical ANOVA (`stats::aov`). Appropriate only when normality and equal-variance assumptions are met.
- **Non-Parametric Tests** — Kruskal-Wallis (1-way) or Aligned Rank Transform / ART (2/3-way). Use for ordinal data or when parametric assumptions are clearly violated.

For 1-way non-parametric designs, an additional radio button lets you choose between **Dunn's Test** and **Pairwise Wilcoxon** as the post-hoc method.

See the **Details** tab for a full explanation of each approach, including which R packages are used.

##### P-Value Adjustment

Configure multiple-comparison correction in the **P-Value Adjustment** sidebar tab (sliders icon). The default is **Bonferroni**. See the **Details** tab for guidance on when to use each method.

##### Bootstrap Options

Enable the **Bootstrap** sidebar tab (arrows icon) for better p-value approximations when group sizes are very small or highly unequal. Set the number of iterations (default 599) and optionally fix the per-group sample size.

##### Additional Options

- **Additional Output** — Show all numeric columns (Linear Contrasts / Cliff's Delta tables) in the post-hoc panels.
- **Show only significant p-values** — Filter post-hoc rows to adjusted *p* < 0.07.
- **Only valid comparisons** *(multi-way designs only)* — Retain only pairs differing by exactly one factor level.

##### Output

After clicking **Compute Statistics**, one result card appears per measurement column. Each card contains:

- **The plot** from the Plotting tab (snapshot captured at compute time)
- **Omnibus table** — Overall test result for each effect (main effects + interactions where applicable)
- **Pairwise Comparisons** — Two side-by-side tables: the pairwise test (Lincon / Tukey / Dunn / Wilcoxon / ART) and the effect size metric (Cliff's Delta / Cohen's *d* / ART Cohen's *d*)

###### 1-Way Design

One omnibus row reporting a single test statistic and *p*-value for the grouping factor. Post-hoc shows all pairwise group comparisons.

###### 2-Way Design

Three omnibus rows: main effect A, main effect B, and interaction A:B. Post-hoc covers all pairwise combinations across the full interaction grid. See **Details** for metric definitions.

###### 3-Way Design

Seven omnibus rows: three main effects (A, B, C), three two-way interactions (A:B, A:C, B:C), and one three-way interaction (A:B:C). Post-hoc covers pairwise combinations across the full factorial grid.

##### HTML Report Download

Click the **download icon** (↓) in any card header to export a self-contained HTML report for that measurement, including the plot, omnibus table, and post-hoc results.

##### Parameter Screening — Separation Ranking

When the Plotting tab's **Disable plots (parameter screening mode)** checkbox is enabled, an additional **Parameter Screening — Separation Ranking** card appears above the per-measurement results after clicking **Compute Statistics**. It ranks every measurement parameter by how well it separates groups across all pairwise comparisons, using the p-values and effect sizes already computed in the post-hoc step — no extra tests are run.

The top-ranked parameters become the **recommended parameter set**, which can be applied with one click in the **PCA**, **LDA**, and **Cluster** tabs to reduce a large parameter list to a statistically motivated subset. See the **Details** tab for the ranking method.
