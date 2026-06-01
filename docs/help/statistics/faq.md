#### FAQ — Statistics

<details>
<summary>Which statistical approach should I choose?</summary>

Start with **Robust Tests** (the default). Robust trimmed-means ANOVA (`WRS2::t1way` / `t2way` / `t3way`) is appropriate for virtually all morphometric and bioarchaeological datasets because it tolerates non-normality, outliers, and variance heterogeneity without requiring those values to be deleted.

Switch to **Parametric Tests** only if you have confirmed normality (e.g., Shapiro-Wilk p > 0.05 in the **Summary** tab) and comparable variances across all groups — and your sample sizes are reasonably large (n ≥ 15–20 per group).

Use **Non-Parametric Tests** for ordinal scales or when parametric assumptions are severely violated and robust trimming would remove too much information (e.g., very small *n* with extreme skew).

</details>

<details>
<summary>Why is the Robust approach recommended over Parametric even for "normal-looking" data?</summary>

Trimmed-means ANOVA is not a fallback for bad data — it is theoretically better suited in the presence of heavy tails, mild non-normality, or unequal sample sizes, which are all common in real-world morphometric datasets. The trimming percentage is controlled by the **Trim** slider in the Plotting tab. At 0% trim, robust tests converge toward the parametric result. At any trim > 0%, they are more powerful than classical ANOVA when the underlying distributions have heavier-than-normal tails.

Classical ANOVA assumes equal variances (homoscedasticity). Even modest violations can inflate Type I error rates, particularly in unbalanced designs. Robust tests use a heteroscedastic approximation (Satterthwaite degrees of freedom) and are not affected by this.

</details>

<details>
<summary>What does a significant omnibus p-value actually mean?</summary>

A significant omnibus test (p < your chosen α, typically 0.05) indicates that **at least one group or combination differs** from the others — it does not tell you which pair is different or by how much. Always interpret the omnibus result together with the post-hoc pairwise comparisons and effect sizes.

For 2-way and 3-way designs, inspect each effect row (main effects and interactions) separately. A significant interaction (e.g., A:B) means the effect of factor A depends on the level of factor B. When an interaction is significant, main effects become difficult to interpret in isolation and should be discussed with caution.

</details>

<details>
<summary>How do I interpret the Q statistic in Robust ANOVA results?</summary>

The Q statistic (also labelled as the F-type statistic) in `t2way` and `t3way` output is the Welch-Yuen heteroscedastic statistic computed on trimmed group means. It is analogous to the classical F-ratio but does not require equal variances. Larger Q values indicate greater divergence between the trimmed group means relative to within-group variation. The associated p-value is derived from an F-distribution with Satterthwaite-corrected degrees of freedom.

For `t1way`, the statistic is named `F_statistic` and carries an effect size measure ξ (xi), which approximates the proportion of variance explained on the trimmed scale.

</details>

<details>
<summary>What is Cliff's Delta and why is it paired with Robust / Non-Parametric tests?</summary>

Cliff's Delta (δ) is a non-parametric rank-based effect size. For each pair of observations across two groups, it counts how often a value from Group A exceeds a value from Group B (minus the reverse), divided by the total number of pairs. It ranges from −1 (all values in Group B exceed Group A) through 0 (no systematic difference) to +1 (all values in Group A exceed Group B).

Unlike Cohen's *d*, Cliff's Delta does not assume normality or equal variances, making it the natural companion to robust and non-parametric test frameworks. The conventional thresholds are |δ| < 0.147 negligible, 0.147–0.330 small, 0.330–0.474 medium, and ≥ 0.474 large.

Always check both the p-value (adjusted) **and** δ: a statistically significant result with a negligible δ is unlikely to be practically meaningful.

</details>

<details>
<summary>What does psihat (ψ̂) mean in the Lincon post-hoc table?</summary>

`psihat` (ψ̂) is the estimated trimmed mean difference between two groups: the trimmed mean of Group A minus the trimmed mean of Group B. A positive value means Group A has a larger trimmed mean. The 95% confidence interval (`ci.lower` / `ci.upper`) provides the uncertainty range for this estimate. If the CI excludes zero, the difference is significant at the corresponding α level.

When bootstrap is enabled, `psihat` is reported as `mean [2.5% – 97.5%]` summarising the bootstrap distribution of the estimate.

</details>

<details>
<summary>When should I enable Bootstrap?</summary>

Bootstrap resampling is beneficial when:

- Any group has fewer than ~10 observations
- Group sizes are highly unequal (ratio > 3:1)
- Data are heavily skewed even after trimming

Under these conditions, the asymptotic approximation used by the standard tests may not hold, and bootstrap confidence intervals are more reliable. Bootstrap adds computation time proportional to the number of iterations (default 599). Values above 599 rarely change the conclusion but significantly increase runtime.

Bootstrap is only available for Robust tests. Parametric and non-parametric approaches do not support it.

</details>

<details>
<summary>What do the p.value and p.adjusted columns mean, and which should I report?</summary>

`p.value` is the raw (unadjusted) p-value from the individual pairwise test. `p.adjusted` is the corrected value after applying the selected multiple-comparison adjustment method to the full set of comparisons.

**Always report `p.adjusted`** in scientific contexts. Reporting raw p-values without adjustment after conducting multiple pairwise comparisons artificially inflates the probability of false positives (Type I error). The adjustment method used must be stated in the methods section of any manuscript.

The **Show only significant p-values** option filters on `p.adjusted < 0.07` (a slightly lenient threshold to surface borderline results). This is a display filter only — it does not change the underlying values.

</details>

<details>
<summary>Which p-value adjustment method should I use?</summary>

- **Holm** — The recommended default for most analyses. It is uniformly more powerful than Bonferroni while still controlling the family-wise error rate (FWER). Use for confirmatory analyses where false positives carry high cost.
- **Bonferroni** — Most conservative. Appropriate when the number of comparisons is small (< 10) and the cost of any false positive is very high.
- **BH (Benjamini-Hochberg)** — Preferred for exploratory analyses with many comparisons (> 20 pairs). Controls the false discovery rate (FDR) rather than FWER, allowing more discoveries at the cost of tolerating a controlled proportion of false positives.
- **None** — Raw p-values only. Never use for reporting; acceptable only during internal diagnostics.

When in doubt: use **Holm** for confirmatory testing, **BH** for exploratory screening.

</details>

<details>
<summary>The ART test fails with a cell size error — what does this mean?</summary>

The Aligned Rank Transform (ART) requires a **balanced design** with at least **3 observations per cell** (each unique combination of factor levels). If any cell has fewer than 3 valid observations, the test cannot be run and an error is shown.

To resolve this:

- Check the **Summary** tab to inspect group sizes across all factor combinations.
- If some cells are empty or very small, consider merging factor levels or removing sparse groups in the raw data.
- Alternatively, switch to **Robust Tests**, which tolerate unequal and small group sizes via the Satterthwaite correction and optional bootstrap.

Missing values (NAs) are dropped before the balance check, so even groups that appear large in the raw data can fail if many values are missing for a specific measurement.

</details>

<details>
<summary>What does "Only valid comparisons" mean in multi-way designs?</summary>

In a 2- or 3-way factorial design, the post-hoc table includes pairwise comparisons across **all** group combinations. Many of these compare groups that differ in more than one factor simultaneously (e.g., `A1.B1 vs. A2.B2`). These "diagonal" comparisons are difficult to attribute to any single factor and are often not interpretable.

Enabling **Only valid comparisons** retains only pairs where groups differ by **exactly one factor level** (e.g., `A1.B1 vs. A2.B1`). This makes the post-hoc table cleaner and directly interpretable as simple effects of individual factors. P-value adjustment is applied **after** this filter, reducing the adjustment penalty.

Disable this option if you specifically need to examine cross-factor comparisons.

</details>

<details>
<summary>Why are some post-hoc rows missing after filtering?</summary>

Two filters can reduce the post-hoc table:

1. **Only valid comparisons** — removes multi-factor-differing pairs (see above).
2. **Show only significant p-values** — removes rows with `p.adjusted ≥ 0.07`.

If all rows are removed by the significance filter, the table shows "No significant pairwise comparisons found." This is a valid outcome — it means no pairwise difference survives the multiple-comparison correction at the threshold used. Disable the filter to inspect all comparisons.

</details>

<details>
<summary>The omnibus test is significant but no post-hoc comparisons are significant — why?</summary>

This can happen for several reasons:

- **Power loss from adjustment:** Post-hoc tests adjust for multiple comparisons, which lowers the per-comparison significance threshold. The omnibus test has only one comparison and is therefore more powerful.
- **The effect is diffuse:** Multiple small differences collectively drive the omnibus result, but no single pair survives correction.
- **Small group sizes:** With n < 5 per group, pairwise tests have low power even when the omnibus is significant.

In this situation, report the significant omnibus result, note that no individual pairwise comparison survived adjustment, and consider reporting effect sizes (Cliff's δ or Cohen's *d*) and confidence intervals for the most theoretically relevant pairs.

</details>

<details>
<summary>The computation button says plots must be generated first — what do I do?</summary>

The Statistics module inherits its data selection, filtering, outlier exclusions, trimming, and plot objects directly from the **Plotting** tab. Before computing statistics:

1. Switch to the **Plotting** tab.
2. Select your X-axis grouping columns and the measurement columns to analyse.
3. Confirm the plots are rendered in the Plotting main panel.
4. Return to the **Statistics** tab and click **Compute Statistics**.

If you change the Plotting configuration after computing, the statistics results remain frozen to the state at compute time (indicated by the timestamp in the results header). Click **Compute Statistics** again to update.

</details>

<details>
<summary>How do I download a report for a specific measurement?</summary>

Each result card in the main panel has a **download icon** (↓) in the card header, next to the *n*-way badge. Clicking it downloads a self-contained HTML file containing the plot (as an interactive SVG), the omnibus table, and the post-hoc pairwise comparisons. The filename includes the measurement name and a timestamp.

The report captures the state at compute time, including all parameter settings (approach, adjustment method, bootstrap status). Use this file for archiving, sharing, or embedding in reports.

</details>

<details>
<summary>Which R packages power the Statistics module?</summary>

| Package | Purpose | Citation |
|---------|---------|----------|
| **ARTool** | Aligned Rank Transform (ART) factorial ANOVA | Kay, M., Elkin, L. A., Higgins, J. J., & Wobbrock, J. O. (2025). *ARTool: Aligned Rank Transform for Nonparametric Factorial ANOVAs*. <https://doi.org/10.5281/zenodo.594511> |
| **base64enc** | Base64 encoding for exports | Urbanek, S. (2026). *base64enc: Tools for 'base64' Encoding*. <https://doi.org/10.32614/CRAN.package.base64enc> |
| **dplyr** | Data manipulation in post-hoc pipeline | Wickham, H., Francois, R., Henry, L., Muller, K., & Vaughan, D. (2026). *dplyr: A Grammar of Data Manipulation*. <https://doi.org/10.32614/CRAN.package.dplyr> |
| **dunn.test** | Dunn's test post-hoc | Dinno, A. (2026). *dunn.test: Dunn's Test of Multiple Comparisons Using Rank Sums*. <https://doi.org/10.32614/CRAN.package.dunn.test> |
| **ggplot2** | Plot generation | Wickham, H. (2016). *ggplot2: Elegant Graphics for Data Analysis*. Springer. <https://ggplot2.tidyverse.org> |
| **htmltools** | HTML generation for reports | Cheng, J., Sievert, C., Schloerke, B., Chang, W., Xie, Y., & Allen, J. (2025). *htmltools: Tools for HTML*. <https://doi.org/10.32614/CRAN.package.htmltools> |
| **WRS2** | Robust trimmed-means ANOVA and contrasts | Mair, P., & Wilcox, R. (2020). Robust statistical methods in R using the WRS2 package. *Behavior Research Methods*, 52, 464-488. <https://doi.org/10.3758/s13428-019-01246-w> |

</details>
