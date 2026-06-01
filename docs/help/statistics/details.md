#### Statistical Approaches

Three testing frameworks are available. The choice should be guided by the properties of your data, **not** by which approach gives the most convenient result.

| Approach | Omnibus test | 1-Way Post-Hoc | 2/3-Way Post-Hoc | Effect size |
|---|---|---|---|---|
| **Robust** | Trimmed-means ANOVA (t*n*way) | Lincon (trimmed) | Lincon on combined groups | Cliff's Delta |
| **Parametric** | Classical ANOVA (F-test) | Tukey HSD | Tukey HSD on interaction factor | Cohen's *d* |
| **Non-Parametric** | Kruskal-Wallis (1-way) / ART (2/3-way) | Dunn's test or Pairwise Wilcoxon | ART-C contrasts | Cliff's Delta (1-way) / ART Cohen's *d* (2/3-way) |

---

<details>
<summary><strong>Robust Tests — Trimmed-Means ANOVA (default, recommended)</strong></summary>

Robust tests use **trimmed means** (a configurable percentage of extreme values is discarded before computing statistics, controlled by the **Trim** slider in the Plotting tab). The Welch-Yuen family of tests does not assume equal variances, making them robust to both non-normality and heteroscedasticity.

**When to use:** For most archaeological and biological morphometric datasets. Particularly appropriate when outliers exist but should not be masked by deletion, when group sizes are unequal, or when normality cannot be confirmed.

##### Omnibus: t1way / t2way / t3way

Implemented via `WRS2::t1way()`, `WRS2::t2way()`, `WRS2::t3way()`. Computes a heteroscedastic *F*-type (Q) statistic on trimmed group means using an approximate degrees-of-freedom correction.

**Omnibus output columns:**

<details>
<summary>1-Way — t1way</summary>

| Column | Description |
|---|---|
| `Effect` | The grouping factor |
| `F_statistic` | Welch-Yuen *F*-type statistic on trimmed means |
| `df1` | Numerator degrees of freedom |
| `df2` | Denominator degrees of freedom (Satterthwaite approximation) |
| `Effect_Size` | Explanatory measure of effect (*ξ*, equivalent to η² on trimmed data) |
| `p.value` | *p*-value of the omnibus test |

When bootstrap is enabled, numeric columns are reported as `mean [2.5% – 97.5%]` across all bootstrap iterations.

</details>

<details>
<summary>2-Way — t2way</summary>

Three rows are reported: main effect A, main effect B, and interaction A:B.

| Column | Description |
|---|---|
| `Effect` | Factor or interaction label |
| `Q` | Robust *F*-type (Q) statistic for this effect |
| `p.value` | *p*-value for this effect |

A significant interaction (A:B) means the effect of one factor depends on the level of the other. Interpret main effects cautiously when the interaction is significant.

</details>

<details>
<summary>3-Way — t3way</summary>

Seven rows: A, B, C, A:B, A:C, B:C, A:B:C — same column structure as 2-way. A significant three-way interaction (A:B:C) means the two-way interaction pattern itself changes across levels of the third factor.

</details>

##### Post-Hoc: Lincon + Cliff's Delta

**Lincon** (`WRS2::lincon()`) performs pairwise comparisons using trimmed means with no internal p-value adjustment (adjustment is applied downstream via the selected method). For multi-way designs, group combinations are collapsed to a single interaction factor before running `lincon`.

**Cliff's Delta** (`cidmulv2_labelled`) is a non-parametric rank-based effect size that does not assume normality.

**Post-hoc output columns (Lincon panel):**

| Column | Description |
|---|---|
| `Interaction` | Pair label, e.g., `GroupA vs. GroupB` |
| `psihat` | Trimmed mean difference (ψ̂). Positive = Group A > Group B |
| `ci.lower`, `ci.upper` | 95% confidence interval for ψ̂ |
| `p.value` | Raw (unadjusted) *p*-value |
| `p.adjusted` | *p*-value after applying the selected adjustment method |

**Post-hoc output columns (Cliff's Delta panel):**

| Column | Description |
|---|---|
| `psihat` | Cliff's δ estimate (range −1 to +1). 0 = no difference |
| `ci.lower`, `ci.upper` | 95% confidence interval for δ |
| `p.value` | Raw *p*-value |
| `p.adjusted` | Adjusted *p*-value |

**Cliff's Delta interpretation:**

| \|δ\| | Conventional label |
|---|---|
| < 0.147 | Negligible |
| 0.147–0.330 | Small |
| 0.330–0.474 | Medium |
| ≥ 0.474 | Large |

</details>

---

<details>
<summary><strong>Parametric Tests — Classical ANOVA</strong></summary>

Classical ANOVA (`stats::aov()`) partitions total variance into between-group and within-group components and tests whether group means differ using an *F*-ratio. The *F*-ratio compares the mean square of the effect to the residual mean square.

**Assumptions (must be verified before use):**
- Observations are independent
- Residuals are normally distributed (check with Shapiro-Wilk in the **Summary** tab)
- Homogeneity of variance across groups (Levene's test recommended)

**When to use:** Only when assumptions are satisfied. For large, balanced samples (n ≥ 30 per group) ANOVA is robust to mild normality violations, but variance heterogeneity remains problematic.

##### Omnibus: 1/2/3-Way ANOVA

| Column | Description |
|---|---|
| `Effect` | Factor or interaction label |
| `Df` | Degrees of freedom for this effect |
| `SS` | Sum of squares — variance attributed to this effect |
| `MS` | Mean square (SS / Df) |
| `F.Statistic` | F-ratio (MS_effect / MS_residual). Larger = stronger effect relative to residual variance |
| `p.value` | Probability of observing F this large under H₀ |

2-way and 3-way designs report main effects and all interactions, same structure as robust. The interaction row (A:B or A:B:C) answers whether effects are additive or synergistic/antagonistic.

##### Post-Hoc: Tukey HSD + Cohen's *d*

**Tukey HSD** (`stats::TukeyHSD()`) tests all pairwise mean differences simultaneously, controlling the family-wise error rate. For multi-way designs, groups are combined into a single interaction factor.

**Cohen's *d*** is computed from pooled standard deviations (pooled SD formula).

**Post-hoc output columns (Tukey HSD panel):**

| Column | Description |
|---|---|
| `Interaction` | Pair label |
| `diff` | Estimated mean difference |
| `ci.lower`, `ci.upper` | 95% confidence interval (Tukey method) |
| `p.value` | Raw *p*-value |
| `p.adjusted` | Adjusted *p*-value |

**Post-hoc output columns (Cohen's *d* panel):**

| Column | Description |
|---|---|
| `d` | Cohen's *d* (mean difference / pooled SD). Positive = Group A > Group B |
| `ci.lower`, `ci.upper` | 95% CI for *d* (normal approximation) |
| `p.value` | Raw *p*-value from independent *t*-test |
| `p.adjusted` | Adjusted *p*-value |

**Cohen's *d* interpretation:**

| \|*d*\| | Conventional label |
|---|---|
| < 0.2 | Negligible |
| 0.2–0.5 | Small |
| 0.5–0.8 | Medium |
| ≥ 0.8 | Large |

</details>

---

<details>
<summary><strong>Non-Parametric Tests — Kruskal-Wallis / ART</strong></summary>

Non-parametric tests make no distributional assumptions; they operate on ranks rather than raw values.

**When to use:** Ordinal measurement scales, severe non-normality with small samples, or when the robust approach is not applicable for theoretical reasons.

##### Omnibus: Kruskal-Wallis (1-Way)

`stats::kruskal.test()` tests whether samples originate from the same distribution by comparing rank sums across groups.

| Column | Description |
|---|---|
| `Effect` | Grouping factor |
| `Df` | Degrees of freedom (number of groups − 1) |
| `H.Statistic` | Kruskal-Wallis *H* statistic (chi-squared approximation). Larger = greater rank dispersion between groups |
| `p.value` | *p*-value of the test |

##### Omnibus: Aligned Rank Transform — ART (2/3-Way)

`ARTool::art()` + `anova()` applies a separate rank transformation for each effect, enabling non-parametric factorial designs. Requires a balanced design with **≥ 3 observations per cell**.

| Column | Description |
|---|---|
| `Effect` | Factor or interaction label |
| `Df` | Numerator degrees of freedom |
| `Df.res` | Residual degrees of freedom |
| `F.Statistic` | *F*-statistic from the ART model |
| `p.value` | *p*-value |

##### Post-Hoc: Dunn's Test (1-Way, default)

`dunn.test::dunn.test()` uses rank sums from the Kruskal-Wallis procedure. It is the standard post-hoc follow-up to the Kruskal-Wallis test.

| Column | Description |
|---|---|
| `Interaction` | Pair label |
| `Z` | Standardised test statistic |
| `p.value` | Raw *p*-value |
| `p.adjusted` | Adjusted *p*-value |

Paired with Cliff's Delta (same output columns as described in the Robust section above).

##### Post-Hoc: Pairwise Wilcoxon (1-Way, alternative)

`stats::pairwise.wilcox.test()` conducts independent Wilcoxon rank-sum tests for each pair. More conservative than Dunn's test because it does not share rank information across all groups.

| Column | Description |
|---|---|
| `Interaction` | Pair label |
| `p.value` | Raw *p*-value |
| `p.adjusted` | Adjusted *p*-value |

Paired with Cliff's Delta.

##### Post-Hoc: ART Contrasts (2/3-Way)

`ARTool::art.con()` via `emmeans` computes pairwise contrasts on the ART interaction model. Cohen's *d* is derived from ART estimates using `sigma_hat` from `artlm.con()`. Results are shown as two side-by-side panels in the UI.

**ART Contrasts panel** (left):

| Column | Description |
|---|---|
| `Interaction` | Pair label (e.g., `A.C vs. A.D`) |
| `estimate` | Contrast estimate on the aligned-rank scale |
| `SE` | Standard error of the estimate |
| `df` | Degrees of freedom |
| `t.ratio` | *t*-statistic (estimate / SE) |
| `p.value` | Raw *p*-value |
| `p.adjusted` | Adjusted *p*-value |

**ART Cohen's d panel** (right):

| Column | Description |
|---|---|
| `d` | Cohen's *d* derived from ART (estimate / σ̂) |
| `ci.lower` | Lower bound of 95% CI for *d* |
| `ci.upper` | Upper bound of 95% CI for *d* |

Interpret ART Cohen's *d* using the same thresholds as classical Cohen's *d* (see Parametric section).

</details>

---

#### P-Value Adjustment Methods

Multiple pairwise comparisons inflate the probability of false positives (Type I error). The selected adjustment method is applied to all raw post-hoc *p*-values simultaneously **after** any valid-comparisons filtering.

| Method | Family-wise error control | Notes |
|---|---|---|
| **Bonferroni** | Controls FWER | Most conservative. Multiply each *p* by the number of comparisons. Use when false positives carry high cost. |
| **Holm** | Controls FWER | Step-down procedure; uniformly more powerful than Bonferroni. Recommended default when FWER control is needed. |
| **Hochberg** | Controls FWER | Step-up procedure; slightly more powerful than Holm under independence. |
| **Hommel** | Controls FWER | Most powerful FWER method; assumes positive dependence. |
| **BH (Benjamini-Hochberg)** | Controls FDR | Controls the false discovery rate — acceptable proportion of false positives among all discoveries. Suitable for exploratory analyses with many comparisons. |
| **BY (Benjamini-Yekutieli)** | Controls FDR | FDR control under arbitrary dependence structures. More conservative than BH. |
| **FDR** | Controls FDR | Alias for BH in R (`stats::p.adjust`). |
| **None** | No correction | Raw *p*-values only. Use only for internal diagnostics; do not report uncorrected values in publications. |

**Choosing a method:** Use **Holm** or **Bonferroni** when a single false positive would be problematic (e.g., confirmatory analysis). Use **BH** when the goal is to identify candidate differences in a large comparison set (exploratory analysis). Always report the adjustment method used.

---

#### Bootstrap

When enabled, the robust omnibus and Lincon post-hoc computations are repeated across `B` bootstrap samples drawn per group. Results are summarised as `mean [2.5% – 97.5%]` (bootstrap mean and percentile confidence interval). Bootstrap is not available for parametric or non-parametric approaches.

**Recommended settings:**
- **Bootstrap samples:** 599 (default). Values > 599 rarely alter conclusions.
- **Samples per bootstrap:** Leave blank to default to the smallest group size. Capping prevents biasing toward large groups.

**Use bootstrap when:** group sizes are < 10, groups are severely unequal, or data are heavily skewed.

---

#### Output Metrics — Quick Reference

<details>
<summary><strong>Omnibus table columns by approach</strong></summary>

| Approach | Statistic | What it measures |
|---|---|---|
| Robust | *Q* / *F* | Variation in trimmed group means relative to within-group spread |
| Robust | *df1*, *df2* | Numerator and denominator DF (Satterthwaite) |
| Robust | Effect_Size (ξ) | Proportion of variance explained on trimmed scale |
| Parametric | *F* | Ratio of between-group to within-group mean squares |
| Parametric | SS | Raw variance attributed to each effect |
| Parametric | MS | SS normalised by degrees of freedom |
| Non-Param (1-way) | *H* | Rank-sum dispersion across groups |
| Non-Param (2/3-way) | *F* (ART) | *F*-ratio on aligned ranks |

</details>

<details>
<summary><strong>Post-hoc table columns by approach</strong></summary>

| Column | Approach | Meaning |
|---|---|---|
| `psihat` | Robust (Lincon) | Trimmed mean difference between the two groups |
| `ci.lower` / `ci.upper` | All | 95% confidence interval. If CI excludes 0, difference is significant at α = 0.05 |
| `p.value` | All | Raw (unadjusted) *p*-value |
| `p.adjusted` | All | *p*-value after the selected correction method |
| `diff` | Parametric (Tukey) | Arithmetic mean difference |
| `d` | Parametric | Cohen's *d* — standardised mean difference |
| `Z` | Non-Param 1-way (Dunn) | Standardised Dunn test statistic |
| `p.value` / `p.adjusted` | Non-Param 1-way (Wilcoxon) | Raw and adjusted *p* from Wilcoxon rank-sum test |
| `Cliff.psihat` (δ) | Robust / Non-Param 1-way | Rank-based effect size (−1 to +1) |
| `estimate` | Non-Param 2/3-way (ART Contrasts panel) | Contrast estimate on aligned-rank scale |
| `d` | Non-Param 2/3-way (ART Cohen's d panel) | Cohen's *d* derived from ART model |

</details>

See the **FAQ** tab for troubleshooting common errors and edge cases.
