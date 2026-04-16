#### Requirements

**Study Design Setup**

Configure factorial structure in the **Study Design** sidebar tab:

| Design | Factors | Total Groups | Use Case |
|---|---|---|---|
| 1-way | 1 factor | *k* levels | Single treatment or grouping variable |
| 2-way | 2 factors | *a* × *b* | Two treatments or a treatment + blocking factor |
| 3-way | 3 factors | *a* × *b* × *c* | Complex factorial experiments with interactions |

For each factor, provide:
- **Factor Name** — Descriptive label (e.g., `Material`, `Treatment`)
- **Levels** — Comma-separated level names (e.g., `Ceramic, Stone, Bone`)

**Measurement Name** — The dependent variable label used in output tables and plots.

---

#### Effect Size Specifications

**Cohen's *f* (Standardized)**

Cohen's *f* is the standard effect size metric for ANOVA power analysis. It represents the standard deviation of standardized group means:

$$f = \\frac{\\sigma_{\\text{means}}}{\\sigma_{\\text{within}}}$$

Conventional benchmarks (Cohen, 1988):

| Effect Size | *f* value | Interpretation |
|---|---|---|
| Small | 0.10 | Detectable with large samples only |
| Medium | 0.25 | Typical for substantive effects |
| Large | 0.40 | Strong, easily detectable effects |

**Deriving *f* from prior research:**
- From published ANOVA: Convert partial η² using  $$f = \\sqrt{\\frac{\\eta^2}{1 - \\eta^2}}$$
- From Cohen's *d* (two-group case): $$f = \\frac{d}{2}$$
- From raw means and SDs: Use the **Raw** input mode — the tool computes *f* via the pooled standard deviation

**Raw Input Mode**

When prior data (pilot studies, similar publications) provide concrete mean estimates:

1. Enter expected mean for each group combination
2. Enter expected standard deviation (within-group variability)
3. The tool calculates *f* from the variance of group means relative to pooled SD

This approach is particularly valuable when:
- Measurement scales have direct interpretable units
- Published studies report means and SDs but not effect sizes
- Expected group differences are known from domain expertise

---

#### The Power—Effect—Sample Size Relationship

**Statistical Power (1 − β)**

Power is the probability of correctly rejecting a false null hypothesis. Four factors determine power:

| Factor | Effect on Power | Controlled by researcher? |
|---|---|---|
| Effect size | Larger effects → higher power | No (but can estimate from prior work) |
| Sample size | Larger *n* → higher power | **Yes** — primary design lever |
| Alpha level | Higher α → higher power | Yes (conventionally fixed at 0.05) |
| Design complexity | More groups → lower power per cell | Yes (simplify design if possible) |

**Why p-values alone are insufficient**

Researchers traditionally focus on achieving *p* < 0.05, but this binary threshold obscures critical information:

- **p-value** indicates evidence against the null, not practical importance
- **Effect size** quantifies the magnitude of the phenomenon
- **Power** indicates the reliability of the detection mechanism

A study can yield *p* < 0.05 with trivial effect sizes if *n* is large enough (overpowered), or miss meaningful effects with *p* > 0.05 if underpowered. Reporting all three — significance, effect size, and achieved power — provides complete evidence (Cumming, 2014; Lakens, 2021).

**Interpreting the Power Curve**

The curve shows how power increases asymptotically toward 1.0 as sample size grows:

- **Steep region** — Small *n* increases yield large power gains (efficient recruitment)
- **Flat region** — Diminishing returns; additional subjects add minimal power
- **Target power line** (horizontal dashed) — Your specified threshold (typically 0.80)
- **Computed *n* line** (vertical dashed) — The sample size achieving target power

If your curve never reaches target power, the effect size is too small for feasible sample sizes — reconsider the design or measurement precision.

---

#### Statistical Approaches

| Approach | Method | Assumptions | Best For |
|---|---|---|---|
| **Parametric** | F-distribution power analysis | Normality, homoscedasticity | Normal data, balanced designs, quick exact calculation |
| **Robust** | Monte Carlo with trimmed means | None (distribution-free) | Expected outliers, heavy tails, heteroscedasticity |
| **Non-Parametric** | Monte Carlo with rank tests | Ordinal or continuous data | Severely non-normal distributions, rank-based analysis |

**Simulation parameters:**
- **Iterations** — More iterations reduce Monte Carlo error. Default 1000 is adequate for most purposes. Increase to 5000+ for final grant proposals.
- **Seed** — Fixed at 42 for reproducibility; results will be consistent across runs

---

#### Best Practices

- **Literature-based effect sizes** — Use meta-analytic estimates from your field when available rather than generic "medium" benchmarks
- **Conservative planning** — Plan for power = 0.90 rather than 0.80 if study cost is high — the 10% risk of missing a true effect may be unacceptable
- **Multiple comparisons** — Factorial designs test multiple effects. Consider Bonferroni-adjusted alpha (e.g., α = 0.05/3 ≈ 0.017 for three tests) and recalculate power accordingly
- **Dropout inflation** — Increase computed sample size by 15–20% to account for attrition
- **Sensitivity analysis** — Compute power across a range of plausible effect sizes; report the range in protocols
- **Pilot-to-full** — Use pilot study variance estimates, but recognize they are uncertain — consider 80% confidence intervals around pilot SDs

---

#### Quality Assurance

**Before running the analysis:**
- Verify group count matches your factorial structure (*a* × *b* × *c*)
- Confirm Cohen's *f* is within reasonable bounds (0.05–1.0); extreme values may indicate calculation errors
- Check that distribution selection matches expected data characteristics

**After running the analysis:**
- Inspect the simulated data preview — does the pattern match your expectations?
- Verify the power curve shape is monotonically increasing (non-monotonicity indicates simulation issues)
- Compare computed *f* with your original estimate when using raw input mode

See the **FAQ** for troubleshooting common issues and guidance on finding effect size estimates.
