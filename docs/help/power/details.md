#### Requirements

**Study Design Setup**

Configure factorial structure in the **Study Design** sidebar tab using either **Manual Entry** or **Import from Data** mode.

**Manual Entry Mode**

| Design | Factors | Total Groups | Use Case |
|---|---|---|---|
| 1-way | 1 factor | *k* levels | Single treatment or grouping variable |
| 2-way | 2 factors | *a* × *b* | Two treatments or a treatment + blocking factor |
| 3-way | 3 factors | *a* × *b* × *c* | Complex factorial experiments with interactions |

For each factor, provide:
- **Factor Name** — Descriptive label (e.g., `Material`, `Treatment`)
- **Levels** — Comma-separated level names (e.g., `Ceramic, Stone, Bone`)

**Measurement Name** — The dependent variable label used in output tables and plots.

**Import from Data Mode**

When you have loaded data in the **Load Data** module, you can import the existing data structure:

1. Select **Import from Data** radio button in the Study Design tab
2. Select 1-3 **Grouping Columns** — These define your experimental groups (1 column = 1-way, 2 columns = 2-way, etc.)
3. Select the **Measurement Column** — The outcome variable for analysis
4. The tool automatically detects the factor structure and displays it as read-only

**Properties automatically extracted from imported data:**

| Property | How it's computed | Usage |
|---|---|---|
| **Sample size (n)** per group | Count of non-missing observations in each group | Used for post-hoc power analysis; displayed as "observed N" |
| **Group means** | Arithmetic mean for each group combination | Basis for computing Cohen's f; shown in statistics table |
| **Standard deviations** | Within-group SD for each combination | Used for pooled SD calculation; shown in statistics table |
| **Pooled SD** | Square root of average group variance | Used in Cohen's f formula and power calculations |
| **Cohen's f** | $\sqrt{\text{between-group variance}} / \text{pooled SD}$ | Primary effect size metric for power analysis |
| **Distribution** | Shapiro-Wilk test on residuals | Determines simulation method (normal, log-normal, exponential) |

The extracted statistics are displayed in a table format in the **Effect Size** tab, along with the computed Cohen's f value.

---

#### Effect Size Specifications

**Cohen's *f* (Standardized)**

Cohen's *f* is the standard effect size metric for ANOVA power analysis. It represents the standard deviation of standardized group means:

$$f = \\frac{\\sigma_{\\text{means}}}{\\sigma_{\\text{within}}}$$

Conventional benchmarks:

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

**Import Mode Effect Size**

When using **Import from Data** mode, the effect size is computed automatically from your loaded data:

- **Effect size type** — Always uses raw parameters (mean + SD) derived from the data
- **Cohen's f** — Computed from the variance of group means relative to the pooled within-group SD
- **Distribution override** — The auto-detected distribution is shown, but you can override it if needed (normal, log-normal, exponential)

The statistics table shows:
- Group names (combinations of factor levels)
- N per group (sample size)
- Mean per group
- SD per group
- Computed Cohen's f (displayed in a highlighted alert)

**Note:** Standardized Cohen's f input is not available in import mode because the tool derives the effect size directly from the observed data.

---

#### Import Mode Workflows

**Workflow 1: Pilot-to-Full Study Planning**

Use pilot data to determine sample sizes for a full-scale study:

1. Load pilot data in **Load Data** module
2. In Power Analysis, select **Import from Data**
3. Set grouping and measurement columns
4. In **Settings**, select **Sample Size** as the solve target
5. Set desired **Target Power** (typically 0.80 or 0.90)
6. Run analysis

**Interpretation:** The result shows how many subjects per group you need in the full study to achieve your target power, given the effect size observed in the pilot data. The observed N from the pilot is shown for reference, but the recommendation applies to future studies.

**Caution:** Pilot study effect sizes are often overestimates. Consider sensitivity analysis with a range of plausible effect sizes (e.g., 50-80% of the pilot f).

**Workflow 2: Post-Hoc Power Analysis**

Calculate the achieved power of a completed study:

1. Load completed study data
2. Select **Import from Data** (auto-switches to Power calculation mode)
3. Verify the detected factor structure matches your design
4. Review the computed Cohen's f in the Effect Size tab
5. Run analysis

**Interpretation:** Shows the probability your study had of detecting the observed effect size. High power (>0.80) means you likely would have detected a true effect; low power suggests the null result may be due to insufficient sample size rather than absence of effect.

**Note:** Post-hoc power is distinct from the p-value. A significant result with low power is still valid; a non-significant result with low power is inconclusive.

**Workflow 3: Sensitivity Analysis (Minimum Detectable Effect)**

Determine what effect size your study could have detected:

1. Load study data
2. Select **Import from Data**
3. In **Settings**, select **Minimum Detectable Effect** as the solve target
4. Set **Target Power** (typically 0.80)
5. Run analysis

**Interpretation:** The MDE is the smallest Cohen's f your study could have detected with 80% probability. Compare this to theoretically meaningful effect sizes:
- If MDE < meaningful effect → Study was adequately powered; null results are informative
- If MDE > meaningful effect → Study was underpowered; null results are inconclusive

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

A study can yield *p* < 0.05 with trivial effect sizes if *n* is large enough (overpowered), or miss meaningful effects with *p* > 0.05 if underpowered. Reporting all three — significance, effect size, and achieved power — provides complete evidence.

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

**Best Practices for Import Mode**

- **Verify factor structure** — Double-check that detected factor levels match your experimental design (e.g., ensure no typos in group labels created spurious levels)
- **Check group Ns** — Unequal group sizes are handled by using minimum N; consider whether this is appropriate for your design
- **Review distribution detection** — The Shapiro-Wilk test may not detect non-normality with small samples; visually inspect your data if unsure
- **Override with caution** — Only override the auto-detected distribution if you have theoretical or visual evidence supporting a different shape
- **Document data source** — When reporting results, note that power calculations were based on pilot/preliminary data and may be optimistic

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
