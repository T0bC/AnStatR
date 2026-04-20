#### Power Analysis

Plan study sample sizes and estimate statistical power for 1-way, 2-way, and 3-way factorial designs. Configure your study design, specify expected effect sizes, and choose what to calculate — the tool computes the third parameter from your inputs.

##### Data Import Mode

Load an existing data frame in the **Load Data** module to enable the **Import from Data** option in the Power Analysis sidebar. This allows you to use pilot data, preliminary measurements, or completed studies as the basis for power calculations.

**How it works:**
1. Load your data in the **Load Data** tab first
2. In the Power Analysis module, select **Import from Data** in the Study Design sidebar tab
3. Select 1-3 **Grouping Columns** that define your experimental groups (determines 1/2/3-way design)
4. Select the **Measurement Column** containing your outcome variable

**Automatically extracted properties:**
- **Sample size (n)** per group
- **Group means** — average value for each group combination
- **Standard deviations** — within-group variability
- **Cohen's f** — computed automatically from the observed group means and pooled SD
- **Distribution shape** — auto-detected using the Shapiro-Wilk test (normal, log-normal, or exponential)

**Use cases for imported data:**
- **Pilot-to-full study planning** — Use pilot data to calculate proper sample sizes for the full study
- **Post-hoc power analysis** — Calculate the achieved power of a completed study
- **Sensitivity analysis** — Determine the minimum detectable effect given your actual sample size

##### Output Types

Select **Solve for** in the **Settings** sidebar tab to determine what the analysis calculates:

- **Sample Size** *(default)* — Calculates required observations per group to achieve your target power given the effect size and alpha level. Use during study planning to determine recruitment targets. When using imported data, this recommends sample sizes for future studies based on the observed effect size.
- **Power** — Calculates the probability of detecting your specified effect size given a fixed sample size and alpha. Use to evaluate feasibility of existing or published study designs. When using imported data, this performs post-hoc power analysis using the observed sample size and effect size.
- **Minimum Detectable Effect (MDE)** — Calculates the smallest effect size detectable with your specified sample size and power. Use to interpret non-significant results or set realistic expectations. When using imported data, this shows what effect size your study could have detected with its actual sample size.

##### Required Inputs by Output Type

**Sample Size calculation needs:**
- Target Power (typically **0.80** or **0.90**)
- Significance Level α (typically **0.05**)
- Effect size (Cohen's *f* or raw group means + SD)
- Study design (factor structure)

**Power calculation needs:**
- Sample size per group (planned or actual *n*)
- Significance Level α
- Effect size
- Study design

**Minimum Detectable Effect calculation needs:**
- Sample size per group
- Target Power
- Significance Level α
- Study design

##### Effect Size Input

Choose input method in the **Effect Size** sidebar tab:

- **Standardized** — Enter Cohen's *f* directly (benchmarks: Small=0.10, Medium=0.25, Large=0.40). For multi-way designs, specify *f* for each main effect and interaction.
- **Raw** — Enter expected group means and standard deviations. The tool converts these to Cohen's *f* automatically.

##### Statistical Approach

Select the computational method in **Settings**:

- **Parametric (ANOVA)** — Classical power analysis using F-distribution. Fast and exact for normal data.
- **Robust (Trimmed Means)** — Simulation-based power using trimmed means. Recommended for data with expected outliers or heavy tails.
- **Non-Parametric** — Simulation-based power using rank tests. For ordinal data or severely non-normal distributions.

Simulation approaches require setting iteration count (default **1000**).

##### Output Display

After clicking **Compute Power Analysis**, results include:

- **Primary result card** — Shows the computed value (sample size, power, or MDE) with Cohen's *f* and design summary
- **Power curve** — Visualizes how power changes across sample sizes, with your target marked
- **Simulated data preview** — Scatter plot showing expected data pattern based on your inputs

See the **Details** tab for guidance on choosing effect sizes and interpreting the power curve.
