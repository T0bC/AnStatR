#### Power Analysis

Plan study sample sizes and estimate statistical power for 1-way, 2-way, and 3-way factorial designs. Configure your study design, specify expected effect sizes, and choose what to calculate — the tool computes the third parameter from your inputs.

##### Output Types

Select **Solve for** in the **Settings** sidebar tab to determine what the analysis calculates:

- **Sample Size** *(default)* — Calculates required observations per group to achieve your target power given the effect size and alpha level. Use during study planning to determine recruitment targets.
- **Power** — Calculates the probability of detecting your specified effect size given a fixed sample size and alpha. Use to evaluate feasibility of existing or published study designs.
- **Minimum Detectable Effect (MDE)** — Calculates the smallest effect size detectable with your specified sample size and power. Use to interpret non-significant results or set realistic expectations.

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
