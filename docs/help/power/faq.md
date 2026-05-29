#### FAQ — Power Analysis

<details>
<summary>Where do I find effect size estimates for my study?</summary>

**Meta-analyses and systematic reviews** in your field provide the most reliable effect size estimates. Search for terms like "meta-analysis [your topic]" or "effect size [your measurement type]".

**When no direct estimates exist:**
- Use related constructs from similar methodologies (e.g., other hardness measures if studying microhardness)
- Examine Cohen's *d* from two-group studies and convert: $$f = d/2$$
- Extract partial η² from ANOVA tables and convert: $$f = \\sqrt{\\eta^2 / (1 - \\eta^2)}$$
- Pilot data from your own lab, inflated by 20–50% to account for optimistic bias

**Conservative strategy:** Plan for the smallest effect size that would be theoretically or practically meaningful, not the average effect found in literature.

</details>

<details>
<summary>What is the difference between Cohen's d and Cohen's f?</summary>

**Cohen's *d*** compares two groups: it is the standardized mean difference ($\\bar{X}_1 - \\bar{X}_2$) / pooled SD. Range is theoretically unbounded but practically −3 to +3.

**Cohen's *f*** generalizes to multiple groups: it is the standard deviation of standardized group means divided by pooled within-group SD. Range typically 0–1.5.

**Conversion:**
- For 2 groups: $f = d/2$
- For *k* groups with equal spacing: $f = d_{\\text{max}} / \\sqrt{2k}$ where $d_{\\text{max}}$ is the largest pairwise Cohen's *d*

The tool uses *f* because it generalizes naturally to factorial designs with multiple factors and interactions.

</details>

<details>
<summary>Why is 80% power the conventional target? Should I use 90% instead?</summary>

**80% power** (β = 0.20) became standard through historical convention. It represents a 4:1 ratio of Type II to Type I error rates, balancing the cost of false negatives against false positives.

**Consider 90% power when:**
- Study costs are high and cannot be repeated (underpowered studies waste resources)
- Missing a true effect has serious consequences (clinical trials, conservation decisions)
- Effect sizes are uncertain — higher power provides buffer against overestimation
- You plan to test multiple hypotheses — familywise power decreases

**Trade-off:** 90% power requires approximately 25% larger sample size than 80% power for the same effect size.

</details>

<details>
<summary>How do I account for multiple factors and interactions in power planning?</summary>

Factorial designs test multiple effects simultaneously. You have three strategies:

**1. Power for specific effect of interest**
Plan sample size to detect your primary effect (main effect of Factor A or A×B interaction) with target power. Other effects may have higher or lower power.

**2. Bonferroni-adjusted alpha**
If testing 3 effects (e.g., A, B, A×B), use α = 0.05/3 ≈ 0.017. This maintains familywise error rate but increases required sample size by ~15–20%.

**3. Power for largest effect**
Plan for the smallest effect you would find theoretically important, recognizing that larger effects will have excess power.

**Recommendation:** Report which effect size determined your sample size in methods sections. Secondary analyses should acknowledge their achieved power may differ.

</details>

<details>
<summary>What does "Minimum Detectable Effect" tell me about a non-significant result?</summary>

A non-significant p-value (*p* > 0.05) is uninformative without knowing what effect sizes could have been detected. The Minimum Detectable Effect (MDE) answers: "What is the smallest effect I could have found with 80% probability?"

**Interpretation guide:**
- If MDE is smaller than theoretically important effects → study was adequately powered; null result is informative
- If MDE is larger than important effects → study was underpowered; null result is inconclusive

**Example:** You find no significant treatment effect. The MDE is *f* = 0.45 (large). Since you would have cared about *f* = 0.25 (medium), you cannot conclude the treatment is ineffective — only that you could not detect medium effects.

Always report MDE alongside non-significant results.

</details>

<details>
<summary>Should I use parametric or simulation-based (robust/non-parametric) power analysis?</summary>

**Parametric** is faster and exact when assumptions hold. Use when:
- Data are normally distributed (or will be after appropriate transformation)
- Groups are balanced or nearly balanced
- No extreme outliers expected

**Simulation-based** is safer when assumptions are questionable. Use when:
- Previous data show outliers or heavy tails
- Groups may become unbalanced due to practical constraints
- You plan to use robust statistics for the actual analysis (power should match the planned analysis)

**General rule:** If you will analyze with robust methods, compute power with robust methods. The parametric power analysis can overestimate power for trimmed-means or rank-based analyses because those methods discard or downweight extreme values.

</details>

<details>
<summary>How many simulation iterations do I need?</summary>

Simulation error decreases with the square root of iterations. Guidance:

| Iterations | Standard Error | Use Case |
|---|---|---|
| 100 | ~5% | Quick exploration only |
| 1,000 | ~1.6% | Standard analysis (default) |
| 5,000 | ~0.7% | Grant proposals, publication |
| 10,000 | ~0.5% | Final precise estimates |

**Practical approach:** Run 1,000 iterations initially. If results are near decision boundaries (e.g., power = 0.78 when you need 0.80), increase to 5,000–10,000.

Computation time scales linearly with iterations; 10,000 iterations typically completes in under 30 seconds.

</details>

<details>
<summary>Why does increasing sample size per group eventually stop increasing power?</summary>

Power is bounded at 1.0 (100% detection probability). As sample size increases:
- Statistical precision improves (narrower confidence intervals)
- Standard errors decrease
- Test statistics grow

But once power exceeds ~0.99, additional subjects provide diminishing returns. The power curve asymptotes at 1.0.

**Practical limit:** Most researchers stop at 0.80–0.90 power. Beyond this, consider whether additional resources would be better spent on:
- Replication studies
- Multiple measurement occasions
- Broader sampling frames
- Larger effect sizes (better measurement precision)

</details>

<details>
<summary>How do I handle expected missing data or dropouts?</summary>

**Pre-study planning:**
1. Compute required complete cases (e.g., 25 per group)
2. Estimate attrition rate from similar studies (typically 10–20%)
3. Inflate recruitment target: $n_{\\text{recruit}} = n_{\\text{complete}} / (1 - \\text{attrition rate})$

**Example:** Need 25 complete, expect 15% dropout → recruit 25 / 0.85 ≈ 30 per group.

**During analysis:**
- Use listwise deletion power analysis if missing data will be excluded
- Consider multiple imputation power analysis for planned missing designs (requires specialized software)
- Missing completely at random (MCAR) preserves power; missing not at random (MNAR) biases results regardless of power

</details>

<details>
<summary>What is the relationship between alpha, power, and the false positive rate?</summary>

These concepts are often confused:

| Concept | Symbol | Definition | Controlled by |
|---|---|---|---|
| Significance level | α | Probability of false positive when null is true | Researcher (conventionally 0.05) |
| Type II error rate | β | Probability of false negative when alternative is true | Effect size, sample size, α |
| Power | 1 − β | Probability of true positive when alternative is true | Effect size, sample size, α |
| False discovery rate | FDR | Proportion of positives that are false | Depends on α, power, and proportion of true nulls |

**Key insight:** α controls false positives only when the null hypothesis is true. In fields where most tested effects are real, the actual false discovery rate may be lower than α. Conversely, in fields where most effects are null, FDR can exceed α.

Pre-registration and replication are stronger safeguards than adjusting α alone.

</details>

<details>
<summary>My power curve looks strange or non-monotonic — what happened?</summary>

**Normal behavior:** Power should increase monotonically with sample size, asymptoting toward 1.0.

**Common causes of anomalies:**
- **Insufficient iterations** — Monte Carlo error creates noise; increase iterations
- **Extreme effect sizes** — Very large *f* (> 1.0) reaches ceiling power at small *n*
- **Very small alpha** — With α = 0.001, power may remain flat until threshold *n* is reached
- **Non-parametric with small samples** — Rank tests have discrete distributions; power jumps at specific sample sizes

**Diagnostic:** Check that the simulated data preview shows reasonable patterns. If the preview looks incorrect, verify your group means and standard deviations.

</details>

<details>
<summary>How do I use data I loaded in the Load Data module?</summary>

To use imported data for power analysis:

1. **Load your data first** — Import or load your data frame in the **Load Data** module
2. **Switch to Import mode** — In Power Analysis, go to the **Study Design** tab and select **Import from Data** (radio button appears when data is available)
3. **Select grouping columns** — Choose 1-3 columns that define your experimental groups
4. **Select measurement column** — Choose the outcome variable
5. **Review detected structure** — The tool shows detected factor levels and automatically extracts group statistics
6. **Proceed to Settings** — Choose what to calculate (sample size recommendation, post-hoc power, or MDE)

**What gets extracted automatically:**
- Sample size per group
- Group means and standard deviations
- Cohen's f (computed from the data)
- Distribution shape (via Shapiro-Wilk test)

**Note:** When switching to Import mode, the system automatically switches the calculation type to **Power** (post-hoc analysis). Change this in Settings if you want sample size recommendations or MDE instead.

</details>

<details>
<summary>What is the difference between pre-study and post-hoc power analysis?</summary>

**Pre-study power analysis** (planning phase):
- **Goal:** Determine required sample size for a future study
- **Inputs:** Expected effect size (from literature or pilot data), desired power, alpha
- **Output:** Required sample size per group
- **Use case:** Grant proposals, study design, ethics applications

**Post-hoc power analysis** (after study completion):
- **Goal:** Calculate the achieved power given the actual sample size and observed effect
- **Inputs:** Observed effect size, actual sample size, alpha
- **Output:** Power achieved (probability of detecting the observed effect)
- **Use case:** Interpreting non-significant results, reporting study limitations

**Critical distinction:** Pre-study uses *hypothesized* effects; post-hoc uses *observed* effects.

**Using Import mode:**
- When you import data and select **Sample Size**, you get sample size recommendations for future studies based on the observed effect size
- When you import data and select **Power**, you get post-hoc power for the imported study
- When you import data and select **MDE**, you get sensitivity analysis for the imported study

**Controversy note:** Some statisticians criticize post-hoc power as redundant with p-values (if p < 0.05, power was sufficient by definition). However, post-hoc power remains valuable for:
- Interpreting non-significant results
- Planning replication studies
- Meta-analytic power cumulation

</details>

<details>
<summary>How do I plan sample sizes based on pilot data?</summary>

**Workflow for pilot-to-full study planning:**

1. **Load pilot data** in the **Load Data** module
2. **Select Import from Data** in the Power Analysis Study Design tab
3. **Set grouping and measurement columns** to match your design
4. **In Settings**, select **Sample Size** as the solve target
5. **Set target power** (typically 0.80 or 0.90)
6. **Run analysis**

**Interpretation:** The result shows the sample size needed per group in the full study to achieve your target power, assuming the pilot effect size is accurate.

**Important caveats:**

- **Pilot effect sizes are often inflated** — Small samples yield noisy estimates; the true effect is likely smaller than observed
- **Conservative strategy** — Use 50-80% of the pilot Cohen's f for planning, or compute sample sizes for a range of plausible effects
- **Pilot variance estimates** — These are more stable than effect size estimates and can be used with more confidence
- **Sample size recommendation** — In import mode with "Sample Size" selected, the tool recommends sample sizes for *future* studies, not analyzing the pilot itself

**Recommended approach:**
1. Use the pilot to estimate variance (SD) and get a rough effect size estimate
2. Plan for power = 0.90 rather than 0.80 to buffer against effect size overestimation
3. Run sensitivity analysis: compute required N for pilot f, 0.75×pilot f, and 0.5×pilot f
4. Choose the largest N from this range that is still feasible

</details>

<details>
<summary>Why can't I enter Cohen's f manually when using imported data?</summary>

When using **Import from Data** mode, the tool derives Cohen's f directly from the observed group means and standard deviations. This is intentional because:

1. **Consistency** — The computed f matches the actual data pattern
2. **Transparency** — Reviewers can verify the calculation from the displayed group statistics
3. **Accuracy** — Manual entry risks mismatch between the data and the assumed effect size

**If you want to use a specific Cohen's f:**
- Switch back to **Manual Entry** mode
- Enter the f value directly in the Effect Size tab
- You can still use the pilot study's SD estimates in Raw input mode if you want to simulate with specific variances

**Adjusting the effect size from pilot data:**
If you believe the pilot effect size is an overestimate (common), you have two options:
1. **Use Manual mode** with a conservative f value (e.g., 0.6× the computed f from the pilot)
2. **Run sensitivity analysis** in Import mode with Sample Size target, but interpret the result conservatively (add 25-50% more subjects)

</details>

<details>
<summary>How does the tool detect the distribution shape from my data?</summary>

The tool uses a statistical test-based approach to detect distribution shape:

1. **Normality test** — Shapiro-Wilk test on the measurement values
   - If p > 0.05: Classify as **Normal**
   - If p ≤ 0.05: Proceed to skewness check

2. **Skewness check** (for non-normal data):
   - If all values are positive: Test log-transformed values
   - If log-transform improves normality (higher p-value): Classify as **Log-normal**
   - Otherwise: Default to **Normal** (conservative choice)

**Limitations:**
- **Small samples** (< 30) — Shapiro-Wilk has low power; may not detect non-normality even when present
- **Exponential detection** — Currently not automatically detected (would require specialized tests); manually override if you know the data follows an exponential distribution
- **Bimodal/multimodal** — Not specifically detected; will likely classify as non-normal and default to normal for simulation

**Recommendation:** If you have theoretical or visual (histogram/Q-Q plot) evidence that your data follows a specific distribution, override the auto-detection in the Effect Size tab.

</details>

<details>
<summary>Which R packages power the Power Analysis module?</summary>

| Package | Purpose | Citation |
|---------|---------|----------|
| **pwr** | Power analysis calculations | Champely, S. (2020). *pwr: Basic Functions for Power Analysis*. <https://doi.org/10.32614/CRAN.package.pwr> |

</details>
