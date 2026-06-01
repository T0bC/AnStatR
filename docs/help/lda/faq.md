#### Frequently Asked Questions

<details>
<summary>What is the difference between LDA, QDA, and MDA — which should I use?</summary>

All three methods assign specimens to groups by maximising separation, but they differ in how they model group shapes:

| Method | Group shape assumption | When to prefer |
|--------|----------------------|----------------|
| **LDA** | All groups share the same elliptical covariance | Default choice; robust with limited data |
| **QDA** | Each group has its own covariance matrix | Groups visibly differ in spread/orientation on the LD Scores Plot; sufficient data (≥ p+1 per group) |
| **MDA** | Each group is a mixture of ellipsoidal sub-clusters | Non-elliptical or multi-modal group clouds; larger datasets |

**Practical advice**: Start with LDA. If the assumption diagnostics overlay (toggle **Show Assumption Diagnostics**) reveals that per-group ellipses differ substantially from the pooled ellipse, switch to QDA. If group clouds in the LD Scores Plot appear clearly non-elliptical or bimodal, try MDA with 2–3 subclasses.

</details>

<details>
<summary>What does the Grouping column actually do?</summary>

The grouping column provides the class label for each observation. LDA/QDA builds a discriminant function that maximises the ratio of *between-group* scatter (how far apart the group means are) to *within-group* scatter (how spread out specimens are within each group). The algorithm never sees the actual values in the grouping column as a number — it uses them purely as category labels to partition the data.

The grouping column must be selected from your **Descriptive (metadata) columns** — it must already be in the metadata selection before it appears in the grouping column dropdown. Rows with missing values in the grouping column are silently removed before computation.

</details>

<details>
<summary>What does "Proportion of Trace" mean and how do I read it?</summary>

The Proportion of Trace table is the key summary of discriminant axis importance. Each row corresponds to one linear discriminant axis (LD1, LD2, …). The **Proportion** column shows what fraction of the total between-group variance that axis captures; values sum to 1.0.

- **LD1 proportion > 0.90**: One axis dominates — a single scatter plot of LD1 captures most of the separation. Examine group differences primarily on this axis
- **LD1 + LD2 proportion > 0.90**: A two-dimensional plot is sufficient
- **Many axes with similar proportions**: Separation is genuinely multi-dimensional; rotating through multiple LD pairs is needed for full interpretation

Unlike PCA eigenvalues, the Proportion of Trace says nothing about *total* variance — it reports only the fraction of *discriminant* variance, i.e., how group differences are distributed across axes.

</details>

<details>
<summary>My resubstitution accuracy is very high but LOO-CV accuracy is much lower — what does this mean?</summary>

This gap indicates **overfitting**. The model has memorised the training data rather than learning generalisable group patterns. Common causes:

- **Too many variables relative to observations** — when p approaches n per group, discriminant functions become nearly perfectly fitted to noise. Use PCA scores as input (see Details tab) or reduce the variable set
- **Very small groups** — groups with few specimens produce unstable covariance estimates. Consider merging rare categories or collecting more data
- **Perfectly separating variables** — if one variable alone completely separates groups in the training set, LDA will exploit it even if it is spurious

A gap of more than 10–15 percentage points warrants caution about reporting the resubstitution accuracy as a model performance measure.

</details>

<details>
<summary>I get a "singular matrix" or "rank deficient" error — how do I fix it?</summary>

Singular covariance matrices occur when variables are perfectly correlated within groups or when there are fewer observations than variables per group. Solutions in order of preference:

1. **Use PCA scores as input** — switch **Data Source** to **PCA Scores** in the Data Selection tab. PCA orthogonalises the variables and eliminates collinearity
2. **Reduce the variable set** — remove variables that are perfectly or near-perfectly correlated with others (|r| > 0.95). Check correlations in the PCA Correlation Matrix first
3. **Increase the Tolerance** — in **Advanced settings**, raise `tol` from 1e-4 to 1e-3 or 1e-2. Variables with within-group variance below `tol²` are dropped automatically. This is a workaround, not a cure
4. **Switch to a robust estimation method** — `MVE` or `t` may handle near-singular matrices more gracefully than `moment`

For QDA specifically: the error almost always means a group has fewer than p + 1 observations. Switch to LDA or reduce the variable count.

</details>

<details>
<summary>Should I use proportional or equal prior probabilities?</summary>

**Proportional** (default) uses group sizes from your dataset as prior probabilities P(group). This is appropriate when your sampling reflects true population frequencies — i.e., a larger sample from a group means it is genuinely more prevalent.

**Equal** assigns equal probability 1/G to all groups regardless of sample size. Use equal priors when:
- Groups are sampled at unequal rates for logistical reasons (some groups are harder to collect)
- You want classification decisions that are not biased toward more common groups
- You are comparing the model's discriminating ability independently of group prevalence

The choice of prior affects posterior probabilities and therefore classification decisions near decision boundaries. It does not affect discriminant coefficients or the Proportion of Trace.

</details>

<details>
<summary>How many subclasses should I use for MDA?</summary>

The `subclasses` parameter controls how many Gaussian components are used to model each group. Guidelines:

- **1 subclass** — equivalent to standard LDA (linear boundaries); use as a sanity check
- **2–3 subclasses** — appropriate for mildly non-elliptical groups (default is 3)
- **4+ subclasses** — use only when group shapes are clearly multi-modal and you have ≥ 10 observations per subclass per group

The rule of thumb is: each group needs at least `subclasses × p` observations, where p is the number of variables. If groups are too small the app will display an error. Reduce subclasses until the error disappears or switch to LDA.

If MDA LOO-CV accuracy is lower than LDA LOO-CV accuracy with the same data, the extra flexibility of MDA is not warranted by the data size.

</details>

<details>
<summary>What does the "t-distribution" estimation method do and how do I choose Nu?</summary>

The `t` method replaces the classical Gaussian assumption with a multivariate t-distribution, making the estimated means and covariance matrices less sensitive to outliers. The **Nu (degrees of freedom)** parameter controls robustness:

| Nu | Behaviour |
|----|-----------|
| 3–5 | Strongly robust; heavy tails; outliers strongly downweighted |
| 6–10 | Moderately robust; good balance for most datasets |
| 20–30 | Nearly equivalent to `moment` |
| → ∞ | Equivalent to Gaussian (moment) estimation |

Start with Nu = 5. If results change substantially when removing a few extreme specimens, try Nu = 3. If results are stable, the classical `moment` estimator is sufficient. The `MVE` method (Minimum Volume Ellipsoid) is an alternative robust estimator that may be preferable when outlier contamination is severe (> 20% of observations per group).

</details>

<details>
<summary>When should I use Train/Test Split instead of LOO-CV?</summary>

Both methods estimate predictive accuracy on unseen data, but they differ in what they measure:

- **LOO-CV** uses every observation as a test point exactly once. It gives an approximately unbiased estimate but has high variance for small datasets. It is the standard choice for paleontological and morphometric datasets where samples are small
- **Train/Test Split** reserves a random fraction (default 70% training) for model fitting and evaluates on the remainder. It is faster for large datasets but depends on the specific split. Use a **Random seed** to make splits reproducible

For datasets with fewer than ~100 specimens, prefer LOO-CV. For large datasets (> 500 specimens) or when you want to demonstrate prediction on genuinely held-out data, use Train/Test Split at 70–80% training fraction.

</details>

<details>
<summary>Can I run LDA on PCA scores instead of raw measurements?</summary>

Yes, and this is the recommended approach when:
- The number of measurement variables is large relative to the number of specimens per group (p approaching n/G)
- Variables are highly correlated (redundant features inflate the variable count without adding information)
- You want to regularise the discriminant function against noise dimensions

Switch **Data Source** to **PCA Scores** in the Data Selection tab. The app displays the number of available PCA dimensions and recommends selecting enough dimensions to capture ≥ 90% of variance. Select all recommended dimensions as measurement columns and choose your grouping column. Scaling is not applied to PCA scores (they are already mean-centered from the PCA step).

This two-stage approach (PCA → LDA) is well established in geometric morphometrics and texture analysis (Ripley, 1996; Zelditch et al., 2012).

</details>

<details>
<summary>The LD Scores Plot shows all groups overlapping — what does this indicate?</summary>

Heavy overlap in LD space means the measurement variables do not strongly discriminate the defined groups. Possible interpretations:

1. **Groups are genuinely similar** in the measured traits — the chosen variables may not capture biologically/archaeologically meaningful differences
2. **Wrong variable selection** — irrelevant measurements dilute the discriminant signal; try a more targeted variable subset
3. **Wrong grouping** — the grouping column categories may not correspond to real distinct populations
4. **Insufficient data** — with very few specimens per group, discriminant functions are estimated on noise

Practical steps:
- Check the **Proportion of Trace** — if LD1 captures < 40% of the trace, group separation is weak across all axes
- Inspect the **Variable Contributions** plot — which variables have the largest discriminant coefficients? Are they the variables you expected?
- Try switching to PCA scores as input — this removes collinear noise and may reveal latent separation
- Consider whether the grouping hypothesis itself is appropriate for these measurements

</details>

<details>
<summary>What are edge cases I should be aware of?</summary>

Several data configurations produce warnings or errors:

- **Only 2 groups** — LDA produces exactly one discriminant axis (LD1). The LD Scores Plot falls back to a 1D jitter strip plot automatically
- **A group with a single specimen** — covariance cannot be estimated for that group. This is fatal for QDA; LDA will warn but proceed using the pooled covariance
- **All specimens in one group** — the grouping column has only one level. The app reports an error requiring at least 2 groups
- **A variable constant within a group** — within-group variance is zero, making the covariance matrix singular. The `tol` parameter will drop this variable if its variance is below `tol²`; increase tol to 1e-3 if needed
- **Perfectly balanced groups with equal priors** — posteriors for boundary specimens may be exactly 0.5. This is expected and not an error
- **MDA with subclasses = 1** — recovers LDA behaviour; useful as a baseline before increasing subclasses
- **PCA scores as input with very few dimensions** — if only 2 PCA dimensions are selected but these capture < 50% of variance, the LDA input space is impoverished. Select enough dimensions for ≥ 90% cumulative variance

</details>

<details>
<summary>Which R packages power the LDA / QDA / MDA computation?</summary>

| Package | Purpose | Citation |
|---------|---------|----------|
| **MASS** | LDA and QDA | Venables, W. N., & Ripley, B. D. (2002). *Modern Applied Statistics with S* (4th ed.). Springer. <https://www.stats.ox.ac.uk/pub/MASS4/> |
| **mda** | Mixture Discriminant Analysis | Hastie, T., & Tibshirani, R. (2024). *mda: Mixture and Flexible Discriminant Analysis*. <https://doi.org/10.32614/CRAN.package.mda> |
| **colorspace** | Colour palettes and manipulation | Zeileis, A., Fisher, J. C., Hornik, K., Ihaka, R., McWhite, C. D., Murrell, P., Stauffer, R., & Wilke, C. O. (2020). *colorspace: A Toolbox for Manipulating and Assessing Colors and Palettes*. *Journal of Statistical Software*, 96(1), 1–49. <https://doi.org/10.18637/jss.v096.i01> |
| **ggiraph** | Interactive SVG plots | Gohel, D., & Skintzos, P. (2026). *ggiraph: Make 'ggplot2' Graphics Interactive*. <https://doi.org/10.32614/CRAN.package.ggiraph> |
| **ggplot2** | Plot generation and styling | Wickham, H. (2016). *ggplot2: Elegant Graphics for Data Analysis*. Springer. <https://ggplot2.tidyverse.org> |
| **openxlsx** | Excel export of results | Schauberger, P., & Walker, A. (2025). *openxlsx: Read, Write and Edit xlsx Files*. <https://doi.org/10.32614/CRAN.package.openxlsx> |
| **scales** | Plot scales and colour utilities | Wickham, H., Pedersen, T. L., & Seidel, D. (2025). *scales: Scale Functions for Visualization*. <https://doi.org/10.32614/CRAN.package.scales> |

</details>
