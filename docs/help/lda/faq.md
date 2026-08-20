#### Frequently Asked Questions

<details>
<summary>What does the "Apply recommended parameters" banner do?</summary>

It appears when the **Statistics** tab has computed a parameter screening ranking (enabled via the Plotting tab's **Disable plots (parameter screening mode)** checkbox, followed by **Compute Statistics**). Clicking it replaces the current measurement-column selection with the parameters that ranked among the top group separators. You can still add or remove columns manually afterward — the applied selection is not locked.

There is no functional difference between recommended and manually selected parameters: both flow through the same measurement-column selection used by the LDA/QDA/MDA computation and by the saved bundle. If the banner does not appear, either screening mode has not been used yet, or no parameters were ranked (e.g., the Statistics results have not been computed).

</details>

<details>
<summary>What is the difference between LDA, QDA, and MDA — which should I use?</summary>

All three methods assign specimens to groups by maximising separation, but they differ in how they model group shapes:

| Method | Group shape assumption | When to prefer |
|--------|----------------------|----------------|
| **LDA** | All groups share the same elliptical covariance | Default choice; robust with limited data |
| **QDA** | Each group has its own covariance matrix | Groups visibly differ in spread/orientation on the LD Scores Plot; sufficient data (≥ p+1 per group) |
| **MDA** | Each group is a mixture of ellipsoidal sub-clusters | Non-elliptical or multi-modal group clouds; larger datasets |

**Practical advice**: Start with LDA. If the assumption diagnostics overlay (toggle **Show Assumption Diagnostics**) reveals that per-group ellipses differ substantially from the pooled ellipse, switch to QDA. If group clouds in the LD Scores Plot appear clearly non-elliptical or bimodal, try MDA with 2–3 subclasses. None of these three work well when you have more measurement variables than specimens per group, or when variables are highly collinear — for that situation see the next question.

</details>

<details>
<summary>I have more measurement parameters than specimens (or highly correlated parameters) — what should I use?</summary>

This is exactly the scenario PLS-DA and sPLS-DA are designed for. LDA/QDA/MDA all require inverting a within-group covariance matrix, which fails or becomes unstable when the number of variables p approaches or exceeds the number of specimens per group, and is disrupted by collinear variables (e.g., a 2D and 3D version of a similar surface metric measuring essentially the same underlying feature).

**Switch Analysis Type to PLS-DA or sPLS-DA** in the Analysis Settings tab:
- **PLS-DA** extracts latent components that maximise covariance between your measurement variables and group membership, without ever inverting a covariance matrix — it works regardless of how many variables you have relative to specimens, and collinear variables simply share loading weight rather than causing errors
- **sPLS-DA** additionally applies sparse variable selection (the **keepX** setting), retaining only a chosen number of variables per component — this directly identifies which of your 40+ parameters actually drive the group differences, shown in the **Selected Variables** results panel

Start with sPLS-DA if your goal is identifying which specific parameters matter (typical for "what drives group X vs. group Y" research questions). Use plain PLS-DA if you only need dimensionality reduction and classification without variable selection.

</details>

<details>
<summary>What's the difference between PLS-DA and sPLS-DA?</summary>

PLS-DA uses all selected measurement variables to build each component — every variable contributes some (possibly small) loading. sPLS-DA adds a sparsity constraint that forces all but a chosen number of variables (**keepX**) to have exactly zero loading on each component, effectively performing variable selection as part of the fit.

Use PLS-DA when you want dimensionality reduction and group separation without necessarily identifying a minimal variable subset. Use sPLS-DA when the scientific question is "which specific parameters explain the group differences" — the Selected Variables panel is the direct answer. sPLS-DA models are also somewhat easier to interpret in the Component Loadings table, since only the selected variables show nonzero values.

</details>

<details>
<summary>How do I choose keepX / how many variables should I keep for sPLS-DA?</summary>

There is no universally correct value — it is a trade-off between interpretability (fewer variables, cleaner story) and completeness (more variables, less risk of excluding a genuine contributor). Two approaches:

1. **Manual**: set keepX directly per component (default 10). Start with a value roughly 10-25% of your total variable count and inspect whether the Selected Variables make biological/scientific sense and whether resubstitution accuracy stays reasonable
2. **Auto-tune**: click **Optimise variable selection (recommended)** to run a cross-validated grid search (`mixOmics::tune.splsda`) that selects the keepX minimising cross-validated balanced error rate per component. This can take from several seconds to a few minutes; the suggested values fill the manual boxes and can still be edited afterward

A keepX that is too small may exclude real contributors; a keepX close to your total variable count behaves like plain PLS-DA and loses the interpretability benefit of sparsity. If several correlated variables measure similar features, expect the selection to favour one representative rather than all of them — see the multicollinearity question below.

After computing, run **Check component count** and check the **Selected Variable Stability** table it produces — this reports how often each variable was actually selected across cross-validation folds. A variable selected consistently (stability close to 1.0) is a robust finding regardless of the exact keepX chosen; a variable that appears only because of the specific keepX value and fold assignment will show low stability and should be treated with more caution than the Selected Variables panel alone suggests.

</details>

<details>
<summary>What is a VIP score and how is it different from the Selected Variables list or the Component Loadings?</summary>

VIP (Variable Importance in Projection) is a single importance score per variable that aggregates its contribution across **all** fitted components at once, weighted by how much each component explains of group membership. It answers "how important is this variable to the model overall?" — a different question from what the other two panels answer:

- **Component Loadings** show, per component, how strongly and in which direction (sign) each variable contributes — useful for understanding *how* a variable relates to group differences on a specific axis
- **Selected Variables** (sPLS-DA only) show *whether* sparse selection kept a variable at all on a given component (nonzero loading) — a binary in/out decision per component
- **VIP** collapses all of that into one number per variable across the whole model, making it the more convenient starting point for ranking variables by overall importance, especially for **plain PLS-DA**, which never zeroes out any loading and therefore has no equivalent to the Selected Variables list

The conventional threshold is VIP > 1 for "above-average" importance — this is a widely used rule of thumb in the PLS-DA/sPLS-DA literature, not a formal statistical test. Use it to shortlist candidates for follow-up, not as a hard cutoff for scientific claims.

</details>

<details>
<summary>My variables are correlated (e.g., a 2D and 3D version of a similar surface metric) — does that break the variable selection?</summary>

It does not break the fit — PLS-DA/sPLS-DA handle collinear variables natively, unlike LDA/QDA/MDA. However, it does affect how you should *interpret* sparse variable selection: when several variables essentially measure the same underlying surface feature, sparse selection tends to pick one representative and assign the others a zero loading, somewhat arbitrarily depending on which one has marginally higher loading magnitude in the specific cross-validation fold.

This means an excluded variable is not necessarily scientifically unimportant — it may simply have been redundant given a correlated variable that was already selected. Before concluding a parameter "doesn't matter," check the PCA Correlation Matrix (or a correlation heatmap of your parameter set) to see whether it clusters tightly with a selected variable. Treat correlated clusters as a group when drawing scientific conclusions, using the selected member as a representative rather than assuming uniqueness.

</details>

<details>
<summary>How many components should I use for PLS-DA/sPLS-DA?</summary>

The **Number of components** setting defaults to (number of groups − 1), matching LDA's discriminant-axis count, but PLS-DA components do not have the same strict upper bound as LDA — you can use more or fewer depending on what the data supports.

Use the **Check component count** button (Analysis Settings sidebar) to check this empirically: it reports cross-validated Overall Error and Balanced Error Rate (BER) per component count. Look for the point where error stops decreasing meaningfully — an "elbow." Adding components past that point usually adds noise rather than genuine group-separation signal. This diagnostic is independent of the main Compute button and can be re-run after adjusting the component count.

</details>

<details>
<summary>Which prediction distance metric should I use?</summary>

For reporting, use the one the app already reports: **maximum distance** (`max.dist`). It is what `mixOmics::predict()` applies, so it is what the accuracy, confusion matrix, posterior probabilities and the exported `.rds` prediction bundle are all based on. It is also generally the strongest performer for discriminant analysis, and the mixOmics default.

The **Prediction Distance Comparison** table (in the Component Diagnostics panel, after running **Check component count**) shows the cross-validated error under all three rules — maximum distance, centroid distance and Mahalanobis distance. It costs nothing extra to compute, because `perf()` calculates all three in the same run.

Its purpose is diagnostic, not decisional. It answers "does the choice of assignment rule matter for my data?", and in most well-separated datasets the answer is no. **Do not** pick the rule with the lowest error after looking at the table and then report that number: choosing a rule on the basis of the error it produces is selecting on the test statistic, and it biases your reported accuracy optimistically. Decide on `max.dist`, report `max.dist`, and use the table to say how sensitive that figure is.

The one place you can freely change the rule is the **Boundary rule** dropdown in Plotting Controls, which affects only how the decision-boundary background is shaded in the scores plot. That is a purely visual choice and changes no reported statistic.

</details>

<details>
<summary>The three distance rules disagree — what does that mean?</summary>

A large spread between the rules (the app flags anything above about 5 percentage points) is *information about your data*, not a problem with the analysis.

The three rules differ in what geometry they assume. Centroid distance assumes groups are roughly spherical and similarly spread; Mahalanobis additionally allows for elongated or correlated component spread; maximum distance uses the fitted regression response rather than geometry at all. When groups are cleanly separated, every reasonable rule puts the boundary in much the same place, so they agree. When they disagree, it means samples are sitting close enough to the boundaries that the *assumptions* of each rule change the outcome — i.e. your groups overlap in component space.

Practical consequences:

- Treat the headline accuracy as one of several defensible numbers, not a single fact. State which rule produced it.
- Check the Component Diagnostics error curve as well — poor separation often shows up simultaneously as a flat or rising error curve.
- Look at the scores plot with decision boundaries enabled and try the **Boundary rule** dropdown. If the shaded regions shift noticeably between rules, you are seeing the same overlap visually.
- If group separation is the scientific claim, this is a signal to be cautious about it, and to report the between-rule variation as part of the uncertainty rather than suppressing it.

A large spread combined with a *high* accuracy under every rule is much less concerning than a large spread around a mediocre accuracy — in the former case the groups separate well and only the marginal cases move.

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

The app enforces a hard minimum of `max(subclasses, p + 1)` observations per group, where p is the number of variables — below that, computation is blocked with an error. That is the floor, not a target: for stable subclass estimates aim well above it, ideally around 10 observations per subclass per group. If groups are too small, reduce subclasses until the error disappears or switch to LDA.

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
<summary>Which Validation setting should I use, and does it matter?</summary>

It matters more than any other setting on that tab, because it decides whether the accuracy you are shown is a real performance estimate or the model grading its own homework.

Every model classifies its training data well — it has already seen the answers. **None (fit only)** reports exactly that: *resubstitution accuracy*, measured on the same specimens the model was fitted to. It is fast and fine for exploring, but it is optimistic by construction and sometimes wildly so. A model with 40 measurement parameters and 15 specimens per group can report 100% while being no better than chance on new specimens.

| Your situation | Use | Why |
|----------------|-----|-----|
| Exploring — "do these groups separate at all?" | **None** | Fastest; you are reading the scores plot, not claiming performance |
| LDA / QDA / MDA, reporting a number | **Leave-one-out CV** | Uses every specimen, no dependence on a lucky split, deterministic |
| Small groups (under ~15 specimens) | **Leave-one-out CV** | A 30% holdout might leave only 3–4 specimens per group — too few to measure anything |
| PLS-DA / sPLS-DA, reporting a number | **Train / Test Split** | LOO-CV is not offered here (a full refit per specimen is prohibitively slow); use **Check component count** for the cross-validated view |
| Large dataset, strictest possible check | **Train / Test Split** | The held-out specimens are used exactly once |

**Rule of thumb**: Leave-one-out CV for LDA/QDA/MDA, Train / Test Split for PLS-DA/sPLS-DA.

**Are the defaults sensible?** Yes. **None** is the right default because the first thing you do is check whether the groups separate at all, and making every first run slow would be a poor trade. The 70/30 split is standard and is stratified, so class proportions are preserved, and the fixed random seed means your split is reproducible. What you should *not* do is leave it on None and then quote the number.

**Where the results appear**: the setting does not add panels — it changes what three existing ones are computed from.

| Panel | None | Leave-one-out CV | Train / Test Split |
|-------|------|------------------|--------------------|
| Summary badge | "Resubstitution Accuracy" | "LOO-CV Accuracy" | "Test Accuracy" |
| Confusion Matrix | All specimens, self-predicted | Each specimen predicted by a model fitted without it | Test specimens only |
| Posterior Probabilities | "(All Data)" | "(LOO-CV)" | "(Test Set)" |
| Train/Test Split panel | — | — | Appears, showing specimen counts per group per split |

Whenever a validated option is active, the Summary panel also shows the resubstitution figure next to it and the gap between them — see the Details tab for how to read that gap.

One thing to keep in mind: the **scores plot always shows the full fitted model**, even in split mode. It is not restricted to training specimens, so the plot and a test-set confusion matrix are describing different things.

</details>

<details>
<summary>What should I report in a paper or thesis?</summary>

Report enough that a reader can judge the result without re-running it. The minimum is **what you ran**, **how well it worked**, and **which variables drove it**.

**Always report these numbers**

| What | Where to find it | Why it is needed |
|------|------------------|------------------|
| Method and software | — | e.g. "sPLS-DA (mixOmics 6.x, R 4.x)" — see the package table below for citations |
| n per group, number of variables | Summary panel | Reviewers need the sample-size-to-variable ratio to judge overfitting risk |
| Preprocessing | Data Selection sidebar | Scaling/centring, transformations, how missing values were handled |
| **Validated** accuracy — not resubstitution | Confusion Matrix panel | LOO-CV or test-set accuracy. Resubstitution alone overstates performance |
| Validation scheme | Analysis Settings | "LOO-CV", or "70/30 stratified split", or "5-fold CV × 10 repeats" |
| Per-class performance | Per-Class Metrics | An overall accuracy hides a group that classifies poorly, especially with unbalanced groups |

**Add these depending on method**

- **LDA/MDA** — Proportion of Trace for the axes you show, and the discriminant coefficients (or the top few) if you interpret axis meaning
- **PLS-DA/sPLS-DA** — the number of components and *how you chose it* (the error curve and table produced by **Check component count**); VIP scores for the variables you highlight
- **sPLS-DA** — keepX per component **and whether it was tuned**; the Selected Variables list; and the stability values, which tell a reader the selection is reproducible rather than an artefact of one data split

**Which plots to show**

1. **The scores plot** — usually the only plot you need. Label the axes with their explained variance, and state which axes are shown. For PLS-DA/sPLS-DA, show the axes that actually separate the groups (see the **Best axes to plot** recommendation in the Dimension Evaluation panel), not Comp1/Comp2 by default — and say so explicitly if they are not the first two, with the ANOVA R² as justification. That is a finding about your data, not a caveat.
2. **A variable-importance figure**, when "which parameters matter" is the question — the VIP scores or Variable Contributions plot
3. **The confusion matrix**, as a small table rather than a figure

**The two most common mistakes**

- **Reporting resubstitution accuracy as performance.** It is measured on the same specimens the model was fitted to, so it is optimistic by construction — sometimes dramatically. Always report the cross-validated or test-set figure, and if you quote both, say which is which.
- **Describing separation from the plot instead of the metric.** The scores plot shows two axes; the model classifies using all of them. A clean-looking plot is not evidence of accuracy, and an overlapping one is not evidence of failure. The confusion matrix is the result; the plot illustrates it.

A defensible one-paragraph summary follows this shape (placeholders in `CAPITALS` — substitute your own values):

> *"sPLS-DA (mixOmics) was applied to `N_VARIABLES` measurement parameters across `N_GROUPS` groups (n = `N_PER_GROUP` each), scaled and centred. `N_COMPONENTS` components were retained based on cross-validated balanced error rate; keepX was tuned by cross-validation (`KEEPX_PER_COMPONENT` variables per component). Classification accuracy was `ACCURACY`% under `VALIDATION_SCHEME`; per-class accuracy ranged `MIN`–`MAX`%. `VARIABLE_A`, `VARIABLE_B` and `VARIABLE_C` had VIP > 1 on component 1 and were selected in > `STABILITY`% of cross-validation folds."*

Every placeholder above corresponds to a number the app reports — none of them should be estimated or omitted.

</details>

<details>
<summary>Which R packages power the LDA / QDA / MDA / PLS-DA / sPLS-DA computation?</summary>

| Package | Purpose | Citation |
|---------|---------|----------|
| **MASS** | LDA and QDA | Venables, W. N., & Ripley, B. D. (2002). *Modern Applied Statistics with S* (4th ed.). Springer. <https://www.stats.ox.ac.uk/pub/MASS4/> |
| **mda** | Mixture Discriminant Analysis | Hastie, T., & Tibshirani, R. (2024). *mda: Mixture and Flexible Discriminant Analysis*. <https://doi.org/10.32614/CRAN.package.mda> |
| **mixOmics** | PLS-DA and sPLS-DA | Rohart, F., Gautier, B., Singh, A., & Lê Cao, K.-A. (2017). *mixOmics: An R package for 'omics feature selection and multiple data integration*. *PLOS Computational Biology*, 13(11), e1005752. <https://doi.org/10.1371/journal.pcbi.1005752> |
| **colorspace** | Colour palettes and manipulation | Zeileis, A., Fisher, J. C., Hornik, K., Ihaka, R., McWhite, C. D., Murrell, P., Stauffer, R., & Wilke, C. O. (2020). *colorspace: A Toolbox for Manipulating and Assessing Colors and Palettes*. *Journal of Statistical Software*, 96(1), 1–49. <https://doi.org/10.18637/jss.v096.i01> |
| **ggiraph** | Interactive SVG plots | Gohel, D., & Skintzos, P. (2026). *ggiraph: Make 'ggplot2' Graphics Interactive*. <https://doi.org/10.32614/CRAN.package.ggiraph> |
| **ggplot2** | Plot generation and styling | Wickham, H. (2016). *ggplot2: Elegant Graphics for Data Analysis*. Springer. <https://ggplot2.tidyverse.org> |
| **openxlsx** | Excel export of results | Schauberger, P., & Walker, A. (2025). *openxlsx: Read, Write and Edit xlsx Files*. <https://doi.org/10.32614/CRAN.package.openxlsx> |
| **scales** | Plot scales and colour utilities | Wickham, H., Pedersen, T. L., & Seidel, D. (2025). *scales: Scale Functions for Visualization*. <https://doi.org/10.32614/CRAN.package.scales> |

</details>
