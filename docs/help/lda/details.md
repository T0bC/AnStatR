#### LDA / QDA / MDA — Technical Reference

##### Requirements

**Data Structure**

| Requirement | LDA | QDA | MDA |
|-------------|-----|-----|-----|
| **Min. observations per group** | > p (warning if violated) | ≥ p + 1 (hard error) | ≥ max(subclasses, p + 1) |
| **Min. groups** | 2 | 2 | 2 |
| **Data type** | Numeric only | Numeric only | Numeric only |
| **Missing values** | Rows with NAs excluded automatically | same | same |
| **Max. discriminant axes** | min(p, G − 1) | none (classification only) | min(p, G − 1) |

Where p = number of measurement variables, G = number of groups.

**Metadata and Grouping Columns**

- **Descriptive (metadata) columns** are carried through the analysis purely for labelling. They appear in the scores plot tooltips, in the exported results tables, and on the axes of the LD Scores Plot. They do not influence the discriminant function in any way
- **Grouping column** must be a categorical variable selected from the metadata columns. It defines the class labels fed to MASS::lda() / MASS::qda() / mda::mda(). Rows with missing values in the grouping column are dropped

##### Technical Specifications

<details>
<summary><strong>LDA Computation Method</strong></summary>

LDA is implemented via `MASS::lda()` (Venables & Ripley, 2002). The algorithm finds the projection matrix $\mathbf{W}$ that maximises the ratio of between-group scatter $\mathbf{S}_B$ to within-group scatter $\mathbf{S}_W$:

$$\mathbf{W} = \underset{\mathbf{W}}{\arg\max} \frac{|\mathbf{W}^\top \mathbf{S}_B \mathbf{W}|}{|\mathbf{W}^\top \mathbf{S}_W \mathbf{W}|}$$

This is solved as a generalised eigenvalue problem. The resulting eigenvectors are the discriminant coefficients (the **scaling** matrix); the eigenvalues (accessed as SVD singular values) determine how much between-group variance each axis explains.

The number of usable discriminant axes is $\min(p,\, G - 1)$. With two groups there is exactly one LD axis; additional groups yield additional axes.

**References**: Fisher (1936); McLachlan (2004); Venables & Ripley (2002).

</details>

<details>
<summary><strong>QDA Computation Method</strong></summary>

QDA is implemented via `MASS::qda()` (Venables & Ripley, 2002). Unlike LDA, QDA estimates a separate covariance matrix $\boldsymbol{\Sigma}_k$ for each group $k$. The quadratic discriminant function for group $k$ is:

$$\delta_k(\mathbf{x}) = -\tfrac{1}{2} \log|\boldsymbol{\Sigma}_k| - \tfrac{1}{2} (\mathbf{x} - \boldsymbol{\mu}_k)^\top \boldsymbol{\Sigma}_k^{-1} (\mathbf{x} - \boldsymbol{\mu}_k) + \log \pi_k$$

Because each group needs to invert its own $p \times p$ covariance matrix, at least $p + 1$ observations per group are required. QDA does not produce discriminant axes; the companion LDA fit (fitted automatically on the same data) is used for LD-space visualisation only.

**References**: Hastie, Tibshirani & Friedman (2009); Venables & Ripley (2002).

</details>

<details>
<summary><strong>MDA Computation Method</strong></summary>

MDA is implemented via `mda::mda()` (Hastie & Tibshirani, 1996). Each group is modelled as a mixture of `subclasses` Gaussian sub-populations:

$$P(\mathbf{x} \mid \text{group}\, k) = \sum_{r} \pi_{kr}\, \mathcal{N}(\mathbf{x};\, \boldsymbol{\mu}_{kr},\, \boldsymbol{\Sigma})$$

Parameters are estimated by the Expectation–Maximisation (EM) algorithm. Because a shared (pooled) covariance matrix is used across all sub-populations, MDA generalises LDA to non-elliptical group shapes without the per-group covariance requirement of QDA.

Key MDA settings:
- **Subclasses per group** — number of Gaussian components per class (default 3; set to 1 to recover standard LDA behaviour)
- **Max EM iterations** — convergence limit for the EM algorithm (increase to 20–50 if deviance is still decreasing)

**References**: Hastie & Tibshirani (1996); Fraley & Raftery (2002).

</details>

<details>
<summary><strong>Using PCA Scores as LDA Input</strong></summary>

The **Data Source** toggle in the Data Selection tab allows LDA / QDA / MDA to be run on **PCA scores** (the individual coordinates from a prior PCA run) instead of the raw measurements. This two-stage approach is well established in morphometrics and texture analysis (Ripley, 1996; Zelditch, Swiderski & Sheets, 2012) - @TM need to reread those sources, fultext not yet available to me.

###### Benefits

- **Eliminates collinearity** — PCA components are orthogonal by construction. Because LDA's within-group covariance matrix $\mathbf{S}_W$ must be invertible, highly correlated raw variables frequently cause singularity errors. PCA scores are always full-rank up to the number of retained components
- **Reduces dimensionality ($p < n/G$)** — when the number of raw variables $p$ approaches or exceeds the number of observations per group $n/G$, discriminant functions overfit. Retaining only the PCA dimensions that explain ≥ 90% of variance substantially reduces p while preserving most of the data structure
- **Noise removal** — trailing PCA dimensions typically capture measurement noise. Excluding them prevents noise dimensions from inflating within-group scatter and diluting discriminant signal
- **Scaling is implicit** — PCA scores are already mean-centred (and standardised if Scale & Center was used in PCA). No additional scaling is needed in the LDA step

###### Interpretation Complications

Using PCA scores as input decouples the LDA discriminant coefficients from the original measurement variables. This has direct consequences for reporting:

- **Discriminant coefficients refer to PCA dimensions, not original variables** — the scaling matrix shows how much each Dim.1, Dim.2, … contributes to LD1, LD2, etc. A large coefficient on Dim.1 does not directly tell you which original measurement drives group separation; you must back-project through the PCA loadings to recover that information
- **Variable Contributions plot loses direct interpretability** — the jitter plot shows contributions per PCA dimension, not per raw variable. Cross-referencing with the PCA variable loadings table is required to identify which original measurements are most discriminating
- **Proportion of variance explained is not additive** — the PCA Proportion of Variance (how much total variance each PC explains) and the LDA Proportion of Trace (how much between-group variance each LD axis captures) are independent quantities and cannot be multiplied to yield a single interpretable percentage
- **Results depend on which PCA dimensions were retained** — including too few dimensions (e.g., only PC1–PC2) may discard PCA dimensions that carry group-discriminating information even if they explain little total variance. A variable that explains 3% of total variance can still strongly discriminate groups. As a safeguard, retain enough dimensions to capture ≥ 90% cumulative variance; the app displays a recommendation based on your PCA result

**Practical guideline**: use PCA scores as input primarily as a methodological fix for dimensionality problems or collinearity. When the primary goal is identifying *which original measurements discriminate the groups*, run LDA directly on the raw (scaled) variables — provided the sample size per group comfortably exceeds the variable count.

**References**: Ripley (1996); Zelditch, Swiderski & Sheets (2012); Mitteroecker & Gunz (2009).

</details>

<details>
<summary><strong>Scaling Implications for LDA</strong></summary>

Scaling decisions directly affect the within-group and between-group scatter matrices and therefore which variables drive the discriminant axes:

| Scaling | Covariance Structure | Effect on LDA | Recommended When |
|---------|---------------------|---------------|-----------------|
| **Scale & Center** | Correlation matrix | All variables contribute equally to $\mathbf{S}_W$; discriminant coefficients are comparable across variables | Variables have different units (mm, %, counts, etc.) |
| **Center only** | Covariance matrix | High-variance variables exert stronger influence on discriminant directions | All variables share the same unit and variance differences are scientifically meaningful |
| **No scaling** | Raw cross-products | Raw scale dominates; variables with large absolute values can monopolise LD axes | Data already preprocessed to a common scale |

**Critical note**: Because LDA computes a ratio of scatter matrices, variables with very large raw variance can render other variables effectively invisible even if they carry genuine group-discriminating information. **Scale & Center is strongly recommended** for mixed-unit data (McLachlan, 2004). Scaling does not need to be applied when using PCA scores as input — the PCA step has already standardized the feature space.

</details>

<details>
<summary><strong>Data Normalisation</strong></summary>

The **Normalize skewed variables** option uses the `bestNormalize` package (Peterson & Cavanaugh, 2020) to transform variables with |skewness| > 2 before analysis. Candidate transformations include Box-Cox, Yeo-Johnson, log, and square-root. The transformation that best achieves normality (assessed by the Pearson P/df statistic) is selected automatically.

**LDA is formally derived under the assumption of multivariate normality within groups.** Extreme skewness inflates within-group scatter estimates, distorts the covariance matrix, and can reduce classification accuracy. Normalisation reduces this risk but alters the measurement scale — interpret discriminant coefficients with caution after transformation. The transformation parameters are stored in the RDS export for full reproducibility.

Enable normalisation when:
- Skewness warning is shown for specific columns
- Outliers likely represent measurement error rather than genuine signal
- Classification accuracy is notably poor without normalisation

</details>

##### Estimation Methods (LDA and QDA)

The estimation method controls how the group means and covariance matrices are computed:

| Method | Description | Use Case |
|--------|-------------|----------|
| **Moment** (default) | Classical moment estimators (sample mean, sample covariance) | Standard; appropriate for clean, roughly normal data |
| **MLE** | Maximum likelihood estimators (biased covariance, divides by n not n−1) | Equivalent to moment for large n; rarely preferred |
| **MVE** | Minimum Volume Ellipsoid — robust estimator downweighting outliers | Data with outliers; robustness is the priority |
| **t-distribution** | Robust estimates assuming multivariate t errors; controlled by the **Nu** parameter | Moderate outlier contamination; lower Nu = heavier tails |

The **Nu (degrees of freedom)** parameter (visible only for `t` method) governs tail weight: ν → ∞ approaches the Gaussian case; ν = 3–5 is strongly robust. See the FAQ for guidance on choosing Nu.

##### Validation Methods

| Method | What It Measures | Limitation |
|--------|-----------------|------------|
| **None (fit only)** | Resubstitution accuracy — classified on training data | Always optimistic; overestimates true performance |
| **Leave-one-out CV** | Each specimen predicted by a model trained on all others (MASS::lda/qda CV=TRUE; manual loop for MDA) | Conservative for small datasets; computationally intensive for MDA |
| **Train / Test Split** | Stratified random split; holdout set accuracy | Single-split variance; reproducible via **Random seed** |

**Resubstitution accuracy** is always reported in the LDA Results panel. When LOO-CV or Train/Test Split is used, the cross-validated or test-set accuracy is reported alongside it.

##### Data Interpretation — LDA Results Panels

The **LDA Results** accordion contains up to nine sub-panels depending on analysis type and validation mode. The panels appear in the order described below.

<details>
<summary><strong>Resubstitution / LOO-CV / Test Accuracy (Summary panel)</strong></summary>

The top of the summary panel shows a coloured accuracy badge and a brief model description:

| Field | Meaning |
|-------|---------|
| **Analysis** | Method used: LDA, QDA, or MDA |
| **Observations** | Number of rows after NA removal |
| **Variables** | Number of measurement columns entered |
| **Groups** | Number of distinct groups and their labels |
| **Discriminant axes** | Number of LD axes computed — always $\min(p,\, G-1)$; not shown for QDA |

The accuracy badge is colour-coded: green ≥ 90 %, yellow ≥ 70 %, red < 70 %.

**Which accuracy is shown** depends on the validation mode selected:

| Validation | Label | Interpretation |
|------------|-------|---------------|
| None | **Resubstitution Accuracy** | Model classified the same data it was trained on — always optimistic; upper bound of true performance |
| LOO-CV | **LOO-CV Accuracy** | Each specimen predicted by a model trained without it — unbiased estimate for small samples |
| Train/Test Split | **Test Accuracy** | Accuracy on the held-out test set — most realistic estimate for larger datasets |

A resubstitution accuracy of 100 % with LOO-CV accuracy substantially lower is a strong sign of overfitting. See the FAQ for remedies.

</details>

<details>
<summary><strong>Prior Probabilities</strong></summary>

Lists the prior probability $\pi_k$ assigned to each group before seeing the measurement data.

| Setting | Prior values | Effect on classification |
|---------|-------------|--------------------------|
| **Proportional** | Proportional to group size in the training data | Larger groups are more likely to be predicted; mirrors realistic population frequencies |
| **Equal** | $1/G$ for all groups | All groups treated equally regardless of sample size; removes sampling-frequency bias from predictions |

Prior probabilities influence posterior probabilities and decision boundaries but do not affect discriminant coefficients or the Proportion of Trace. If one group is substantially larger than others and priors are proportional, small groups near boundaries will tend to be absorbed by the larger group.

</details>

<details>
<summary><strong>Group Means</strong></summary>

Shows the within-group mean for every measurement variable, computed on the (scaled) data used for analysis. Rows are groups; columns are variables.

These are the centroid coordinates that LDA/QDA uses as the reference points for classification. Key uses:

- **Identify which variables distinguish groups**: large mean differences across groups on a variable signal strong discriminating potential on that variable
- **Interpret discriminant axes**: if LD1 has large positive coefficients for variable X and group A has the highest mean on X, group A will plot at the positive end of LD1
- **Verify scaling effect**: with Scale & Center applied, all means are in standard deviation units (z-scores) and are directly comparable across variables

For QDA and MDA the group means have the same interpretation, but the within-group covariance structure differs between methods.

</details>

<details>
<summary><strong>Coefficients of Linear Discriminants / Discriminant Coefficients</strong></summary>

*Available for LDA and MDA (model mode only); not shown for QDA.*

The table lists the discriminant coefficients (the **scaling matrix**): how much each variable contributes to each LD axis. Rows are variables; columns are LD1, LD2, … (LDA) or DC1, DC2, … (MDA).

After z-score scaling the coefficients are on a common scale and directly comparable:

| Coefficient magnitude | Interpretation |
|-----------------------|---------------|
| Large positive | Variable pulls specimens towards the positive end of that axis |
| Large negative | Variable pulls specimens towards the negative end of that axis |
| Near zero | Variable contributes little to separation on that axis |

To identify the primary discriminating variables: look for the rows with the largest absolute values in LD1. These are the measurements that most strongly separate the groups along the first (and usually most important) discriminant axis.

The **Variable Contributions** jitter plot (separate accordion panel below the LDA Results) visualises these coefficients across all LD axes simultaneously — variables with consistently large absolute values across multiple axes are the overall key discriminators.

For MDA, the coefficients describe the shared pooled discriminant space across all mixture components; their interpretation is analogous to LDA coefficients.

</details>

<details>
<summary><strong>MDA Subclass Information (MDA only)</strong></summary>

*Shown only for MDA model fits (not CV mode).*

Contains two sub-sections:

**Subclass Priors** — the estimated prior probability of each subclass within each group. Each group is modelled as a mixture of `subclasses` Gaussian components; the subclass prior shows how much weight each component received after EM convergence. Balanced subclass priors (all components roughly equal weight) indicate the subclasses are all being used. A subclass with near-zero prior has collapsed and is effectively unused — consider reducing the subclasses count.

**Model Details** lists:
- **Dimension** — number of discriminant dimensions retained by the MDA fit
- **Subclasses per group** — the value used for the current run
- **Deviance** — the final negative log-likelihood of the fitted model; lower is better, but only comparable across runs on identical data

</details>

<details>
<summary><strong>Proportion of Trace</strong></summary>

*Available for LDA and MDA (model mode only); not shown for QDA.*

The primary summary of discriminant axis importance, directly analogous to the variance-explained table in PCA. Columns:

| Column | Definition | Interpretation |
|--------|-----------|----------------|
| **LD / DC** | Axis label — LD1, LD2, … for LDA; DC1, DC2, … for MDA | Axes are ranked by discriminating power, LD1/DC1 always first |
| **Singular Value** | Square root of the corresponding eigenvalue (LDA only) | Larger → stronger between-group separation on that axis |
| **Proportion** | Fraction of total between-group variance explained by this axis | Values sum to 1.0 |
| **Cumulative** | Running sum of proportions | Background colour: grey < 0.6, yellow 0.6–0.8, green > 0.8 |

**Reading the table**: If LD1 proportion > 0.90, a single scatter plot of LD1 captures the overwhelming majority of group separation. If the proportion is split more evenly (e.g., 0.69 / 0.31), both axes carry substantial discriminating information and the two-dimensional LD Scores Plot should be examined carefully. The cumulative column reaching green (> 0.80) indicates that the axes up to that row together explain most between-group variance.

</details>

<details>
<summary><strong>Dimension Evaluation (ANOVA)</strong></summary>

*Shown for model fits (not CV mode) when LD scores are available. For QDA, based on the companion LDA projection.*

A one-way ANOVA is run separately for each discriminant axis, testing whether group membership explains a significant proportion of the variance in the LD scores on that axis. Columns:

| Column | Definition | Interpretation |
|--------|-----------|----------------|
| **Dimension** | LD1, LD2, … | Each row is one discriminant axis |
| **F** | ANOVA F-statistic | Higher F → stronger group effect on this axis |
| **p-value** | Significance of the F-test | Reported as exact value or `< 0.001` |
| **R² (%)** | Proportion of LD-score variance explained by group | Background: grey < 10 %, yellow 10–25 %, green > 25 % |
| **Sig.** | Significance stars | *** p < 0.001, ** p < 0.01, * p < 0.05, . p < 0.1 |

A high R² (e.g., 90 %) on LD1 confirms that group membership strongly structures the scores on that axis. Low R² or non-significant F on an axis means that axis adds little discriminating value over chance — consider omitting it from plots and interpretation.

**Note for QDA**: the ANOVA uses the companion LDA projection (fitted internally for visualisation purposes), not the QDA classification boundaries. It still provides useful guidance on which projected axes carry group signal.

</details>

<details>
<summary><strong>Confusion Matrix</strong></summary>

The confusion matrix cross-tabulates true group labels (rows) against predicted labels (columns). A perfect classifier has counts only on the diagonal; off-diagonal cells indicate misclassifications.

**Per-Class Metrics** table:

| Metric | Formula | Interpretation |
|--------|---------|----------------|
| **N** | Group sample size | Larger groups have more influence on overall accuracy |
| **Correct** | True positives (diagonal cell) | Specimens correctly assigned to their true group |
| **Precision** | $\text{TP} / (\text{TP} + \text{FP})$ | Of all specimens *predicted* as this group, what fraction truly belongs |
| **Recall** | $\text{TP} / (\text{TP} + \text{FN})$ | Of all specimens *truly* in this group, what fraction was correctly predicted |
| **F1** | $2 \times \text{Precision} \times \text{Recall} / (\text{Precision} + \text{Recall})$ | Harmonic mean — use when group sizes are imbalanced |

Overall accuracy (shown below the table) is the fraction of all specimens correctly classified. The **accuracy badge** in the summary panel uses the same value with colour coding: green ≥ 90 %, yellow ≥ 70 %, red < 70 %.

Which data the confusion matrix describes depends on the validation mode:
- **None**: resubstitution — trained and tested on all data; always optimistic
- **LOO-CV**: leave-one-out predictions — unbiased but conservative
- **Train/Test Split**: test-set predictions only — see the **Train / Test Split** panel for split details

</details>

<details>
<summary><strong>Posterior Probabilities</strong></summary>

Lists the posterior probability $P(\text{group}_k \mid \mathbf{x})$ for every specimen and every group. The specimen is assigned to the group with the highest posterior (shown in the **Predicted** column).

Table columns:
- **Metadata columns** (e.g., `SAMPLE_ID`, `SPECIES`) — carried from the descriptive column selection for identification
- **Predicted** — the group assignment based on the highest posterior
- **One column per group** — the posterior probability for that group, summing to 1.0 across all group columns for each row

| Posterior pattern | Interpretation |
|-------------------|---------------|
| One group near 1.0, others near 0.0 | Confident, unambiguous classification |
| Two groups both > 0.3 | Specimen is near the decision boundary; classification uncertain |
| All groups roughly equal (≈ 1/G) | Specimen is equidistant from all group centroids — highly ambiguous |

The panel label changes based on validation mode:
- **Posterior Probabilities (All Data)** — model fitted on all data, posteriors computed on training set
- **Posterior Probabilities (LOO-CV)** — each specimen's posterior from the model that excluded it
- **Posterior Probabilities (Test Set)** — posteriors for the held-out test specimens only

For QDA, posteriors are derived from the per-group covariance matrices and are the primary classification result (no LD scores are produced directly). For MDA, posteriors are summed across subclass components within each group.

</details>

<details>
<summary><strong>Train / Test Split (split mode only)</strong></summary>

*Shown only when Train/Test Split validation is selected.*

Displays the **Stratified Split Summary** table, listing for each group how many specimens were allocated to the training set and how many to the test set. The split is stratified, meaning each group's proportional representation is maintained in both subsets.

Key checks:
- All groups should have at least a few specimens in both train and test — if a group is very small it may appear only in train, making test-set accuracy for that group undefined
- The **Random seed** controls which specimens are assigned to train vs. test; use the same seed to reproduce the exact split

The confusion matrix and posterior probabilities shown in their respective panels refer to the **test set** when split mode is active. The summary accuracy badge shows **Test Accuracy**, not resubstitution.

</details>

<details>
<summary><strong>Download Results</strong></summary>

Two export formats are available:

**Download Excel (All Results)** — an `.xlsx` workbook with one sheet per result component: LD scores (or posterior probabilities for QDA) with metadata columns prepended, proportion of trace, group means, discriminant coefficients, confusion matrix, and per-class metrics. Sheet 1 (scores / posteriors with metadata) is ready for import into the Cluster module or for external analysis.

**Download RDS (LDA/QDA Object)** — an `.rds` file containing the full result bundle including the fitted model object, raw and scaled data, transformation parameters, scale parameters, and all settings. Load in R with `readRDS()` for programmatic access to the model or for reproducibility documentation.

</details>

##### Plotting Controls

Configure the **LD Scores Plot** in the **LDA Plotting Controls** sidebar tab:

| Control | Options | Effect |
|---------|---------|--------|
| **Dim.X / Dim.Y** | LD1, LD2, … (LDA/MDA) or original variables (QDA) | Select which discriminant axes map to the plot axes |
| **Dim.Z** | Same choices as X/Y | Reserved for future 3D discriminant plot |
| **Show Assumption Diagnostics** | On/Off | Overlays per-group (solid) and pooled within-group (dashed) covariance ellipses; if they match, the equal-covariance assumption holds |
| **Show Decision Boundaries** | On/Off (default On) | Shades the LD space by predicted class region and draws boundary contour lines |
| **Width / Height (cm)** | Numeric | Export dimensions for SVG and PNG downloads |

The **Variable Contributions** jitter plot (visible when discriminant coefficients are available) displays the absolute discriminant coefficient for each variable across all LD axes. Variables with consistently large coefficients are the primary drivers of group separation.

##### Best Practices

- **Start with LDA** — use QDA or MDA only when you have evidence that the equal-covariance assumption is violated or group shapes are clearly non-elliptical
- **Scale & Center by default** — essential for mixed-unit data; omit only when all variables share the same unit and variance is meaningful
- **Use PCA scores for high-dimensional data** — when p approaches n per group, run PCA first and use the PCA scores (≥ 90% variance) as LDA input (Ripley, 1996)
- **Compare resubstitution vs. CV accuracy** — a gap > 10% suggests overfitting; reduce p or switch to PCA-based input
- **Inspect the Proportion of Trace first** — if LD1 captures < 50%, examine higher axes; two-dimensional plots may miss important separation
- **Enable diagnostics overlay** — covariance ellipsis mismatch between per-group and pooled estimates is the key visual test for the LDA equal-covariance assumption
- **Download full results** — the Excel export contains the full proportion of trace, discriminant coefficients, posterior probabilities, and per-class accuracy for reporting

**References**

- Fisher, R. A. (1936). The use of multiple measurements in taxonomic problems. *Annals of Eugenics*, 7(2), 179–188.
- Fraley, C., & Raftery, A. E. (2002). Model-based clustering, discriminant analysis, and density estimation. *Journal of the American Statistical Association*, 97(458), 611–631.
- Hastie, T., & Tibshirani, R. (1996). Discriminant analysis by Gaussian mixtures. *Journal of the Royal Statistical Society: Series B*, 58(1), 155–176.
- Hastie, T., Tibshirani, R., & Friedman, J. (2009). *The Elements of Statistical Learning* (2nd ed.). Springer.
- McLachlan, G. J. (2004). *Discriminant Analysis and Statistical Pattern Recognition*. Wiley.
- Peterson, R. A., & Cavanaugh, J. E. (2020). Ordered quantile normalization. *Journal of Applied Statistics*, 47(13-15), 2312–2327.
- Mitteroecker, P., & Gunz, P. (2009). Advances in geometric morphometrics. *Evolutionary Biology*, 36(2), 235–247.
- Ripley, B. D. (1996). *Pattern Recognition and Neural Networks*. Cambridge University Press.
- Venables, W. N., & Ripley, B. D. (2002). *Modern Applied Statistics with S* (4th ed.). Springer.
- Zelditch, M. L., Swiderski, D. L., & Sheets, H. D. (2012). *Geometric Morphometrics for Biologists* (2nd ed.). Academic Press.
