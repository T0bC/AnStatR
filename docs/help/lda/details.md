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

LDA is implemented via `MASS::lda()` (Venables & Ripley, 2002). The algorithm finds the projection matrix **W** that maximises the ratio of between-group scatter **S_B** to within-group scatter **S_W**:

**W** = argmax |**W**ᵀ **S_B** **W**| / |**W**ᵀ **S_W** **W**|

This is solved as a generalised eigenvalue problem. The resulting eigenvectors are the discriminant coefficients (the **scaling** matrix); the eigenvalues (accessed as SVD singular values) determine how much between-group variance each axis explains.

The number of usable discriminant axes is min(p, G − 1). With two groups there is exactly one LD axis; additional groups yield additional axes.

**References**: Fisher (1936); McLachlan (2004); Venables & Ripley (2002).

</details>

<details>
<summary><strong>QDA Computation Method</strong></summary>

QDA is implemented via `MASS::qda()` (Venables & Ripley, 2002). Unlike LDA, QDA estimates a separate covariance matrix Σ_k for each group k. The quadratic discriminant function for group k is:

δ_k(x) = −½ log|Σ_k| − ½ (x − μ_k)ᵀ Σ_k⁻¹ (x − μ_k) + log π_k

Because each group needs to invert its own p × p covariance matrix, at least p + 1 observations per group are required. QDA does not produce discriminant axes; the companion LDA fit (fitted automatically on the same data) is used for LD-space visualisation only.

**References**: Hastie, Tibshirani & Friedman (2009); Venables & Ripley (2002).

</details>

<details>
<summary><strong>MDA Computation Method</strong></summary>

MDA is implemented via `mda::mda()` (Hastie & Tibshirani, 1996). Each group is modelled as a mixture of `subclasses` Gaussian sub-populations:

P(x | group k) = Σ_r π_{kr} · N(x; μ_{kr}, Σ)

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

- **Eliminates collinearity** — PCA components are orthogonal by construction. Because LDA's within-group covariance matrix Σ_W must be invertible, highly correlated raw variables frequently cause singularity errors. PCA scores are always full-rank up to the number of retained components
- **Reduces dimensionality (p < n/G)** — when the number of raw variables p approaches or exceeds the number of observations per group n/G, discriminant functions overfit. Retaining only the PCA dimensions that explain ≥ 90% of variance substantially reduces p while preserving most of the data structure
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
| **Scale & Center** | Correlation matrix | All variables contribute equally to Σ_W; discriminant coefficients are comparable across variables | Variables have different units (mm, %, counts, etc.) |
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

##### Data Interpretation

<details>
<summary><strong>Proportion of Trace (LDA and MDA)</strong></summary>

The **Proportion of Trace** table is the primary summary of discriminant axis importance, analogous to the variance-explained table in PCA:

| Column | Definition | Interpretation |
|--------|-----------|----------------|
| **LD / DC** | Axis label (LD = LDA axes, DC = MDA discriminant coordinates) | Axes are ranked by discriminating power |
| **Singular Value** | Square root of the eigenvalue (LDA only) | Larger values → stronger group separation on that axis |
| **Proportion** | Fraction of total between-group variance explained by this axis | Values sum to 1.0 across all axes |
| **Cumulative** | Running total | Retain axes until cumulative proportion ≥ 0.80–0.90 for interpretation |

**Key insight**: If LD1 explains > 90% of the trace, a single axis captures most group separation. If proportions are distributed across several axes, group discrimination is multi-dimensional and requires examining multiple plots.

</details>

<details>
<summary><strong>Classification Accuracy and Confusion Matrix</strong></summary>

The **Confusion Matrix** cross-tabulates true group labels against predicted labels. Per-class metrics are reported alongside overall accuracy:

| Metric | Definition | Interpretation |
|--------|-----------|----------------|
| **Overall Accuracy** | Correct / Total | Green ≥ 90%, Yellow ≥ 70%, Red < 70% |
| **Precision** | TP / (TP + FP) | What fraction of predicted-class specimens actually belong to it |
| **Recall** | TP / (TP + FN) | What fraction of true-class specimens were correctly predicted |
| **F1** | 2 × Precision × Recall / (Precision + Recall) | Harmonic mean; useful for imbalanced groups |

**Resubstitution accuracy** (no cross-validation) is always optimistic — the model was trained on the same data it classifies. For genuine predictive evaluation, use LOO-CV or Train/Test Split. A large gap between resubstitution and CV accuracy indicates overfitting, commonly caused by too many variables relative to observations (p ≥ n per group).

</details>

<details>
<summary><strong>Posterior Probabilities</strong></summary>

For each specimen the model reports a posterior probability P(group | x) for every group. These are derived via Bayes' theorem using the discriminant functions and the selected prior probabilities. The specimen is assigned to the group with the highest posterior.

High posterior probability (> 0.90) for the assigned group indicates confident classification. Specimens with roughly equal posteriors across two or more groups are ambiguous — they sit near decision boundaries and are most likely to be misclassified. Inspect these in the LD Scores Plot by hovering over the interactive points.

</details>

<details>
<summary><strong>Discriminant Coefficients (Scaling Matrix)</strong></summary>

The **Discriminant Coefficients** table (available for LDA and MDA) lists the weight each variable receives on each discriminant axis. After z-score scaling, these coefficients are directly comparable:

- Large absolute coefficient → variable contributes strongly to group separation on that axis
- Variables with near-zero coefficients on all axes are not discriminating the groups in this dataset
- The **Variable Contributions** jitter plot visualises these coefficients across all axes simultaneously

For QDA, no discriminant coefficients exist — classification is based on posterior probabilities from per-group covariance matrices.

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
