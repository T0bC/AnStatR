#### LDA / QDA / MDA / PLS-DA / sPLS-DA — Technical Reference

##### Requirements

**Data Structure**

| Requirement | LDA | QDA | MDA | PLS-DA / sPLS-DA |
|-------------|-----|-----|-----|-------------------|
| **Min. observations per group** | > p (warning if violated) | ≥ p + 1 (hard error) | ≥ max(subclasses, p + 1) | none — designed for n < p |
| **Min. groups** | 2 | 2 | 2 | 2 |
| **Data type** | Numeric only | Numeric only | Numeric only | Numeric only |
| **Missing values** | Rows with NAs excluded automatically | same | same | same |
| **Max. discriminant axes / components** | min(p, G − 1) | none (classification only) | min(p, G − 1) | user-set (up to min(n − 1, p)) |
| **Handles collinear variables** | No (may fail with singular matrix) | No | No | Yes — built into the algorithm |
| **Built-in variable selection** | No | No | No | sPLS-DA only (via keepX) |

Where p = number of measurement variables, G = number of groups, n = number of observations.

**When to reach for PLS-DA/sPLS-DA instead of LDA/QDA/MDA**: if you have more measurement variables than specimens (a common situation with 40+ computed 3D surface-texture parameters and a handful of specimens per group), or if your variables are highly collinear (e.g., a 2D and 3D version of a similar surface feature), LDA/QDA/MDA will warn or fail outright. PLS-DA handles both situations natively, and sPLS-DA additionally performs sparse variable selection — directly answering "which of my 40+ parameters actually drive the group differences?"

**Metadata and Grouping Columns**

- **Descriptive (metadata) columns** are carried through the analysis purely for labelling. They appear in the scores plot tooltips, in the exported results tables, and on the axes of the LD Scores Plot. They do not influence the discriminant function in any way
- **Grouping column** must be a categorical variable selected from the metadata columns. It defines the class labels fed to MASS::lda() / MASS::qda() / mda::mda(). Rows with missing values in the grouping column are dropped

**Recommended parameters (parameter screening mode)**

When the **Statistics** tab has computed a parameter screening ranking (see the Plotting tab's **Disable plots (parameter screening mode)**), an **Apply recommended parameters** banner appears above the measurement-column selector. It applies the union of parameters that ranked among the top group separators across all pairwise comparisons. This is purely a selection shortcut — clicking it calls the same column-selection mechanism as manual selection, so recommended and manually chosen columns are treated identically by the LDA/QDA/MDA computation and by the saved `.rds` bundle (see the **Prediction** module for how the bundle's `numeric_cols` are used to align new data). See the **Statistics** module's Details tab for how the ranking is computed.

##### Technical Specifications

<details>
<summary><strong>LDA Computation Method</strong></summary>

LDA is implemented via `MASS::lda()`. The algorithm finds the projection matrix $\mathbf{W}$ that maximises the ratio of between-group scatter $\mathbf{S}_B$ to within-group scatter $\mathbf{S}_W$:

$$\mathbf{W} = \underset{\mathbf{W}}{\arg\max} \frac{|\mathbf{W}^\top \mathbf{S}_B \mathbf{W}|}{|\mathbf{W}^\top \mathbf{S}_W \mathbf{W}|}$$

This is solved as a generalised eigenvalue problem. The resulting eigenvectors are the discriminant coefficients (the **scaling** matrix); the eigenvalues (accessed as SVD singular values) determine how much between-group variance each axis explains.

The number of usable discriminant axes is $\min(p,\, G - 1)$. With two groups there is exactly one LD axis; additional groups yield additional axes.

</details>

<details>
<summary><strong>QDA Computation Method</strong></summary>

QDA is implemented via `MASS::qda()`. Unlike LDA, QDA estimates a separate covariance matrix $\boldsymbol{\Sigma}_k$ for each group $k$. The quadratic discriminant function for group $k$ is:

$$\delta_k(\mathbf{x}) = -\tfrac{1}{2} \log|\boldsymbol{\Sigma}_k| - \tfrac{1}{2} (\mathbf{x} - \boldsymbol{\mu}_k)^\top \boldsymbol{\Sigma}_k^{-1} (\mathbf{x} - \boldsymbol{\mu}_k) + \log \pi_k$$

Because each group needs to invert its own $p \times p$ covariance matrix, at least $p + 1$ observations per group are required. QDA does not produce discriminant axes; the companion LDA fit (fitted automatically on the same data) is used for LD-space visualisation only.

</details>

<details>
<summary><strong>MDA Computation Method</strong></summary>

MDA is implemented via `mda::mda()`. Each group is modelled as a mixture of `subclasses` Gaussian sub-populations:

$$P(\mathbf{x} \mid \text{group}\, k) = \sum_{r} \pi_{kr}\, \mathcal{N}(\mathbf{x};\, \boldsymbol{\mu}_{kr},\, \boldsymbol{\Sigma})$$

Parameters are estimated by the Expectation–Maximisation (EM) algorithm. Because a shared (pooled) covariance matrix is used across all sub-populations, MDA generalises LDA to non-elliptical group shapes without the per-group covariance requirement of QDA.

Key MDA settings:
- **Subclasses per group** — number of Gaussian components per class (default 3; set to 1 to recover standard LDA behaviour)
- **Max EM iterations** — convergence limit for the EM algorithm (increase to 20–50 if deviance is still decreasing)

</details>

<details>
<summary><strong>PLS-DA Computation Method</strong></summary>

PLS-DA (Partial Least Squares Discriminant Analysis) is implemented via `mixOmics::plsda()`. Unlike LDA, which inverts a within-group covariance matrix, PLS-DA finds latent components that maximise the covariance between the measurement variables $\mathbf{X}$ and a dummy-coded group-membership matrix $\mathbf{Y}$:

$$\mathbf{t}, \mathbf{u} = \underset{\mathbf{t} = \mathbf{X}\mathbf{w},\, \mathbf{u} = \mathbf{Y}\mathbf{c}}{\arg\max}\ \mathrm{cov}(\mathbf{t}, \mathbf{u})$$

Components are extracted iteratively via the NIPALS algorithm, deflating $\mathbf{X}$ after each component so subsequent components capture orthogonal, complementary information. Because this never requires inverting a $p \times p$ matrix, PLS-DA remains well-defined when $p > n$ and is not disrupted by collinear predictors — collinear variables simply share loading weight on the same component(s) rather than causing a singular-matrix failure.

The number of components is chosen by the user (**Number of components** setting), defaulting to $G - 1$ to match LDA's discriminant-axis count, but any value up to $\min(n-1,\ p)$ is valid. Use the **Check component count** button (Analysis Settings sidebar) to check whether the chosen count is actually justified by cross-validated classification error.

</details>

<details>
<summary><strong>sPLS-DA Computation Method</strong></summary>

sPLS-DA (sparse PLS-DA) is implemented via `mixOmics::splsda()`. It extends PLS-DA with an $\ell_1$ (LASSO-style) penalty on the component loadings, forcing all but a fixed number of variables to exactly zero on each component:

$$\mathbf{w} = \underset{\mathbf{w}}{\arg\max}\ \mathrm{cov}(\mathbf{X}\mathbf{w}, \mathbf{u}) \quad \text{subject to } \lVert \mathbf{w} \rVert_1 \le \lambda,\ \lVert \mathbf{w} \rVert_2 = 1$$

The **Variables to keep per component (keepX)** setting controls sparsity directly — rather than tuning $\lambda$, mixOmics lets you specify exactly how many variables should retain a nonzero loading on each component. The variables with the largest loading magnitude survive; the rest are set to zero and excluded from that component's score computation entirely. This is the mechanism behind the **Selected Variables** results panel: the parameters most responsible for separating your groups on each component.

**Choosing keepX**: keepX is a *count of variables*, not a threshold — it answers "how many parameters should this component be allowed to keep?" Smaller values give a shorter, more interpretable variable list but risk excluding weaker real contributors; larger values behave closer to standard PLS-DA and stop producing a useful shortlist. There is no universally correct value — it is a trade-off between interpretability and completeness, and it should be decided by cross-validation rather than guessed. Until tuning has been run, the **Selected Variables** panel is marked **keepX not tuned**, because the variable list then reflects a sidebar default rather than the data. The **Optimise variable selection (recommended)** button runs `mixOmics::tune.splsda()`, a cross-validated grid search over candidate keepX values per component, and fills in the value that minimises balanced classification error. Because this repeats model fitting across a grid × folds × repeats, it can take from several seconds to a few minutes depending on data size — the suggested values are a starting point you can still edit by hand.

**Multicollinearity and variable selection**: when several measurement columns essentially describe the same underlying surface feature at different scales or in 2D vs. 3D, sparse selection may pick one representative more or less arbitrarily and assign the others a zero loading — this does not mean the excluded variables are scientifically irrelevant, only that they were redundant given the ones already selected. When interpreting the Selected Variables list, group correlated parameters conceptually and treat the "selected" one as representative of that group rather than uniquely important. Cross-check with the PCA Correlation Matrix (or a correlation heatmap of your parameter set) to identify which variables cluster together before drawing conclusions about "unimportant" excluded parameters.

</details>

<details>
<summary><strong>Component Diagnostics (perf) for PLS-DA/sPLS-DA</strong></summary>

The **Component Diagnostics (perf)** panel (Analysis Settings sidebar → **Check component count**) wraps `mixOmics::perf()`, which repeats k-fold cross-validation (default 5 folds × 10 repeats) to estimate classification error at each component count, independently of the model fitted by the main **Compute** button. Two error metrics are reported per component:

| Column | Definition |
|--------|-----------|
| **Overall Error** | Fraction of all cross-validated predictions that were misclassified |
| **BER** (Balanced Error Rate) | Average of per-group error rates — more informative than Overall Error when groups are unevenly sized |

Both are computed using the `max.dist` classification rule (assign to the class with maximum predicted score). Look for the component count where error stops decreasing meaningfully (an "elbow") — adding components beyond that point usually adds noise, not signal, to the model. This is the PLS-DA/sPLS-DA-specific analogue of comparing resubstitution vs. LOO-CV accuracy for LDA: it is deliberately separated from the main Validation setting (None / Train-Test Split) because it evaluates *component count*, not the final fitted model's generalisation.

**Note**: PLS-DA/sPLS-DA does not support Leave-one-out CV as a Validation option (unlike LDA/QDA/MDA) because a full model refit per left-out observation would be prohibitively slow at typical component/keepX settings; the perf() panel's repeated k-fold CV is the standard validation approach for this method family in the literature.

**Selected Variable Stability (sPLS-DA only)**: below the error-rate table, a second table reports how often each selected variable was chosen across the CV folds/repeats (`mixOmics::perf()`'s `$features$stable` output). A variable selected in nearly every fold (frequency close to 1.0) is a robust, reproducible finding; a variable selected in only a few folds was likely chosen due to that fold's specific data split rather than a genuine, stable association with the groups. This is the direct empirical check for the caveat raised in the Selected Variables panel and the FAQ about selection instability at the margins — run this diagnostic before treating a borderline selected variable as a confident scientific conclusion.

</details>

<details>
<summary><strong>Using PCA Scores as LDA Input</strong></summary>

*This section applies to LDA / QDA / MDA. PLS-DA/sPLS-DA already handle high-dimensional, collinear raw measurements directly and do not need this workaround — running them on PCA scores instead of raw variables discards the "which original parameter matters" interpretability that is usually the reason for choosing PLS-DA in the first place.*

The **Data Source** toggle in the Data Selection tab allows LDA / QDA / MDA to be run on **PCA scores** (the individual coordinates from a prior PCA run) instead of the raw measurements. This two-stage approach is well established in morphometrics and texture analysis.

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

</details>

<details>
<summary><strong>Scaling Implications for LDA</strong></summary>

Scaling decisions directly affect the within-group and between-group scatter matrices and therefore which variables drive the discriminant axes:

| Scaling | Covariance Structure | Effect on LDA | Recommended When |
|---------|---------------------|---------------|-----------------|
| **Scale & Center** | Correlation matrix | All variables contribute equally to $\mathbf{S}_W$; discriminant coefficients are comparable across variables | Variables have different units (mm, %, counts, etc.) |
| **Center only** | Covariance matrix | High-variance variables exert stronger influence on discriminant directions | All variables share the same unit and variance differences are scientifically meaningful |
| **No scaling** | Raw cross-products | Raw scale dominates; variables with large absolute values can monopolise LD axes | Data already preprocessed to a common scale |

**Critical note**: Because LDA computes a ratio of scatter matrices, variables with very large raw variance can render other variables effectively invisible even if they carry genuine group-discriminating information. **Scale & Center is strongly recommended** for mixed-unit data. Scaling does not need to be applied when using PCA scores as input — the PCA step has already standardized the feature space.

</details>

<details>
<summary><strong>Data Normalisation</strong></summary>

The **Normalize skewed variables** option uses the `bestNormalize` package to transform variables with |skewness| > 2 before analysis. Candidate transformations include Box-Cox, Yeo-Johnson, log, and square-root. The transformation that best achieves normality (assessed by the Pearson P/df statistic) is selected automatically.

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
| **Leave-one-out CV** | Each specimen predicted by a model trained on all others (MASS::lda/qda CV=TRUE; manual loop for MDA) | Conservative for small datasets; computationally intensive for MDA; **not available for PLS-DA/sPLS-DA** — use Component Diagnostics (perf) instead |
| **Train / Test Split** | Stratified random split; holdout set accuracy | Single-split variance; reproducible via **Random seed** |

**Resubstitution accuracy** is always reported in the results panel. When LOO-CV or Train/Test Split is used, the cross-validated or test-set accuracy is reported alongside it. For PLS-DA/sPLS-DA, component-count validation is handled separately by the **Component Diagnostics (perf)** panel (see above) rather than by the Validation setting.

##### Data Interpretation — Results Panels

The results accordion — titled after the selected method (**LDA Results**, **sPLS-DA Results**, and so on) — contains a variable number of sub-panels depending on analysis type and validation mode. The panels appear in the order described below.

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

Shows the within-group mean for every measurement variable, computed on the (scaled) data used for analysis. Rows are variables; columns are groups — datasets normally have far more measurement variables than groups, so this orientation keeps the table narrow and paginates the variables instead of scrolling sideways. (The Excel export uses the opposite orientation, groups as rows, for spreadsheet convenience.)

These are the centroid coordinates that LDA/QDA uses as the reference points for classification. Key uses:

- **Identify which variables distinguish groups**: large mean differences across groups on a variable signal strong discriminating potential on that variable
- **Interpret discriminant axes**: if LD1 has large positive coefficients for variable X and group A has the highest mean on X, group A will plot at the positive end of LD1
- **Verify scaling effect**: with Scale & Center applied, all means are in standard deviation units (z-scores) and are directly comparable across variables

For QDA and MDA the group means have the same interpretation, but the within-group covariance structure differs between methods.

</details>

<details>
<summary><strong>Coefficients of Linear Discriminants / Discriminant Coefficients / Component Loadings</strong></summary>

*Available for LDA, MDA, and PLS-DA/sPLS-DA (model mode only); not shown for QDA.*

The table lists the discriminant coefficients or component loadings (the **scaling matrix**): how much each variable contributes to each axis. Rows are variables; columns are LD1, LD2, … (LDA), DC1, DC2, … (MDA), or Comp1, Comp2, … (PLS-DA/sPLS-DA).

After z-score scaling the coefficients are on a common scale and directly comparable:

| Coefficient magnitude | Interpretation |
|-----------------------|---------------|
| Large positive | Variable pulls specimens towards the positive end of that axis |
| Large negative | Variable pulls specimens towards the negative end of that axis |
| Near zero | Variable contributes little to separation on that axis |

To identify the primary discriminating variables: look for the rows with the largest absolute values in LD1 (or Comp1). These are the measurements that most strongly separate the groups along the first (and usually most important) axis.

The **Variable Contributions** jitter plot (separate accordion panel below the results panel) visualises these coefficients across all axes simultaneously — variables with consistently large absolute values across multiple axes are the overall key discriminators.

For MDA, the coefficients describe the shared pooled discriminant space across all mixture components; their interpretation is analogous to LDA coefficients. **For PLS-DA/sPLS-DA, loadings are on a different mathematical footing than LDA discriminant coefficients** — they describe how strongly each variable contributes to a component that jointly maximises covariance with group membership, not a ratio of between/within-group scatter. The *relative ranking* of variables by absolute loading within a component is still meaningful for identifying key drivers, but the absolute values are not directly comparable to LDA coefficients or to loadings from a different PLS-DA fit. For sPLS-DA specifically, variables with a zero loading on a given component were excluded by sparse selection entirely (see the **Selected Variables** panel), not merely judged unimportant.

</details>

<details>
<summary><strong>VIP Scores (PLS-DA/sPLS-DA only)</strong></summary>

*Shown for both PLS-DA and sPLS-DA model fits — the standard companion to Component Loadings for answering "which variables discriminate the groups?"*

Variable Importance in Projection (VIP), computed via `mixOmics::vip()`, aggregates each variable's contribution **across all fitted components at once**, weighted by how much of the variance in group membership each component explains. This differs from the Component Loadings table, which reports one coefficient per variable *per component* — VIP instead gives a single importance ranking for the whole model.

| VIP value | Interpretation |
|-----------|---------------|
| > 1 (highlighted green) | Above-average contributor to the model's overall group separation — the conventional threshold used in the PLS-DA/sPLS-DA literature |
| ≈ 1 | Average importance |
| < 1 | Below-average contributor |

**Why VIP matters for plain PLS-DA specifically**: unlike sPLS-DA, plain PLS-DA never sets any loading to exactly zero — every variable contributes at least a little to every component, so the Component Loadings table alone cannot tell you which variables are negligible. VIP > 1 filtering is the standard way researchers narrow down a PLS-DA loadings table to a shortlist of variables worth reporting.

**For sPLS-DA**: VIP is computed on the already-sparse fitted model, so a variable with zero loading on every component (never selected — see Selected Variables) will show VIP = 0. VIP still adds value here by aggregating a variable's importance across multiple components into one number, which the per-component Selected Variables list does not do.

**Reading this table alongside Component Loadings and Selected Variables**: VIP tells you *how important* a variable is overall; Component Loadings tell you *which component(s)* and *in which direction* (sign); Selected Variables (sPLS-DA only) tells you *whether* sparse selection kept it at all. Use all three together rather than any single one in isolation.

</details>

<details>
<summary><strong>Selected Variables (sPLS-DA only)</strong></summary>

*Shown only for sPLS-DA model fits.*

Lists, per component, the measurement columns that survived sparse selection (nonzero loading) — the direct answer to "which of my 40+ parameters actually drive the group differences?" A variable appearing under multiple components is contributing to separation along more than one axis of the group structure.

**Reading this list scientifically**:
- Treat it as a starting hypothesis for which measured surface features matter, not a final proof — sparse selection is sensitive to the chosen keepX and to which correlated variable "wins" among near-duplicates
- Cross-reference selected variables against their Component Loadings sign and magnitude (above) to understand *how* each one relates to group differences, not just *that* it was selected
- If a variable you expected to be important is missing, check whether a highly correlated sibling variable was selected instead — inspect the PCA Correlation Matrix for that variable's correlation partners before concluding it is irrelevant
- Re-running with a different keepX or a different random seed's cross-validation fold assignment can shift the selection at the margins; variables selected consistently across several settings are the more robust candidates for follow-up (e.g., univariate group comparisons, targeted biological/archaeological interpretation)

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
<summary><strong>Proportion of Trace / Explained Variance</strong></summary>

*Available for LDA, MDA, and PLS-DA/sPLS-DA (model mode only); not shown for QDA. Titled "Explained Variance" for PLS-DA/sPLS-DA.*

The primary summary of discriminant axis / component importance, directly analogous to the variance-explained table in PCA. Columns:

| Column | Definition | Interpretation |
|--------|-----------|----------------|
| **LD / DC / Comp** | Axis label — LD1, LD2, … for LDA; DC1, DC2, … for MDA; Comp1, Comp2, … for PLS-DA/sPLS-DA | Axes are ranked by discriminating power, the first axis always largest |
| **Singular Value** | Square root of the corresponding eigenvalue (LDA only) | Larger → stronger between-group separation on that axis |
| **Proportion** | Fraction of variance explained by this axis (between-group variance for LDA/MDA; variance in X explained by the component for PLS-DA/sPLS-DA) | Values sum to at most 1.0 |
| **Cumulative** | Running sum of proportions | Background colour: grey < 0.6, yellow 0.6–0.8, green > 0.8 |

**Reading the table**: If LD1/Comp1 proportion > 0.90, a single scatter plot of that axis captures the overwhelming majority of group separation. If the proportion is split more evenly (e.g., 0.69 / 0.31), both axes carry substantial discriminating information and the two-dimensional Scores Plot should be examined carefully. The cumulative column reaching green (> 0.80) indicates that the axes up to that row together explain most of the relevant variance. **For PLS-DA/sPLS-DA**, unlike LDA's Proportion of Trace, the values here describe variance explained *in the measurement variables* by each component (not strictly between-group variance) — a low proportion does not necessarily mean weak group separation on that component; cross-check against the Component Diagnostics (perf) error rate and the Dimension Evaluation (ANOVA) table below for a group-separation-specific view.

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

**Which axes should I plot?** R² answers this directly: it ranks the axes by how much group separation each one carries. The panel therefore ends with a **Best axes to plot** recommendation naming the two highest-R² axes. If those are not the first two, the Scores Plot is not showing your clearest separation — set **Dim.X** and **Dim.Y** in the Plotting Controls tab to the recommended axes. This matters most for PLS-DA/sPLS-DA with several components, where the strongest group signal is not always on Comp1/Comp2 and an overlapping default plot can otherwise be mistaken for a genuine lack of separation.

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

**Download Excel (All Results)** — an `.xlsx` workbook with one sheet per result component. Which sheets appear depends on the analysis type and validation mode:

| Sheet | Written when |
|-------|--------------|
| **LD Scores** | LDA, MDA, PLS-DA, sPLS-DA — scores with metadata columns prepended |
| **Classification** | QDA, or cross-validation mode (posterior probabilities + predicted class, in place of LD Scores) |
| **Prior Probabilities** | Prior probabilities are available (not for PLS-DA/sPLS-DA) |
| **Group Means** | Group means are available |
| **LD Coefficients** | Discriminant coefficients / component loadings are available |
| **VIP Scores** | PLS-DA and sPLS-DA only |
| **Selected Variables** | sPLS-DA only — the variables retained per component |
| **Selected Variable Stability** | sPLS-DA only, and only if **Run perf()** was executed |
| **Proportion of Trace** | Proportion of trace / explained variance is available |
| **Confusion Matrix** | A confusion matrix is available |
| **Per-Class Metrics** | Alongside the confusion matrix |
| **Posterior Probabilities** | LDA, MDA, PLS-DA, sPLS-DA (QDA reports these in the Classification sheet instead) |
| **Subclass Priors** | MDA only — per-group subclass prior weights |
| **Split Summary** | Train/test split mode only |

The first sheet (scores, or posteriors with metadata for QDA/CV) is ready for import into the Cluster module or for external analysis.

**Download RDS (LDA/QDA Object)** — an `.rds` file containing the full result bundle including the fitted model object, raw and scaled data, transformation parameters, scale parameters, and all settings. Load in R with `readRDS()` for programmatic access to the model or for reproducibility documentation.

</details>

##### Plotting Controls

Configure the **LD Scores Plot** (titled **Component Scores Plot** for PLS-DA/sPLS-DA) in the **Discriminant Analysis Plotting Controls** sidebar tab:

| Control | Options | Effect |
|---------|---------|--------|
| **Dim.X / Dim.Y** | LD1, LD2, … (LDA/MDA), Comp1, Comp2, … (PLS-DA/sPLS-DA), or original variables (QDA) | Select which discriminant axes/components map to the plot axes |
| **Dim.Z** | Same choices as X/Y | Reserved for future 3D discriminant plot |
| **Show Assumption Diagnostics** | On/Off — hidden for PLS-DA/sPLS-DA | Overlays per-group (solid) and pooled within-group (dashed) covariance ellipses; if they match, the equal-covariance assumption holds. Not applicable to PLS-DA/sPLS-DA, which make no Gaussian equal-covariance assumption |
| **Show Decision Boundaries** | On/Off (default On) | Shades the plotted space by predicted class region and draws boundary lines. Computed exactly for LDA and QDA. For PLS-DA/sPLS-DA, computed exactly when plotting Comp1 vs Comp2 — the same `max.dist` classification rule mixOmics uses internally (`predict()`/`background.predict()`), evaluated directly in component space; for any other component pair, or for MDA, approximated via nearest-neighbour classification on the training scores (mixOmics' own classification integrates information from all components in original-variable space, which cannot be reduced to a closed-form boundary outside the fitted Comp1/Comp2 pair) |
| **Width / Height (cm)** | Numeric | Export dimensions for SVG and PNG downloads |

The **Variable Contributions** jitter plot (visible when discriminant coefficients or component loadings are available) displays the absolute coefficient/loading for each variable across all axes. Variables with consistently large values are the primary drivers of group separation.

##### Best Practices

- **Start with LDA** — use QDA or MDA only when you have evidence that the equal-covariance assumption is violated or group shapes are clearly non-elliptical
- **Switch to PLS-DA/sPLS-DA when p ≥ n per group or variables are collinear** — this is the situation LDA/QDA/MDA cannot handle gracefully; PLS-DA/sPLS-DA are designed for exactly this data shape
- **Use sPLS-DA (not plain PLS-DA) when the goal is variable selection** — plain PLS-DA still reduces dimensionality and separates groups, but sPLS-DA's Selected Variables panel is what directly answers "which parameters matter"
- **Scale & Center by default** — essential for mixed-unit data; omit only when all variables share the same unit and variance is meaningful
- **Use PCA scores for high-dimensional data with LDA/QDA/MDA** — when p approaches n per group and you are staying with LDA/QDA/MDA, run PCA first and use the PCA scores (≥ 90% variance) as input. Prefer PLS-DA/sPLS-DA directly on raw variables instead if identifying original measurement parameters is the goal
- **Compare resubstitution vs. CV accuracy** — a gap > 10% suggests overfitting; for LDA/QDA/MDA reduce p or switch to PCA-based input; for PLS-DA/sPLS-DA check the Component Diagnostics (perf) panel for the component count that minimises cross-validated error
- **Inspect the Proportion of Trace / Explained Variance first** — if LD1/Comp1 captures < 50%, examine higher axes; two-dimensional plots may miss important separation
- **Enable diagnostics overlay (LDA/QDA/MDA only)** — covariance ellipsis mismatch between per-group and pooled estimates is the key visual test for the equal-covariance assumption
- **For sPLS-DA, treat correlated variable groups together** — do not conclude an excluded variable is scientifically unimportant without checking whether a correlated sibling was selected in its place
- **Download full results** — the Excel export contains the full proportion of trace / explained variance, discriminant coefficients or component loadings (plus selected variables for sPLS-DA), posterior probabilities, and per-class accuracy for reporting

