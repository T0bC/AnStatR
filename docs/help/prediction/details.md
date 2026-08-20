#### Prediction — Technical Reference

##### Requirements

**Model Bundle**

The bundle file must be an `.rds` object exported directly from the PCA, LDA, QDA, MDA, PLS-DA/sPLS-DA, or Cluster tab of this application. It must contain all of the following fields:

| Field | Content |
|-------|---------|
| `analysis_type` | One of `pca`, `spca`, `ipca`, `lda`, `qda`, `mda`, `plsda`, `splsda`, `cluster` |
| `model` | The fitted model object (`mixOmics::pca`/`spca`/`ipca`, `MASS::lda`/`qda`, `mda::mda`, `mixOmics::plsda`/`splsda`, `stats::kmeans`, or `cluster::pam`) |
| `numeric_cols` | Character vector of measurement column names used during training |
| `raw_data` | Training data before preprocessing (used for range validation) |
| `used_data` | Training data after preprocessing (used for overlay plots) |
| `scale_params` | Center and scale vectors — stored for every analysis type, including PCA/sPCA/IPCA |
| `transform_params` | Stored skewness transformation parameters or empty list |
| `app_version` | Version of AnStatR that created the bundle |
| `created` | Timestamp of bundle creation |

Cluster bundles additionally store `variant` (`kmeans` or `pam`), `cluster_metric` (`euclidean` or `manhattan`), `n_clusters`, and `cluster_labels` (the training-set cluster assignments, needed to redraw the training biplot since clustering has no `predict()` to recompute them from).

Bundles exported from external R sessions or other tools will not be accepted unless they conform to this structure. Bundles exported in cross-validation (CV) mode do not include a fitted model object and cannot be used for prediction. Cluster bundles are only exportable for **K-Means or PAM fit on raw measurement data** — see the Cluster module's Details tab for why Hierarchical, DBSCAN, and score-sourced clusters are excluded.

**Unknown Data**

- Format: CSV or XLSX (`.xlsx`, first sheet only)
- Must contain **all** `numeric_cols` listed in the bundle — column names are matched exactly (case-sensitive)
- Measurement columns must be numeric. Text or factor columns with the same names as required measurement columns will cause a validation error
- Metadata / label columns (e.g., specimen IDs, site codes) are optional but must not share names with required measurement columns unless they are numeric

**Reference Population Requirement**

The most important non-technical requirement is methodological: the unknown specimens must belong to the same reference population as the training data. In the context any analysis, this means specimens were acquired using the same measurement instrument, protocol, and analytical conditions. Applying a model trained on one reference collection to unknowns measured under different conditions introduces systematic bias that inflates out-of-range warnings, distorts posterior probabilities, and may cause misclassification even if no validation error is raised.

##### Technical Specifications

<details>
<summary><strong>Preprocessing Pipeline for Unknown Data</strong></summary>

Before prediction, the unknown data is transformed using parameters stored in the bundle — not re-estimated from the unknown data itself. This is essential for valid prediction: the same transformation that was applied to training data must be applied to new data.

**Step 1 — Skewness transformations** (`transform_params`)

If skewness normalization was enabled during model training, `bestNormalize` transformation objects are stored in the bundle. The same transformations (Box-Cox, Yeo-Johnson, log, square-root — whichever was selected per column) are applied to the corresponding unknown data columns using `predict()` on the stored transformer objects.

**Step 2 — Scaling** (all analysis types)

The stored `center` and `scale` vectors are applied to the transformed unknown data:

$$x'_{ij} = \frac{x_{ij} - \bar{x}_{j,\text{train}}}{s_{j,\text{train}}}$$

where $\bar{x}_{j,\text{train}}$ and $s_{j,\text{train}}$ are the training mean and standard deviation for column $j$.

**PCA / sPCA / IPCA use the same manual step**: mixOmics' `pca()`/`spca()`/`ipca()` objects have no `predict()` S3 method, so unlike `stats::prcomp`, there is no automatic centering/scaling on projection. The stored `center`/`scale` vectors are applied to PCA/sPCA/IPCA unknown data via the exact same Step 2 formula above as LDA/MDA/QDA/Cluster — there is no special case for PCA.

This design ensures that the preprocessing pipeline is fully reproducible and that the unknown data occupies the same feature space as the training data.

</details>

<details>
<summary><strong>Prediction Methods by Analysis Type</strong></summary>

LDA/MDA/QDA/PLS-DA/sPLS-DA dispatch through R's generic `stats::predict()` applied to the stored model object. PCA/sPCA/IPCA have no `predict()` S3 method in mixOmics and are projected manually via matrix multiplication instead.

**PCA**

A single matrix multiply against the stored loadings (`model$loadings$X`) reproduces `$variates$X` exactly — projection is `x_new %*% loadings`, mathematically equivalent to `stats::predict.prcomp` for plain, non-sparse PCA since loadings are orthogonal SVD vectors with no deflation between components. The result is a matrix of PC scores (`Dim.1`, `Dim.2`, …) — one row per unknown specimen. No classification is performed; the scores indicate where each unknown falls within the training variance structure.

Interpreting PCA projections: an unknown that projects close to a cluster of training specimens in PC space shares a similar multivariate profile with those specimens. An unknown that falls far from all training specimens (extrapolation zone) may have a measurement profile outside the range the training data can describe reliably.

**sPCA**

Unlike plain PCA, a single matrix multiply against the loadings does **not** reproduce sPCA's scores beyond the first component: mixOmics fits sPCA one component at a time via NIPALS-style power iteration, deflating the training data matrix after each component by regressing out that component's score. Projecting new data replays the same per-component deflation to match. Interpretation is otherwise identical to PCA.

**IPCA**

IPCA computes each score by projecting onto the rotation matrix and then normalizing by a constant derived from the *training* data as a whole (not a per-observation transform). To keep new samples on the same scale as training scores, projection reuses those training-derived normalization constants rather than recomputing them from the new sample count. Interpretation is otherwise identical to PCA, with the caveat that IPCA's components are not variance-ranked (see the IPCA scaling documentation in the PCA module's Details tab).

**LDA**

`predict.lda(model, newdata)` returns:
- `$class` — predicted group label (maximum posterior)
- `$posterior` — posterior probability matrix $P(\text{group}_k \mid \mathbf{x})$ for all groups
- `$x` — LD scores (projections onto linear discriminant axes)

Classification uses Bayes' rule with the prior probabilities stored in the training model:

$$\hat{k} = \underset{k}{\arg\max}\; P(k \mid \mathbf{x}) = \underset{k}{\arg\max}\; \pi_k \cdot f_k(\mathbf{x})$$

where $f_k(\mathbf{x})$ is the multivariate Gaussian density under the pooled within-group covariance.

**MDA**

`predict.mda(model, newdata)` is called three times to retrieve the predicted class (default), posterior probabilities (`type = "posterior"`), and discriminant variates (`type = "variates"`). Classification uses the mixture model posteriors summed across subclass components within each group.

**QDA**

`predict.qda(model, newdata)` returns `$class` and `$posterior` using per-group quadratic discriminant functions. Because QDA does not produce linear discriminant axes, LD scores for visualization are obtained by projecting the preprocessed unknown data through a **companion LDA** model stored in the bundle (`bundle$lda_model`). This companion LDA is fitted on the same training data for visualization purposes only and does not influence classification.

**PLS-DA / sPLS-DA**

`predict.mixo_plsda(model, newdata)` returns `$class$max.dist` (predicted group, using the maximum-distance classification rule) and `$predict` (posterior-like class scores) for the final retained component, plus `$variates` (component scores, labelled `Comp1`, `Comp2`, …). sPLS-DA uses the identical prediction call — its stored model already encodes which variables were selected during training, so no additional handling is needed at prediction time.

**Cluster (K-Means / PAM)**

Clustering algorithms have no `predict()` generic. Instead, each unknown observation is assigned to the **nearest reference point** using the same distance metric the algorithm used during training:

$$\hat{k} = \underset{k}{\arg\min}\; d(\mathbf{x}, \mathbf{c}_k)$$

where $\mathbf{c}_k$ is the centroid (K-Means, `model$centers`) or medoid (PAM, `model$medoids`) of cluster $k$, and $d$ is Euclidean distance for K-Means or Manhattan distance for PAM — matching `bundle$cluster_metric`. This is the same nearest-centroid/medoid rule the algorithms use internally to assign training points, applied out-of-sample. There is no posterior probability; the assignment is a hard, deterministic label (`Cluster 1`, `Cluster 2`, …).

</details>

<details>
<summary><strong>Validation Logic</strong></summary>

Validation runs automatically when both the bundle and unknown data are loaded. The following checks are performed:

| Check | Type | Trigger |
|-------|------|---------|
| All `numeric_cols` present in unknown data | **Error** (blocking) | Any required column missing |
| Present measurement columns are numeric | **Error** (blocking) | Column with measurement name contains text/factor |
| Optional metadata columns present | **Warning** (non-blocking) | A metadata column from the bundle is absent in unknown data |
| Value ranges within 20% margin of training range | **Warning** (non-blocking) | `unknown_range` exceeds `training_range ± 0.2 × training_span` for any column |

Errors prevent prediction; warnings are displayed as an alert banner above the results but do not block the run. Range warnings indicate that one or more columns have unknown values outside the distribution the model was trained on — these samples are extrapolation points and their predictions should be interpreted with additional caution.

</details>

##### Data Interpretation

<details>
<summary><strong>Interpreting Classification Results (LDA / MDA / QDA)</strong></summary>

The results table contains the following columns:

| Column | Content | Interpretation |
|--------|---------|----------------|
| `Sample` | Specimen label (from label column or auto-generated `Unknown_N`) | Row identifier |
| `Predicted` | Predicted group label | The group with highest posterior probability |
| `P(GroupA)`, `P(GroupB)`, … | Posterior probability for each group | Sums to 1.0 per row |
| `LD1`, `LD2`, … | Linear discriminant scores (LDA/MDA) or companion LDA projection (QDA) | Position in discriminant space |

**Reading posterior probabilities**:

| Pattern | Interpretation |
|---------|---------------|
| One group near 1.0, others near 0.0 | Confident, unambiguous assignment |
| Two groups both > 0.3 | Specimen lies near the decision boundary; classification is uncertain |
| All groups approximately equal ($\approx 1/G$) | Specimen is equidistant from all group centroids — highly ambiguous |
| One group > 0.5 but < 0.7 | Weak assignment — report with caution; consider collecting additional measurements |

**Confidence in prediction is distinct from model accuracy**: a specimen can receive a high posterior for one group even if that group is wrong — the model assigns the *most likely* group given the training distribution. Posterior probabilities reflect confidence within the model's assumptions; they do not account for the possibility that the unknown belongs to a group not represented in the training data.

</details>

<details>
<summary><strong>Interpreting the Overlay Plot</strong></summary>

The overlay plot superimposes unknown specimens on the full training data visualization. Visual encoding:

| Symbol | Meaning |
|--------|---------|
| **Circles** (semi-transparent) | Training specimens, coloured by group |
| **Filled triangles** (opaque, coloured by predicted group) | Unknown specimens |
| **Shaded regions / decision boundaries** (LDA/MDA/QDA) | Predicted class region for each location in discriminant space |
| **Confidence ellipses / convex hulls** (PCA) | Group extent in PC space |

Hover over any triangle to see the specimen label, predicted class, and axis coordinates.

**Positioning interpretation for LDA/MDA/QDA**: an unknown triangle that falls deep within a group's cloud and far from decision boundaries indicates a high-confidence assignment. A triangle near a boundary line — especially between two groups — corresponds to low posterior probability separation, regardless of the printed predicted label.

**Positioning interpretation for PCA**: the overlay plot does not classify; it shows the unknown's multivariate profile relative to the training population. An unknown that plots outside all training group clouds may be atypical or may belong to a group not represented in the training data.

</details>

<details>
<summary><strong>Interpreting T² and Q-Residual (PCA / sPCA / IPCA)</strong></summary>

The overlay plot shows *where* an unknown falls relative to training groups, but it cannot substitute for a quantitative statement of how well the unknown's measurement profile is actually described by the fitted model. Two complementary metrics answer that:

- **Hotelling's T²** — the (Mahalanobis-type) distance of the unknown's score vector from the training score centroid, measured within the retained components. A large T² means the unknown is unusual *along the axes the model actually captures*.
- **Q-residual (SPE)** — the part of the unknown's measurement profile **not** explained by the retained components, i.e. how much is left over after projecting onto the model and reconstructing back. A large Q means the unknown has structure the model was never fit to describe.

Reading the two together:

| T² | Q | Interpretation |
|----|---|-----------------|
| Low | Low | Typical — well described by the model on every axis that matters |
| High | Low | Unusual position within the retained components, but still well-reconstructed |
| Low | High | **The critical blind spot**: looks normal within the retained components, but is structurally unlike anything in training — see below |
| High | High | Clear outlier by both measures |

**This matters most for sPCA and IPCA.** Plain PCA's early components are variance-ranked, so a small number of retained components already captures most of the structure any Iris-scale dataset can have — low-T² samples are rarely also high-Q. sPCA's sparse loadings and IPCA's independence-based rotation do **not** guarantee this: a sample can land squarely inside the training cloud on the retained components (low T²) while being poorly reconstructed by them (high Q), because those methods do not prioritize explaining total variance the way plain PCA's leading components do.

`T2_flag`/`Q_flag` in the diagnostics table mark samples exceeding a threshold at each metric. T²'s threshold is the standard F-distribution-based cutoff. Q's threshold is an **empirical 95th percentile of the training set's own Q-residuals** rather than the parametric Jackson-Mudholkar chi-square approximation — the parametric form needs eigenvalues of the full (not just retained) training covariance matrix, which this application does not retain; the empirical quantile is a standard, dependency-free alternative used in the process-monitoring literature and degrades gracefully for small training sets.

</details>

<details>
<summary><strong>Interpreting Mahalanobis Distance and Typicality Probability (LDA / MDA / QDA / PLS-DA / sPLS-DA)</strong></summary>

Posterior probability alone cannot detect that an unknown belongs to a group **not represented in the training data** — see the FAQ entry on posterior probability for the closed-world caveat this addresses. Typicality probability is the metric designed to catch it: it is the chi-square probability that the unknown's distance to its *nearest* group centroid — not necessarily the predicted group — is consistent with normal within-group scatter. A low typicality probability flags a specimen the model still had to assign somewhere, but which does not actually resemble any trained group.

| Column | Content |
|--------|---------|
| `Mahalanobis_to_predicted` | Mahalanobis distance from the unknown to the centroid of its *predicted* group |
| `Nearest_group` | The group whose centroid is closest, which may differ from the predicted group |
| `Mahalanobis_to_nearest` | Mahalanobis distance to that nearest group |
| `Typicality_p` | Chi-square probability associated with `Mahalanobis_to_nearest` — low values flag an atypical specimen |

**Where the distance is computed**: for LDA/QDA/MDA the metric uses true Mahalanobis distance in **original measurement space**, with per-group covariance and per-group mean — this is QDA's native classification geometry, applied uniformly to LDA/MDA as well. For PLS-DA/sPLS-DA the metric is computed in **component-score space** instead, because that is the space these methods' classification decision actually lives in.

</details>

<details>
<summary><strong>Interpreting the Distance-Ratio Confidence Proxy (Cluster)</strong></summary>

K-Means and PAM have no posterior probability or discriminant model — cluster assignment is a hard nearest-centroid/medoid rule with no built-in confidence measure. The **distance ratio** is a lightweight, out-of-sample confidence proxy: the ratio of the unknown's distance to its assigned centroid/medoid versus its distance to the second-nearest one.

| Column | Content |
|--------|---------|
| `Dist_to_assigned` | Distance to the nearest (assigned) centroid/medoid |
| `Dist_to_second_nearest` | Distance to the second-nearest centroid/medoid |
| `Distance_ratio` | `Dist_to_assigned / Dist_to_second_nearest`, always in `[0, 1]` |

A ratio near **0** means the unknown sits much closer to its assigned cluster than to any alternative — a confident assignment. A ratio near **1** means the unknown is nearly equidistant between two clusters — an ambiguous assignment, analogous to a near-zero silhouette width. This proxy exists specifically because full silhouette width requires pairwise distances to every training point, which is not cheaply available for a single new observation at prediction time.

</details>

<details>
<summary><strong>Plot Settings Controls</strong></summary>

Controls are in the **Plot Settings** sidebar tab and adapt based on analysis type.

**Shared controls (all types)**:

| Control | Effect |
|---------|--------|
| **Label column** | Selects a text/factor column from the unknown data to use as specimen labels in the plot and results table. If no column is selected, specimens are labelled `Unknown_1`, `Unknown_2`, … |
| **Width / Height (cm)** | Export dimensions for SVG and PNG downloads |

**LDA / MDA / QDA controls**:

| Control | Effect |
|---------|--------|
| **Dim.X / Dim.Y** | Selects which LD axes to display. For QDA, also allows original measurement variables |
| **Show Assumption Diagnostics** | Overlays per-group (solid) and pooled within-group (dashed) covariance ellipses to assess the equal-covariance assumption |
| **Show Decision Boundaries** | Shades the plot by predicted region and draws boundary contour lines |

**PCA controls**:

| Control | Effect |
|---------|--------|
| **X Axis / Y Axis** | Selects which PC dimensions to display |
| **Biplot Layer** | `Individuals` — scores only; `Variables (Loadings)` — variable arrows only; `Combined` — both |
| **Group training data** | Groups training specimens by one or more metadata columns from the bundle |
| **Use Convex Hull** | Replaces 95% confidence ellipses with convex hulls |
| **Point Alpha / Point Size** | Fixed value or contribution-scaled opacity/size for training points |

**Cluster controls**:

| Control | Effect |
|---------|--------|
| **X Axis / Y Axis** | Selects which raw measurement columns to display (cluster overlays always plot in raw measurement space, not a reduced projection) |

</details>

##### Quality Assurance

<details>
<summary><strong>Verifying the Bundle–Data Match</strong></summary>

Before interpreting results, confirm:

1. **Bundle summary card** shows the expected analysis type, training observation count, and variable count — if these do not match your expectation, you may have loaded the wrong bundle
2. **Validation badge** on the unknown data upload shows **Ready** or at most warnings — any errors must be resolved before prediction
3. **Range warnings** list which specific columns have out-of-range values — check whether these reflect genuine differences or measurement errors in the unknown data
4. **Overlay plot position** — unknown triangles that fall entirely outside the training data cloud for all specimens suggest a systematic measurement difference between the unknown and training populations

</details>

<details>
<summary><strong>Detecting Problematic Predictions</strong></summary>

Flag predictions for further scrutiny when:

- **Posterior probability for the predicted class < 0.6** — the model is not confident; supplement with other evidence
- **Unknown sample is outside the training range for multiple variables** (range warnings) — the model is extrapolating; predictions in extrapolation zones have unknown reliability
- **All posterior probabilities are approximately equal** — the specimen is equidistant from all group centroids in the scaled feature space; prediction is essentially arbitrary
- **Unknown triangle plots in a white/unshaded region** — for LDA/MDA/QDA with decision boundaries enabled, an unshaded location means the prediction grid did not cover that region; the classification is still computed but not visually confirmed
- **Large gap between training overlay and unknown position in PCA** — the unknown has a different multivariate profile from all training specimens; it may not belong to any represented group

</details>

##### Best Practices

- **Always verify the reference population before applying a bundle** — measurement protocol, instrument, and variable definitions must match between training and unknown data
- **Use the Label column** — assigning meaningful specimen IDs in the label column makes results traceable when exported to Excel
- **Inspect posteriors, not just predicted class** — a predicted class with P < 0.6 is a weak assignment that should be reported with uncertainty
- **Export to Excel for reporting** — the Excel file contains all posterior probabilities and LD scores needed for statistical reporting
- **Enable decision boundaries** — the visual boundary overlay gives immediate intuition about classification confidence without reading the full posterior table
- **Use equal priors in the training model when groups are unequally sampled** — if the training bundle was built with proportional priors and sampling was unequal, the priors in the bundle may bias classifications toward larger training groups. This is a training decision; it cannot be changed at prediction time
