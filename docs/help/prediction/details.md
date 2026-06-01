#### Prediction — Technical Reference

##### Requirements

**Model Bundle**

The bundle file must be an `.rds` object exported directly from the PCA, LDA, QDA, or MDA tab of this application. It must contain all of the following fields:

| Field | Content |
|-------|---------|
| `analysis_type` | One of `pca`, `lda`, `qda`, `mda` |
| `model` | The fitted model object (`prcomp`, `lda`, `qda`, or `mda`) |
| `numeric_cols` | Character vector of measurement column names used during training |
| `raw_data` | Training data before preprocessing (used for range validation) |
| `used_data` | Training data after preprocessing (used for overlay plots) |
| `scale_params` | Center and scale vectors (LDA/MDA/QDA) or NULL (PCA) |
| `transform_params` | Stored skewness transformation parameters or empty list |
| `app_version` | Version of AnStatR that created the bundle |
| `created` | Timestamp of bundle creation |

Bundles exported from external R sessions or other tools will not be accepted unless they conform to this structure. Bundles exported in cross-validation (CV) mode do not include a fitted model object and cannot be used for prediction.

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

**Step 2 — Scaling** (LDA / MDA / QDA only)

The stored `center` and `scale` vectors are applied to the transformed unknown data:

$$x'_{ij} = \frac{x_{ij} - \bar{x}_{j,\text{train}}}{s_{j,\text{train}}}$$

where $\bar{x}_{j,\text{train}}$ and $s_{j,\text{train}}$ are the training mean and standard deviation for column $j$.

**PCA is handled differently**: `predict.prcomp()` applies centering and scaling automatically from the stored `prcomp` object. No manual scaling step is needed.

This design ensures that the preprocessing pipeline is fully reproducible and that the unknown data occupies the same feature space as the training data.

</details>

<details>
<summary><strong>Prediction Methods by Analysis Type</strong></summary>

All prediction dispatches through R's generic `stats::predict()` applied to the stored model object.

**PCA**

`predict.prcomp(model, newdata)` projects unknown observations into the PC space defined by the training eigenvectors. The result is a matrix of PC scores (`Dim.1`, `Dim.2`, …) — one row per unknown specimen. No classification is performed; the scores indicate where each unknown falls within the training variance structure.

Interpreting PCA projections: an unknown that projects close to a cluster of training specimens in PC space shares a similar multivariate profile with those specimens. An unknown that falls far from all training specimens (extrapolation zone) may have a measurement profile outside the range the training data can describe reliably.

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
