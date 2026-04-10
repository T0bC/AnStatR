#### Frequently Asked Questions — Prediction

<details>
<summary>What is a model bundle and why does prediction require one?</summary>

A model bundle is an `.rds` file exported from the PCA, LDA, QDA, or MDA tab after a successful analysis. It is a self-contained archive that stores everything needed to reproduce the exact same preprocessing and model for new data:

- The fitted model object (`prcomp`, `lda`, `qda`, or `mda`)
- The names of all measurement columns used during training (`numeric_cols`)
- Skewness transformation parameters (`transform_params`) — if normalization was applied, the transformation objects are stored so the same transform can be applied to unknown data
- Scaling parameters (`scale_params`) — the per-column training means and standard deviations for z-score standardization
- The training dataset itself (`raw_data`, `used_data`) — used for range validation and overlay plot reconstruction
- Metadata about the analysis (type, app version, creation timestamp)

Without a bundle, it would be impossible to apply the same preprocessing pipeline to new data. Re-scaling unknown data with its own mean and SD would place it in a different feature space than the training data, making the model's learned boundaries meaningless.

</details>

<details>
<summary>What does "reference population" mean and why does it matter?</summary>

The reference population (also called the comparative reference collection or reference assemblage) is the set of specimens with known group membership on which the model was trained. In zooarchaeological, paleontological, or archaeological applications this is typically a modern or well-documented collection of specimens of known taxonomy, diet, geographic origin, or time period.

For a prediction to be valid, the unknown specimens must be reasonably assumed to be drawn from the same population in a statistical sense — meaning they share the same measurement scale, instrument, and analytical protocol as the training data. More formally, the joint distribution of the measurement variables in the unknowns should be similar to the distribution represented in the training data.

**Reasoning**: a discriminant model trained on microwear texture data acquired with one confocal microscope and specific scan parameters will not produce reliable predictions for data acquired on a different instrument, at different magnification, or with a different scale-sensitive surface parameter. Even if all column names match and no validation error is raised, the feature space is systematically different and the model's decision boundaries do not apply.

This requirement cannot be enforced automatically by the software. It is a scientific judgement that must be made by the analyst before applying a bundle.

</details>

<details>
<summary>My unknown data passes validation but I get implausible predictions — what could be wrong?</summary>

Validation only checks column presence, type, and approximate value ranges. Several issues can produce implausible predictions without triggering errors:

- **Different measurement protocol or instrument**: column names match but values are on a different scale. The range check uses a 20% margin, so moderate systematic offsets pass validation while completely distorting predictions
- **Different preprocessing applied externally**: if the unknown data was filtered, smoothed, or otherwise transformed before upload, it will not be comparable to the raw training data that the bundle's preprocessing parameters expect
- **Wrong bundle**: a bundle trained on a different reference collection, even with the same variable names, produces meaningless predictions for an unrelated dataset
- **Extrapolation zone**: the unknown specimens have measurement profiles unlike anything in the training data. All predictions will be assigned to *some* group, but the model has no basis for these assignments. Check the overlay plot — unknowns far from the training cloud are in the extrapolation zone

Check the **range warnings** in the upload sidebar and inspect the **overlay plot** carefully. Unknowns that plot far outside the training distribution should be reported with explicit uncertainty.

</details>

<details>
<summary>I get a "Missing required measurement columns" error — how do I fix it?</summary>

The error lists the exact column names that are present in the bundle's `numeric_cols` but absent from your uploaded file. The most common causes and fixes:

| Cause | Fix |
|-------|-----|
| Column names differ by capitalisation (e.g., `Asfc` vs. `ASFC`) | Rename columns in your data to match exactly — matching is case-sensitive |
| Column was renamed in the export step | Check the original measurement file; use the exact names from the bundle summary or from the training data export |
| File uses a different delimiter or encoding | Re-export as UTF-8 CSV; avoid special characters in column names |
| Wrong sheet in the Excel file | The app reads only the first sheet; move your data to Sheet 1 |
| Bundle was trained on a subset of variables | Verify which variables are in the bundle (visible in the bundle summary card — *n* variables) and ensure your file includes all of them |

If the bundle was trained on PCA scores (column names `Dim.1`, `Dim.2`, …) the unknown data must also be structured as PCA score columns — raw measurements cannot be used directly in that case.

</details>

<details>
<summary>I see range warnings for several columns — should I continue with the prediction?</summary>

Range warnings are non-blocking: the prediction will run. However, they indicate that one or more columns in the unknown data contain values that fall outside the training distribution (beyond a 20% margin beyond the training range). Warnings do not necessarily mean the prediction is wrong, but they signal that the model is extrapolating.

**When range warnings are acceptable**:
- A few specimens slightly exceed the training range on one or two variables — this is common with small reference collections and does not fundamentally undermine the prediction
- The out-of-range values reflect genuine biological or archaeological variation that was not fully captured in the training collection

**When range warnings are a serious concern**:
- Many columns show warnings simultaneously — suggests a systematic offset between the unknown and training data (different instrument, protocol, or taphonomic alteration)
- The range exceedance is large (unknowns extend far beyond training range) — the model has no information about this part of the feature space
- Unknowns cluster away from all training specimens in the overlay plot

In these cases, report predictions with explicit uncertainty and consider expanding the reference collection before drawing conclusions.

</details>

<details>
<summary>What is the posterior probability and how should I interpret it for individual specimens?</summary>

The posterior probability $P(\text{group}_k \mid \mathbf{x})$ is the probability that specimen $\mathbf{x}$ belongs to group $k$, given the trained model and the observed measurements. It is computed via Bayes' rule combining the group likelihood (from the discriminant function) with the prior probability of each group.

The specimen is assigned to the group with the highest posterior. Posteriors sum to 1.0 across all groups for each specimen.

**Interpreting confidence**:

| Posterior for predicted group | Interpretation |
|-------------------------------|---------------|
| > 0.90 | High confidence — specimen clearly belongs to this group under the model |
| 0.70 – 0.90 | Moderate confidence — report the assignment with the posterior value |
| 0.50 – 0.70 | Weak assignment — specimen is near a decision boundary; supplementary evidence recommended |
| < 0.50 | Ambiguous — another group is nearly as likely or the specimen is equidistant from multiple groups |

**Important caveat**: posteriors are computed within the closed-world assumption that the unknown *must* belong to one of the groups represented in the training data. If the unknown actually belongs to a group not in the training collection (an unrepresented taxon, geographic origin, or time period), all posteriors will still sum to 1.0 and the model will assign it to the "nearest" training group — without any indication that the true group is absent. Low posteriors across all groups are the main warning sign of an unrepresented group (Ripley, 1996; McLachlan, 2004).

</details>

<details>
<summary>All my unknown specimens are predicted as the same group — is this a problem?</summary>

Not necessarily, but it warrants investigation. Possible explanations:

1. **Genuine similarity**: all unknowns truly belong to the dominant group, which is a valid scientific result
2. **Unbalanced priors**: if the training bundle used proportional priors and one group is much larger than others, the model will systematically favour that group for borderline specimens. This is by design — the prior reflects group prevalence in the training collection. If prevalence in the unknown population is different, this is a scientific mismatch
3. **Measurement offset**: a systematic difference between unknown and training measurements shifts all unknowns toward one group's region
4. **Small reference collection for other groups**: if competing groups have few training specimens, their group regions are small and less likely to capture unknowns

Check the **overlay plot** — do the unknown triangles cluster in one region? Also inspect the posterior probability columns: if all unknowns have very similar posterior patterns, the measurement profile may be systematically different from the training data.

</details>

<details>
<summary>Can I predict with a bundle if my unknown data is missing some metadata columns?</summary>

Yes. Metadata columns (non-measurement columns stored in the bundle as `meta_cols`) are not required for prediction. Missing metadata columns trigger a **non-blocking warning** in the validation panel — prediction proceeds normally. The only columns that are required are the `numeric_cols` listed in the bundle.

However, if the label column you selected in **Plot Settings** is absent from the unknown data, specimens will be labelled `Unknown_1`, `Unknown_2`, … in results and plots. This does not affect the prediction but makes specimen identification more difficult.

</details>

<details>
<summary>Edge cases: what happens in unusual data configurations?</summary>

Several configurations produce specific behaviour:

- **Single unknown specimen**: prediction runs normally; the overlay plot shows one triangle. Posterior probabilities are valid but a single specimen provides no statistical basis for generalising beyond that individual
- **Unknown specimen with NA values in required columns**: the row is passed to `predict()` with NA values. Behaviour depends on the model type — LDA and QDA in `MASS` will produce NA for that specimen's prediction; MDA may error. Remove rows with NAs from the unknown file before uploading
- **QDA bundle without companion LDA** (`bundle$lda_model` is NULL): QDA classification still runs and produces predicted class and posteriors, but the overlay plot cannot be displayed because LD scores are unavailable for visualisation. The results table is still populated
- **PCA bundle with many components**: all PC dimensions stored in the bundle are used for projection. The overlay plot defaults to `Dim.1` vs `Dim.2`; use the **X Axis / Y Axis** selectors in Plot Settings to explore other dimensions
- **Bundle trained on PCA scores (two-stage PCA → LDA)**: the unknown data must already be structured as PCA score columns (e.g., `Dim.1`, `Dim.2`, …). Raw measurements cannot be directly used with such a bundle — they must first be projected through the same PCA model
- **Column present but all values are NA**: the column passes the presence check but will be all-NA after reading. Validation does not detect this; the prediction will fail at the `predict()` call. Inspect the unknown file before uploading
- **Bundle created by a different TexAn version**: the `app_version` field is checked for display only; older bundles are accepted as long as all required fields are present. If a field was added in a newer version and is absent, the relevant feature (e.g., range validation) silently degrades

</details>

<details>
<summary>Why does the overlay plot not appear after prediction?</summary>

The overlay plot is shown only when LD scores or PC scores are available:

- **PCA**: always shown (scores are the primary output)
- **LDA / MDA**: shown when at least 2 discriminant dimensions are available. With only 2 groups (G = 2) there is exactly 1 LD axis — the plot cannot be rendered in 2D and is suppressed
- **QDA**: shown only when the companion LDA model is present in the bundle and successfully projects unknowns. If the bundle was exported without a companion LDA, the plot section is suppressed

If the plot is missing and you expected it: check the results table for a `scores` column — if no LD scores are listed, the plot cannot be generated from the current bundle.

</details>

<details>
<summary>Which R packages are used for the prediction computation?</summary>

| Package | Version | Purpose | Citation |
|---------|---------|---------|----------|
| **MASS** | ≥ 7.3 | LDA and QDA prediction via `MASS::lda()` / `MASS::qda()` and `stats::predict()` | Venables, W. N., & Ripley, B. D. (2002). *Modern Applied Statistics with S* (4th ed.). Springer. ISBN 0-387-95457-0 |
| **mda** | ≥ 0.5 | MDA prediction via `mda::mda()` and `stats::predict()` | Hastie, T., & Tibshirani, R. (1996). Discriminant analysis by Gaussian mixtures. *Journal of the Royal Statistical Society: Series B*, 58(1), 155–176 |
| **bestNormalize** | ≥ 1.0 | Applies stored skewness transformations to unknown data | Peterson, R. A., & Cavanaugh, J. E. (2020). Ordered quantile normalization. *Journal of Applied Statistics*, 47(13-15), 2312–2327 |
| **ggplot2** | ≥ 3.4 | Overlay plot generation and styling | Wickham, H. (2016). *ggplot2: Elegant Graphics for Data Analysis*. Springer |
| **ggiraph** | ≥ 0.8 | Interactive SVG overlay plots with hover tooltips | Gohel, D. (2024). *ggiraph: Make ggplot2 Graphics Interactive*. R package |
| **DT** | ≥ 0.28 | Interactive results table | Xie, Y., Cheng, J., & Tan, X. (2024). *DT: A Wrapper of the JavaScript Library DataTables*. R package |
| **openxlsx** | ≥ 4.2 | Excel export of prediction results | Schauberger, P., & Walker, A. (2023). *openxlsx: Read, Write and Edit xlsx Files*. R package |

Additional supporting packages: **stats** (base R `predict()`, `scale()`), **bslib** / **shiny** (application framework and UI).

</details>


**References**

- Fisher, R. A. (1936). The use of multiple measurements in taxonomic problems. *Annals of Eugenics*, 7(2), 179–188.
- Hastie, T., & Tibshirani, R. (1996). Discriminant analysis by Gaussian mixtures. *Journal of the Royal Statistical Society: Series B*, 58(1), 155–176.
- Hastie, T., Tibshirani, R., & Friedman, J. (2009). *The Elements of Statistical Learning* (2nd ed.). Springer.
- McLachlan, G. J. (2004). *Discriminant Analysis and Statistical Pattern Recognition*. Wiley.
- Peterson, R. A., & Cavanaugh, J. E. (2020). Ordered quantile normalization: A robust method for normalizing data. *Journal of Applied Statistics*, 47(13-15), 2312–2327.
- Venables, W. N., & Ripley, B. D. (2002). *Modern Applied Statistics with S* (4th ed.). Springer.