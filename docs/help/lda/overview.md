#### Discriminant Analysis (LDA / QDA / MDA / PLS-DA / sPLS-DA)

Supervised method that finds combinations of measurement variables maximizing separation between predefined groups. Unlike PCA (which ignores group labels), discriminant analysis explicitly targets group differences.

##### Data Selection

Configure columns in the **Data Selection** sidebar tab:

- **Descriptive (metadata) columns** — Select columns like `SAMPLE_ID`, `SPECIES`, `SITE`, or `TOOTH_TYPE` that describe your specimens. These are *not* used in the analysis computation but are displayed in plots and exported results for identification and context
- **Grouping column** *(required)* — The single categorical column that defines the groups to be discriminated (e.g., `SPECIES`, `PERIOD`, `TAXON`). LDA/QDA maximizes separation *between* these groups. Must contain at least 2 distinct, non-missing levels
- **Measurement columns** — Numeric variables included in the analysis. Click **Select all** to include all measurement columns

If the **Statistics** tab has computed a parameter screening ranking (see the Plotting tab's parameter screening mode), an **Apply recommended parameters** banner appears above the measurement-column selector. Clicking it replaces the current selection with the parameters the Statistics tab identified as good group separators — you can still add or remove columns manually afterward.

##### Analysis Type

Choose between five methods in the **Analysis Settings** tab:

| Method | Decision Boundary | Key Requirement | Best For |
|--------|-------------------|-----------------|----------|
| **LDA** (Linear) | Linear (flat) | Groups share same covariance structure | Default; limited observations per group |
| **QDA** (Quadratic) | Quadratic (curved) | ≥ p+1 observations per group | Groups with clearly different spread/shape |
| **MDA** (Mixture) | Flexible (mixture) | ≥ max(subclasses, p + 1) observations per group (enforced); more recommended for stability | Multi-modal or non-elliptical group shapes |
| **PLS-DA** | Distance to component-space centroid | None — works even with more variables than specimens | High-dimensional or collinear measurement sets |
| **sPLS-DA** (sparse) | Distance to component-space centroid | None | Same as PLS-DA, plus built-in variable selection |

**When to use PLS-DA/sPLS-DA instead of LDA/QDA/MDA**: if you have more measurement parameters than specimens per group (common with 40+ computed 3D surface-texture parameters and modest sample sizes), or your parameters are collinear (e.g., a 2D and a 3D version of a similar surface feature), LDA/QDA/MDA will warn or fail with a singular-matrix error. PLS-DA handles both situations directly, and sPLS-DA additionally performs sparse variable selection to identify which parameters actually drive the group differences — see the **Selected Variables** results panel and the Details tab for the full method description.

##### Scaling and Preprocessing

- **Data Scaling** — **Scale & Center (recommended)** applies z-score standardization (mean=0, SD=1), ensuring all variables contribute equally regardless of original units or magnitude. Essential when variables are on different scales. See Details tab for full scaling implications
- **Normalize skewed variables** — Transforms highly skewed variables (|skewness| > 2) using the bestNormalize package before analysis. Reduces outlier influence. See Details tab for guidance on when to enable this

##### Key Result

After clicking **Compute**, the most important result is the **LD Scores Plot** (**Component Scores Plot** for PLS-DA/sPLS-DA), open by default — the scatter plot projecting all specimens onto the discriminant axes or components. The degree of separation between group clouds directly reflects how well the measurement variables discriminate the groups. The **Proportion of Trace** (**Explained Variance** for PLS-DA/sPLS-DA) table in **LDA Results** reports how much variance each axis/component captures. For PLS-DA/sPLS-DA, the **VIP Scores** panel ranks every variable's overall importance across all components — the primary starting point for "which of my parameters discriminate the groups?" For sPLS-DA specifically, the **Selected Variables** panel additionally lists which measurement parameters were retained per component by sparse selection, and the **Component Diagnostics (perf)** panel's stability table reports how reproducible that selection is across cross-validation folds.
