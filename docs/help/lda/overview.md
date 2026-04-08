#### Discriminant Analysis (LDA / QDA / MDA)

Supervised method that finds linear combinations of measurement variables maximizing separation between predefined groups. Unlike PCA (which ignores group labels), discriminant analysis explicitly targets group differences.

##### Data Selection

Configure columns in the **Data Selection** sidebar tab:

- **Descriptive (metadata) columns** — Select columns like `SAMPLE_ID`, `SPECIES`, `SITE`, or `TOOTH_TYPE` that describe your specimens. These are *not* used in the analysis computation but are displayed in plots and exported results for identification and context
- **Grouping column** *(required)* — The single categorical column that defines the groups to be discriminated (e.g., `SPECIES`, `PERIOD`, `TAXON`). LDA/QDA maximizes separation *between* these groups. Must contain at least 2 distinct, non-missing levels
- **Measurement columns** — Numeric variables included in the analysis. Click **Select all** to include all measurement columns

##### Analysis Type

Choose between three methods in the **Analysis Settings** tab:

| Method | Decision Boundary | Key Requirement | Best For |
|--------|-------------------|-----------------|----------|
| **LDA** (Linear) | Linear (flat) | Groups share same covariance structure | Default; limited observations per group |
| **QDA** (Quadratic) | Quadratic (curved) | ≥ p+1 observations per group | Groups with clearly different spread/shape |
| **MDA** (Mixture) | Flexible (mixture) | ≥ subclasses × p observations per group | Multi-modal or non-elliptical group shapes |

##### Scaling and Preprocessing

- **Data Scaling** — **Scale & Center (recommended)** applies z-score standardization (mean=0, SD=1), ensuring all variables contribute equally regardless of original units or magnitude. Essential when variables are on different scales. See Details tab for full scaling implications
- **Normalize skewed variables** — Transforms highly skewed variables (|skewness| > 2) using the bestNormalize package before analysis. Reduces outlier influence. See Details tab for guidance on when to enable this

##### Key Result

After clicking **Compute LDA / QDA / MDA**, the most important result is the **LD Scores Plot** (open by default) — the scatter plot projecting all specimens onto the linear discriminant axes (LD1, LD2, …). The degree of separation between group clouds directly reflects how well the measurement variables discriminate the groups. The **Proportion of Trace** table in **LDA Results** reports how much between-group variance each discriminant axis captures.
