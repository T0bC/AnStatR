#### Requirements

The Summary module requires data to be loaded and processed in the **Load Data** tab. Measurement columns must be present and follow the mixed-case naming convention (e.g., `Asfc`, `epLsar`, `Sq`). At least one descriptive (metadata) column must exist in the dataset to serve as the grouping variable.

#### Technical Specifications

##### Column Classification

Summary statistics are computed on **measurement columns** only.:

| Column Pattern | Role | Included in stats? |
|----------------|------|--------------------|
| Mixed-case (e.g., `Asfc`, `Sq`) | Measurement | Yes |
| `{col}_outlier` | Outlier flag | No — used for filtering |
| `{col}_trimmed` | Trim flag | No — used for filtering |
| `{col}_normalized` | Normalized values | Only when **Show transformed summary** is active |
| UPPERCASE-only (e.g., `SPECIES`) | Metadata / grouping | No — used as grouping keys |

##### Value Filtering

Before computing any statistic, values are filtered per group per measurement. The `n` column in the output reflects the count of retained values — **not** the raw group size.

##### Multi-Column Grouping

When multiple columns are selected in **Group by**, groups are formed by combining all selected columns (e.g., `SPECIES | SITE`). Each unique combination produces one row in the statistics table. Group labels in the output match the original column values.

##### Shapiro-Wilk Test Constraints

The Shapiro-Wilk test (`shapiro.test()`) is only applicable under specific conditions:

| Condition | Behaviour |
|-----------|-----------|
| `n < 3` | `shapiro_p`, `shapiro_W` set to `NA` |
| `n > 5000` | `shapiro_p`, `shapiro_W` set to `NA` |
| All values identical | `normal` = `"identical values"`, statistics set to `NA` |
| `p > 0.05` | `normal` = `"yes"` (normality not rejected) |
| `p ≤ 0.05` | `normal` = `"no"` (non-normal distribution) |

#### Data Interpretation

##### Statistic Reference

| Statistic | Formula | Interpretation |
|-----------|---------|----------------|
| `n` | Count of retained values | Sample size after filtering |
| `mean` | Arithmetic mean | Central tendency; sensitive to outliers |
| `median` | Middle value | Robust central tendency |
| `var` | Sample variance | Spread; units are squared |
| `sd` | √variance | Spread in original units |
| `sem` | `sd / √n` | Uncertainty of the mean estimate |
| `cv` | `sd / mean` | Relative variability; unit-free |
| `shapiro_W` | Test statistic | Closer to 1 → more normal |
| `shapiro_p` | p-value | `< 0.05` → non-normal at 5% significance |
| `n_outliers` | Outlier count | Excluded by outlier detection (Plotting tab) |
| `n_trimmed` | Trim count | Excluded by trimming (Plotting tab) |

The `n_outliers` and `n_trimmed` columns are hidden from the table when all values are zero to reduce visual clutter.

##### Normalized Data

When **Show transformed summary** is active, the module replaces each `{col}` with `{col}_normalized` before computing statistics. This means `mean`, `sd`, etc. reflect the transformed scale, not the original measurement units. The info note in the sidebar confirms this state.

#### Quality Assurance

- Compare `n` against the total group size to assess how many values were filtered out.
- A large difference between `mean` and `median` suggests skewed distributions or residual outliers — consider enabling **Test for Normality**.
- High `cv` values (typically **> 0.3** for morphological measurements) indicate high relative variability within groups.
- If `shapiro_p` is `NA` for many groups, group sizes are likely too small (**n < 3**). Consider merging groups or reducing grouping columns.

#### Best Practices

- Start with the grouping column used in the **Plotting** tab — the module defaults to this selection.
- Use **Test for Normality** to inform downstream statistical test selection (parametric vs. non-parametric).
- If `n_outliers` or `n_trimmed` are non-zero, review the Plotting tab's outlier and trimming settings before interpreting means.
- For normalized data comparisons, enable **Show transformed summary** to ensure statistics match the scale used in analysis.
- Download individual tables per measurement for focused reporting, or use **Download All Tables** for a complete archive.

See the FAQ for guidance on interpreting unexpected results or NA values in the output.
