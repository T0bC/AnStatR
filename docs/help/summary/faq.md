#### Frequently Asked Questions

<details>
<summary>Why does the table show fewer observations (n) than my group actually has?</summary>

The `n` column counts only the values that passed all filters — it excludes:

- Rows flagged as outliers (`{col}_outlier = TRUE`) by the outlier detection in the Plotting tab
- Rows flagged as trimmed (`{col}_trimmed = TRUE`) by the trimming setting in the Plotting tab
- Rows with `NA` values for that measurement column

Check the `n_outliers` and `n_trimmed` columns (visible when non-zero) for the exact exclusion counts. Review the Plotting tab's outlier and trimming configuration if the filtered count is unexpectedly low.

</details>

<details>
<summary>Why is shapiro_p showing NA for some groups?</summary>

The Shapiro-Wilk test has strict sample size requirements. `NA` is returned when:

- **n < 3** — Too few observations to perform the test
- **n > 5000** — Test becomes computationally unreliable at very large sizes
- **All values are identical** — The test is undefined for zero-variance data (the `normal` column will show `"identical values"`)

To resolve small-group NAs, consider using fewer grouping columns to create larger combined groups, or interpret normality based on the `mean` vs `median` divergence instead.

</details>

<details>
<summary>How do I interpret the cv (coefficient of variation)?</summary>

The `cv` is `sd / mean` — a unit-free measure of relative variability. It allows comparison of spread across measurements with different scales or units.

- **cv < 0.1**: Low variability within the group
- **cv 0.1–0.3**: Moderate variability, typical for many biological measurements
- **cv > 0.3**: High variability — consider checking for undetected outliers or inhomogeneous groups

Note: `cv` is set to `NA` when `mean = 0` or `n ≤ 1`, as it is mathematically undefined in those cases.

</details>

<details>
<summary>My Group by selector is empty or missing columns</summary>

The **Group by** dropdown only lists descriptive (metadata) columns — those with UPPERCASE names and no digits, which the application classifies as grouping variables. If expected columns are missing:

- Verify column names follow the UPPERCASE convention (e.g., `SPECIES`, `SITE`, `PERIOD`)
- Ambiguous names (UPPERCASE + digits, e.g., `DATING_MIN`) may be classified as measurements instead — see the Details tab in the **Load Data** help for naming conventions
- Reload the data if column classification has changed

</details>

<details>
<summary>Statistics look different when "Show transformed summary" is enabled</summary>

When **Show transformed summary** is active, statistics are computed on the `{col}_normalized` columns instead of the raw measurement values. Normalized values have been transformed (e.g., log-transformed, z-scored, or range-scaled depending on the normalization method selected in the Plotting tab), so:

- `mean`, `sd`, and other stats reflect the transformed scale — they are not directly comparable to raw-unit values
- The info note in the sidebar confirms normalized data is in use
- To return to raw-unit statistics, uncheck **Show transformed summary**

</details>

<details>
<summary>Can I select multiple grouping columns at once?</summary>

Yes. Selecting multiple columns in **Group by** creates combined groups from all unique combinations of the selected columns (e.g., `SPECIES | SITE`). Each combination appears as one row in the output table.

Keep in mind: with many grouping columns or many distinct values, the number of rows can become large and group sizes (`n`) will decrease, which may make Shapiro-Wilk results unreliable.

</details>

<details>
<summary>The download produces an empty or corrupt file</summary>

This can occur if the summary tables have not yet been computed (e.g., no **Group by** column is selected). Ensure:

1. At least one grouping column is selected in **Group by**
2. The main panel shows the statistics cards (not a loading or waiting state)
3. Then click the download icon or **Download All Tables**

If the file downloads but opens as empty in Excel, verify that the measurement columns contain numeric data and are not all `NA` after filtering.

</details>
