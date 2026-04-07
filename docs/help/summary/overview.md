#### Summary Statistics

Compute descriptive statistics for each measurement column, grouped by one or more descriptive (metadata) columns. Uses the same data filtering, outlier detection, and trimming as the **Plotting** tab.

##### Configuration

- **Group by** — Select one or more descriptive columns (e.g., `SPECIES`, `SITE`) to define the grouping structure. Defaults to the X-axis selection from the Plotting tab.
- **Test for Normality** — Enable to add Shapiro-Wilk test results (`shapiro_W`, `shapiro_p`, `normal`) to each table. A p-value **< 0.05** indicates non-normal distribution.
- **Show transformed summary** — Visible only when normalization is active in the Plotting tab. When checked, statistics are computed on the normalized values instead of raw measurements.

##### Output

One card per measurement column appears in the main panel. Each card contains a table with one row per group:

- `n` — Observations retained after outlier/trim filtering
- `mean`, `median` — Central tendency
- `var`, `sd` — Spread
- `sem` — Standard error of the mean
- `cv` — Coefficient of variation (`sd / mean`)
- `n_outliers`, `n_trimmed` — Excluded value counts (hidden when all zero)

##### Downloads

- Click the **download icon** (↓) in any card header to export that table as XLSX.
- Click **Download All Tables** in the sidebar to export all measurement tables into a single multi-sheet XLSX workbook.
