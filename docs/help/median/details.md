#### Requirements

The Median module requires:

- **Loaded data** with at least one numeric measurement column (mixed-case column names like `Asfc`, `epLsar`)
- **Optional grouping columns** — descriptive columns (UPPERCASE names like `SAMPLE_ID`, `SITE`, `SPECIES`)
- **Optional quality column** — any column indicating measurement quality (can be descriptive or measurement)

#### Technical Specifications

##### Column Classification

The module uses the application's standard column classification system:

| Column Type | Pattern | Used For |
|-------------|---------|----------|
| **Descriptive/Metadata** | `^[A-Z_]+$` (uppercase + underscores only) | Grouping, constant metadata |
| **Measurement** | Mixed case (any lowercase letters) | Median calculation |

##### Quality Column Auto-Detection

The system automatically detects quality column types:

| Type | Detection Criteria | Filter Interface |
|------|-------------------|------------------|
| **Categorical** | ≤10 unique integer values, or non-numeric | Multi-select dropdown for "bad" values |
| **Percentage (0-1)** | All values between 0 and 1 | Numeric threshold (default: 0.8) |
| **Percentage (0-100)** | All values between 0 and 100, >10 unique values | Numeric threshold (default: 80) |
| **Numeric** | Any numeric range | Numeric threshold (default: midpoint) |

##### Quality Filtering Logic with Grouping

When grouping is enabled, quality filtering follows a **group-aware preservation** rule:

1. Rows below quality threshold (or matching "bad" categorical values) are marked
2. Groups are checked: if a group has **at least one good measurement**, bad rows are removed
3. Groups with **only bad measurements** are **kept intact** (not removed entirely)

This ensures that samples are not completely lost from the dataset due to quality issues — they remain available for analysis with their existing (low-quality) data.

Without grouping, bad rows are simply removed from the dataset.

##### Median Calculation Process

1. **Apply quality filter** (if enabled) — remove low-quality rows per group-aware logic
2. **Identify varying columns** — descriptive columns that change within groups are flagged for removal
3. **Calculate medians** — `stats::median(x, na.rm = TRUE)` for each measurement column per group
4. **Merge constant metadata** — descriptive columns that are constant within groups are preserved
5. **Round results** — measurement values rounded to 4 decimal places

#### Data Interpretation

##### Processing Summary Panel

The summary shows:

- **Grouping by**: Columns used for aggregation (or "No grouping selected")
- **Columns removed**: Descriptive columns that vary within groups (these cannot be meaningfully aggregated)
- **Quality filter**: Filter type applied, rows before/after, groups affected

##### Median Results Table

| Element | Interpretation |
|---------|---------------|
| **Row count** | Number of unique groups (if grouped) or filtered rows (if ungrouped) |
| **Column filters** | Excel-style dropdown filters on each column; click "All" to select all values |
| **Active filtering** | Filtered data flows to downstream modules — subset data by selecting specific values |
| **Missing cells** | Groups with no valid measurements for that variable (all values were NA or filtered) |

##### Excel-Style Column Filtering

The results table includes interactive column filters that function like Excel's AutoFilter:

- **Metadata columns** (grouping columns, constant descriptive columns): Multi-select dropdowns to include/exclude specific values
- **Use cases**:
  - Remove specific individuals by deselecting `SAMPLE_ID` values
  - Filter to specific groups (e.g., `SEX` = "Male" only, or `SITE` = "Site_A")
  - Create custom subsets for downstream analysis
- **Propagation**: Active filters affect the data passed to **all** downstream modules — Plotting, Summary, Statistics, PCA, LDA and Cluster. Only visible rows enter the analysis pipeline, so a filter left active here silently narrows every later tab
- **Convenience**: Click **All** above any filter to quickly select all values; individual items can then be unchecked

This filtering is independent of the quality filter and applies after median calculation, allowing flexible data subsetting without reprocessing.

##### What you can filter on downstream

Median calculation **drops** any descriptive column that varies within a group rather than collapsing it (see *Median Calculation Process* above). This determines which columns remain available for filtering in this table and in the PCA, LDA and Cluster **Filter Data** tabs:

| Grouping columns | `FACET` survives? | Can you filter by facet downstream? |
| --- | --- | --- |
| `SAMPLE_ID` | No | No |
| `SAMPLE_ID`, `JAW`, `TOOTH` | No | No |
| `SAMPLE_ID`, `JAW`, `TOOTH`, `FACET` | Yes (it is a grouping column) | Yes |

In short: **you can only filter downstream on columns you grouped by**, plus any descriptive column that happens to be constant within every group. If you intend to hunt for a signal at facet level, group down to facet level first.

#### Quality Assurance

Verify your median calculation with these checks:

- **Row count matches expected groups**: If grouping by `SAMPLE_ID`, row count should equal unique sample count
- **No unexpected column removal**: If important metadata columns are removed, verify they are constant within groups
- **Quality filter reasonable**: Check that filtered row counts align with your quality criteria
- **Median values in expected range**: Spot-check a few groups by manual calculation

#### Best Practices

- **Use consistent grouping**: Choose grouping columns that define your unit of analysis (e.g., `SAMPLE_ID` for per-sample medians)
- **Check quality column distribution**: Review quality values in the data preview before setting thresholds
- **Verify column classification**: Ensure measurement columns have mixed-case names; rename if needed
- **Review removed columns**: Check the summary — removed columns may indicate data structure issues
- **Download filtered data**: Use the download button to save results for external analysis or reporting
- **Use "Select All" in filters**: Click the "All" link above column filters to quickly select all values
