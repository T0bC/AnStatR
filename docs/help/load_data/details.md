#### File Requirements

Your data file should contain:

- **Header row** — Column names in the first row (recommended)
- **Numeric columns** — Measurement values for analysis
- **Grouping columns** — Optional categorical columns for grouping (e.g., Site, Period, Type)

#### Column Naming Conventions

The application distinguishes between **metadata** (descriptive) and **measurement** columns based on naming patterns:

| Column Type | Pattern | Examples |
|-------------|---------|----------|
| **Metadata** | UPPERCASE with underscores only (no digits) | `SPECIES`, `SAMPLE_ID`, `SITE`, `PERIOD` |
| **Measurement** | Mixed case (any combination of upper/lowercase) | `Asfc`, `epLsar`, `Sq`, `HAsfc9`, `S10z` |
| **Ambiguous** | UPPERCASE with digits (triggers warning) | `S10`, `DATING_MIN` |

**Important:** Ambiguous columns (uppercase + digits) generate a warning. They are treated as metadata if they contain fewer than 20 unique values, otherwise as measurements.

##### Example Table Structure

Below is a minimal working example showing properly formatted metadata and measurement columns:

| SAMPLE_ID | SPECIES | SITE | PERIOD | Asfc | epLsar | Sq | HAsfc9 |
|-----------|---------|------|--------|------|--------|-----|--------|
| S001 | Homo_sapiens | Site_A | Upper | 2.34 | 0.015 | 0.89 | 1.87 |
| S002 | Homo_sapiens | Site_A | Upper | 3.12 | 0.022 | 0.92 | 2.15 |
| S003 | Homo_neanderthalensis | Site_B | Lower |  | 0.018 | 0.85 | 1.92 |
| S004 | Homo_neanderthalensis | Site_B | Lower | 2.98 |  | 0.88 |  |
| S005 | Homo_sapiens | Site_C | Upper | 2.76 | 0.019 |  | 2.03 |

**Notes on this example:**
- **Metadata columns** (`SAMPLE_ID`, `SPECIES`, `SITE`, `PERIOD`): UPPERCASE names used for grouping and identification
- **Measurement columns** (`Asfc`, `epLsar`, `Sq`, `HAsfc9`): Mixed-case names containing numeric measurements
- **Missing values**: Shown as empty cells (rows 3-5). The application also accepts explicit `NA` values

#### CSV Import Options

When importing CSV files, you can customize:

| Setting | Options | Default |
|---------|---------|---------|
| Delimiter | Comma, Semicolon, Tab | Comma |
| Quote character | Double quote, Single quote, None | Double quote |
| Header row | Yes / No | Yes |

#### Excel Import

For XLSX files:

- Only the **first sheet** is imported
- Empty rows at the top are automatically skipped
- Column types are auto-detected

#### Data Summary Interpretation

The **Data Summary** panel provides a statistical overview of your dataset using `summarytools::dfSummary()`:

| Statistic | Purpose | Typical Use |
|-----------|---------|-------------|
| **Type** | Variable class (numeric, character, factor) | Verify columns imported correctly |
| **Distinct Values** | Count of unique entries | Identify factor columns (metadata typically has fewer distinct values than measurements) |
| **Valid/Obs** | Non-missing versus total observations | Assess data completeness |
| **Distribution** | Histogram or frequency table | Spot outliers or unexpected value ranges |

**Factor columns** (categorical data) are typically metadata columns with limited distinct values (e.g., 3 sites, 2 periods). Checking factor levels helps verify:
- No typos in category names (e.g., "Site_A" vs "site_a" creating separate groups)
- Expected categories are present
- Unexpected numeric values haven't been mixed into text fields

#### Data Quality and Visualization

The visualization panels serve a critical quality control function. Missing data patterns directly impact analysis reliability — if a column exceeds a threshold of missing values (typically >20-30%), results may be statistically unreliable. The visualizations help identify:

- **Columns with excessive missing data** that may need exclusion before analysis
- **Data import errors** such as wrong delimiters causing merged columns or misaligned data
- **Unexpected value distributions** indicating formatting issues or outliers
- **Type mismatches** where numeric data was interpreted as text

Review these panels systematically before proceeding to downstream analysis modules.

#### Best Practices

- **Verify import settings**: If numbers appear as text or columns merge, check delimiter/quote settings
- **Check missing data thresholds**: Consider excluding columns with >20-30% missing values
- **Validate column naming**: Use consistent UPPERCASE for metadata, mixed-case for measurements
- **Review factor levels**: Ensure categorical groupings are clean and consistent
- **Inspect distributions**: Look for impossible values (e.g., negative measurements) indicating import errors
