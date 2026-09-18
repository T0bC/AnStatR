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

#### Overview Panel

The **Overview** panel summarises the shape and completeness of the dataset in four figures:

| Figure | Meaning | What to check |
|--------|---------|---------------|
| **Rows** | Total observations imported | Matches the row count of your source file |
| **Columns** | Total columns, with the metadata / measurement / ambiguous split shown as badges | The split matches your naming intent — see *Column Naming Conventions* above |
| **Cells missing** | Share of all cells that are empty, with the absolute count | A high figure warrants inspecting the **Missing Values** panel before analysis |
| **Complete rows** | Rows with no gaps in any column | A low figure is normal for wide tables where optional metadata is sparse |

The metadata and measurement counts need not add up to the total: ambiguous names (uppercase with digits) are counted separately.

#### Data Checks

A banner above the panels reports structural problems that the summary statistics do not surface. It lists only the checks that actually triggered, and is absent when the data is clean.

| Check | Trigger | Why it matters |
|-------|---------|----------------|
| **Capitalisation collision** | Two or more column names identical apart from case, e.g. `FDI` and `fdi` | Column selections downstream may pick the wrong one |
| **Mostly missing** | Column missing **≥50%** of its values | Groups relying on the column may be lost in downstream models |
| **Single value throughout** | Column has one distinct non-missing value | Cannot be used for grouping or comparison |
| **Entirely empty** | Column has no values at all | Carries no information and can be dropped |

A column flagged as mostly missing is not listed again as single-valued — the more informative check wins.

#### Missing Values Panel

Three sub-tabs answer three different questions. All of them consider **only the columns that actually contain gaps**, so a dataset with 60 columns and 20 affected ones charts 20, not 60.

| Sub-tab | Shows | Use it to answer |
|---------|-------|------------------|
| **By column** | Ranked bar chart of missing count and percentage per affected column, captioned with how many columns are complete | *Which variables are incomplete, and how badly?* |
| **By row** | Raster of every observation against every affected column, rows sorted by their missingness pattern | *Are the gaps scattered, or do whole blocks of rows share them?* |
| **Co-occurrence** | The most frequent distinct missingness patterns, each labelled with the columns involved and the number of rows sharing it | *Which columns go missing together?* |

**Reading the By row raster**: contiguous bands of missing cells indicate that gaps arrive in blocks — typically whole batches, runs, or collection episodes. Scattered speckle indicates gaps arising independently per measurement. The distinction matters because block-wise missingness removes entire groups from a model, while scattered missingness merely thins them.

**Reading Co-occurrence**: when every pattern affects only a single row, the panel says so directly instead of charting a misleading ranking. Large datasets often have more patterns than fit on screen; the caption reports how many are not shown.

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

The panels serve a critical quality control function. Missing data patterns directly impact analysis reliability — if a column exceeds a threshold of missing values (typically **>20-30%**), results may be statistically unreliable. Working from the top of the tab downwards, the panels help identify:

- **Structural faults** flagged in the data checks banner, such as duplicate column names or columns with no variation
- **Columns with excessive missing data** that may need exclusion before analysis
- **Systematic gaps** where whole blocks of rows share the same missing columns
- **Data import errors** such as wrong delimiters causing merged columns or misaligned data
- **Unexpected value distributions** indicating formatting issues or outliers
- **Type mismatches** where numeric data was interpreted as text

Review these panels systematically before proceeding to downstream analysis modules. See the FAQ for guidance on acceptable missingness levels.

#### Best Practices

- **Resolve banner flags first**: They point at faults that silently distort later results
- **Verify import settings**: If numbers appear as text or columns merge, check delimiter/quote settings
- **Check missing data thresholds**: Consider excluding columns with >20-30% missing values
- **Establish whether gaps are structured**: Use **By row** and **Co-occurrence** before deciding how to handle them
- **Validate column naming**: Use consistent UPPERCASE for metadata, mixed-case for measurements
- **Review factor levels**: Ensure categorical groupings are clean and consistent
- **Inspect distributions**: Look for impossible values (e.g., negative measurements) indicating import errors
