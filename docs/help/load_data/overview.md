#### Load Data

Upload a CSV or XLSX file, or use one of the built-in **Example Datasets** to explore the application's features without importing your own data.

##### Supported Formats

- **CSV** — Comma-separated values (configurable delimiter, quote character, and header row)
- **XLSX** — Excel workbook (first sheet is imported)

##### Example Datasets

Built-in datasets are available for testing functionality:
- Select a dataset from the dropdown and click **Load** to import it
- Click **Save copy** to download the file for external use or inspection

##### CSV Settings

Use the gear icon in the sidebar to adjust:

- **Header row** — Whether the first row contains column names
- **Delimiter** — Comma, semicolon, or tab
- **Quote character** — Double quote, single quote, or none

##### Data Checks

If the application finds structural problems, a banner appears above the panels listing what to look at — column names that differ only by capitalisation, columns holding a single value throughout, and columns missing most of their values. Nothing appears when the data is clean.

##### Data Overview

After loading, the tab opens on a health check rather than on the raw table:

- **Overview** — Row and column counts, the split into metadata and measurement columns, the share of missing cells, and how many rows have no gaps at all
- **Missing Values** — Three views of incomplete data: **By column** (which columns are affected), **By row** (where the gaps sit), and **Co-occurrence** (which columns are missing together)
- **Data Summary** — Statistical overview including variable types, distinct values, and distributions
- **Data Preview** — Interactive table with column filtering to verify values and structure
