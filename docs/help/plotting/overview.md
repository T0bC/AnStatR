#### Plotting

Visualize your data with customizable scatter plots. Select descriptive and measurement columns to generate one plot per measurement variable, with configurable grouping, filtering, and styling options.

##### Data Selection

Configure the core plotting parameters:

- **Descriptive columns** — Select metadata columns (`SAMPLE_ID`, `SPECIES`, `SITE`, etc.) for grouping and filtering
- **Measurement columns (Y-Axis)** — Select numeric columns to plot; one plot is generated per measurement column
- **X-Axis** — Select up to 3 descriptive columns to define horizontal groupings; multiple selections create a nested X-axis design

##### Filter Data

Refine your dataset before plotting:

- **Hide columns** — Exclude high-cardinality columns (e.g., `SAMPLE_ID` with hundreds of entries) from the filter UI to reduce clutter; hidden columns remain available for tooltips
- **Filter checkboxes** — Include or exclude specific factor levels from the analysis; selections persist across data recalculations

##### Data Processing

Apply optional transformations to improve data quality and meet statistical assumptions. See the **Details** tab for comprehensive explanations of outlier detection methods and normalization options.

##### Plot Style

Customize plot appearance.. Adjust point size, transparency, shapes, colors, median/SD lines, axis settings, legend position, and export dimensions. The **Custom Colors** panel allows per-group color assignment when color grouping is active.
