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

##### Parameter Screening Mode

Enable **Disable plots (parameter screening mode)** to skip plot and diagnostics generation and screen many measurement parameters at once instead of reviewing them one plot at a time. In this mode:

- Select **X-Axis** and **measurement columns** only — plot styling, outlier detection, and normalization settings are hidden, and auto-normalization is applied automatically
- Selections pass straight through to the **Statistics** tab, which computes a p-value and effect size for every parameter across all pairwise group comparisons and ranks them by how well each separates the groups
- The top-ranked parameters are then available as a one-click **recommendation** in the **PCA**, **LDA**, and **Cluster** tabs

This screening-then-reduce workflow is recommended when working with 40+ measurement parameters, where manually inspecting each one is impractical. See the **Statistics** module help for how the ranking is computed, and the **Details** tab below for the underlying method.
