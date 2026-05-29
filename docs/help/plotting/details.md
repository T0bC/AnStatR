#### Data Processing Reference

Comprehensive guide to outlier detection methods, data normalization, and plot styling options.

---

##### Outlier Detection Methods

Eight methods are available for identifying univariate outliers. Detection is performed independently per measurement column within each X-axis group. Adjust the **Factor** or **Threshold** slider based on your data characteristics.

<details>
<summary><strong>IQR (Tukey's Method)</strong></summary>

Uses the interquartile range to define outlier boundaries. Values outside `[Q1 - factor*IQR, Q3 + factor*IQR]` are flagged.

- **Best for**: Symmetric distributions
- **Factor range**: 1.5–3.0 (default: 1.5)
- **Robust**: Yes — uses quartiles, not mean/SD
- **Special cases**: Falls back to less strict bounds when IQR is zero
</details>

<details>
<summary><strong>Z-Score</strong></summary>

Flags values where `|z| > factor`, with `z = (x - mean) / SD`.

- **Best for**: Normally distributed data
- **Factor range**: 2.0–3.0 (default: 3.0)
- **Robust**: No — sensitive to outliers in mean/SD calculation
- **Requirement**: Minimum 3 valid values per group
</details>

<details>
<summary><strong>Modified Z-Score</strong></summary>

Uses median and MAD (Median Absolute Deviation) instead of mean/SD. Flags values where the modified z-score exceeds the factor threshold.

Formula: `M = 0.6745 × (x - median) / MAD`, where MAD is the raw (unscaled) median absolute deviation. The constant 0.6745 ≈ 1/1.4826 scales the result to be comparable with standard z-scores under normality.

- **Best for**: Skewed distributions
- **Factor range**: 3.5–4.5 (default: 3.5)
- **Robust**: Yes — MAD is less sensitive to extreme values
- **Requirement**: Minimum 3 valid values per group
</details>

<details>
<summary><strong>Adjusted Boxplot</strong></summary>

Skewness-adjusted IQR method using the medcouple (MC) statistic. Adjusts upper and lower bounds asymmetrically based on distribution skewness using exponential functions:

- For MC ≥ 0: `[Q1 − factor × e^(−4×MC) × IQR, Q3 + factor × e^(3×MC) × IQR]`
- For MC < 0: `[Q1 − factor × e^(−3×MC) × IQR, Q3 + factor × e^(4×MC) × IQR]`

- **Best for**: Moderately skewed data (−0.6 ≤ MC ≤ 0.6)
- **Factor range**: 1.5–3.0 (default: 1.5)
- **Robust**: Yes
- **Dependency**: Requires the `robustbase` package; falls back to standard IQR if unavailable
</details>

<details>
<summary><strong>KDE (Kernel Density Estimation)</strong></summary>

Estimates the probability density function and flags values in low-density regions below the threshold percentile.

- **Best for**: Multimodal distributions with multiple peaks
- **Threshold range**: 0.05–0.20 (default: 0.05)
- **Interpretation**: Lower threshold = more aggressive outlier detection
- **Requirement**: Minimum 4 valid values per group
</details>

<details>
<summary><strong>Isolation Forest</strong></summary>

Tree-based method that isolates anomalies by randomly selecting features and split values. Anomalies require fewer splits to isolate.

- **Best for**: Large datasets with complex patterns
- **Threshold range**: 0.05–0.20 (default: 0.05)
- **Robust**: Yes — handles high-dimensional data well
- **Dependency**: Requires the `isotree` package; falls back to IQR if unavailable
- **Requirement**: Minimum 10 valid values per group
</details>

<details>
<summary><strong>LOF (Local Outlier Factor)</strong></summary>

Density-based method comparing local density of a point to its neighbors. Points with substantially lower density are flagged.

- **Best for**: Data with density clusters
- **Threshold range**: 0.05–0.20 (default: 0.05)
- **Robust**: Yes — considers local neighborhood structure
- **Dependency**: Requires the `dbscan` package; falls back to IQR if unavailable
- **Requirement**: Minimum 10 valid values per group; uses `k = min(5, n-1)` neighbors
</details>

<details>
<summary><strong>Bootstrap</strong></summary>

Resampling method that estimates the sampling distribution of the mean via bootstrap. Flags values far from the median in terms of bootstrap standard deviation.

- **Best for**: Small samples where parametric assumptions are questionable
- **Factor range**: 1.5–3.0 (default: 1.5)
- **Samples**: 100–10,000 bootstrap replicates (default: 1000)
- **Robust**: Yes — non-parametric approach
- **Requirement**: Minimum 4 valid values per group
</details>

---

##### Trimming

Percentage-based trimming removes extreme values from each end of the distribution within groups. Applied **after** outlier detection to non-outlier rows only. Trimming is mutually exclusive with normalization — when trimming is active (>0%), normalization is automatically disabled because robust statistical tests do not require normality.

- **Range**: 0–50% (default: 0%)
- **Applied to**: Non-outlier rows only
- **Effect**: Removes lowest and highest `trim_percent`% from each group

---

##### Data Normalization

Automatic transformation using the `bestNormalize` package to achieve normality. Applied per measurement column when the proportion of non-normal groups exceeds the threshold.

<details>
<summary><strong>Normalization Process</strong></summary>

1. **Normality assessment**: Shapiro-Wilk test is run on each X-axis group per measurement column
2. **Threshold comparison**: If non-normal groups exceed the threshold percentage, transformation is triggered
3. **Transformation selection**: `bestNormalize` selects the optimal transformation (Box-Cox, Yeo-Johnson, log, square-root, or orderNorm)
4. **Column creation**: Transformed values stored in `{col}_normalized` columns

The **Show transformed values in plots** option displays normalized values on the Y-axis instead of raw values. Raw values are typically preferred for publication.
</details>

<details>
<summary><strong>Normality Assumption Checks</strong></summary>

The module performs two types of normality tests:

| Test Type | Method | Description |
|-----------|--------|-------------|
| **Per-group** | Shapiro-Wilk on raw values | Tests each X-axis group independently |
| **Residual-based** | Shapiro-Wilk on residuals | Tests ANOVA model residuals for group comparison validity |

Both tests exclude outlier and trimmed rows before testing. Results feed into the recommendation banner:

- **Green (success)**: All groups normal — parametric tests appropriate
- **Yellow (warning)**: Some groups non-normal but below threshold — parametric tests likely OK
- **Red (danger)**: Non-normal groups exceed threshold — transformation recommended

**Homogeneity check**: Levene's test on absolute deviations from group medians tests equal variances. If violated (p ≤ 0.05), Welch's ANOVA is recommended over classical ANOVA.
</details>

| Parameter | Range | Default | Description |
|-----------|-------|---------|-------------|
| **Threshold** | 10–90% | 50% | Percentage of non-normal groups required to trigger transformation |

---

##### Plot Style Options

<details>
<summary><strong>Points Panel</strong></summary>

| Setting | Range | Default | Description |
|---------|-------|---------|-------------|
| **Size** | 1–20 | 4 | Diameter of plotted points |
| **Jitter** | 0–2 | 0.15 | Horizontal spread to reduce overplotting |
| **Alpha** | 0–1 | 0.6 | Point transparency (0 = invisible, 1 = opaque) |
| **Shape by** | Descriptive columns | None | Column(s) determining point shapes (max 6 unique combinations) |
| **Color by** | X-Axis columns | X-Axis default | Column(s) determining point colors for grouping |
</details>

<details>
<summary><strong>Legend & Grid Panel</strong></summary>

| Setting | Options | Default | Description |
|---------|---------|---------|-------------|
| **Legend Position** | none, right, top, bottom, left | none | Placement of the color/shape legend |
| **Grid Lines** | Horizontal, Vertical, Top/Right | All enabled | Which grid elements to display |
| **Statistics** | Median, SD, Aspect Ratio | Median + SD | Which statistical annotations to show |

The **Aspect Ratio** option constrains the plot to a 1:1 aspect ratio for better visual comparison across plots.
</details>

<details>
<summary><strong>Median & SD Lines Panel</strong></summary>

| Setting | Range | Default | Description |
|---------|-------|---------|-------------|
| **Median Thickness** | 0.1–5 | 0.5 | Line width of median indicator |
| **Median Width** | 0.1–1 | 0.15 | Horizontal extent of median line |
| **SD Thickness** | 0.1–5 | 0.5 | Line width of standard deviation bar |
| **SD Width** | 0.1–1 | 0.15 | Horizontal extent of SD bar |
</details>

<details>
<summary><strong>Axis Settings Panel</strong></summary>

| Setting | Range | Default | Description |
|---------|-------|---------|-------------|
| **Tick Length** | 0.1–1 | 0.15 | Length of axis tick marks |
| **Line Thickness** | 0.1–5 | 0.5 | Width of axis lines |
</details>

<details>
<summary><strong>Custom Colors Panel</strong></summary>

Dynamic color pickers appear when X-axis columns are selected. The number of color groups equals the number of unique combinations across the selected color-by columns.

**How color groups are determined:**
1. If **Color by** is explicitly set, those columns define the groups
2. If **Color by** is empty, all X-axis columns define the groups
3. The interaction of group column values creates unique color categories

**Features:**
- Responsive grid layout (up to 3 columns of pickers)
- Default palette assigned automatically
- Per-group customization with color picker UI
- Colors persist across plot redraws
- Minimum 1 group, no explicit maximum (practical limit depends on screen space)

**Example**: With X-axis = `SPECIES` and `SITE` (2 species × 3 sites = 6 combinations), 6 color pickers appear for each unique `SPECIES:SITE` group.
</details>

<details>
<summary><strong>Export Settings Panel</strong></summary>

| Setting | Range | Default | Description |
|---------|-------|---------|-------------|
| **Width** | 1–50 cm | 16 cm | SVG export width |
| **Height** | 1–50 cm | 10 cm | SVG export height |

Recommended ratio: 16:10 (width:height) for typical Word document placement. Use the download button on each plot card to export individual plots as SVG files.
</details>

---

##### Nested X-Axis Design

When multiple columns are selected in **X-Axis**, the plot creates a nested hierarchical structure:

- **1 column**: Standard grouping (e.g., by `SPECIES`)
- **2 columns**: Two-level nesting (e.g., `SPECIES` × `SITE`)
- **3 columns**: Three-level nesting (e.g., `SPECIES` × `SITE` × `PERIOD`)

**Requirements**: The same number of descriptive columns must be selected in **Descriptive columns** to enable multi-level X-axis selection. The grouping follows the order of column selection — first column is the outermost grouping, last column is the innermost.

---

##### Best Practices

- **Start with no processing**: Preview raw data first before applying transformations
- **Choose outlier method based on distribution**: Use IQR/Modified Z-Score for skewed data, Z-Score for normal data, KDE for multimodal data
- **Check normality results**: Review the assumption check banner before deciding on normalization
- **Use trimming OR normalization, not both**: Trimming disables normalization automatically
- **Hide high-cardinality columns**: Hide `SAMPLE_ID` or similar columns with hundreds of unique values to keep the filter UI usable
- **Export at publication size**: Set width to 16 cm for typical Word document single-column figures
