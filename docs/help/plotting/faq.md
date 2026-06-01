#### Frequently Asked Questions

Common questions about plotting configuration, data processing, and troubleshooting.

---

##### Data Normalization

<details>
<summary>What is data normalization and why should I use it?</summary>

Data normalization transforms your measurement values to follow a normal (Gaussian) distribution. Many statistical tests (like classical ANOVA) assume normality, and violations can lead to incorrect p-values and false conclusions. Normalization helps meet this assumption when your raw data is skewed or has non-normal distributions.

The application uses the `bestNormalize` package to automatically select the optimal transformation (Box-Cox, Yeo-Johnson, log, square-root, or orderNorm) based on your data characteristics. The transformation is applied per measurement column when the proportion of non-normal groups exceeds your specified threshold.
</details>

<details>
<summary>When should I enable normalization?</summary>

Enable normalization when:

- The assumption check banner shows **red (danger)** — more than 50% (or your threshold) of groups are non-normal
- You plan to use parametric tests (ANOVA, t-tests) and the normality assumption is violated
- Your data is visibly skewed in the plots (long tails, asymmetric boxplots)

Skip normalization when:

- **Trimming is active** — robust statistical tests don't require normality
- All or most groups pass the Shapiro-Wilk test (banner shows green or yellow)
- You prefer to interpret raw values for publication clarity
</details>

<details>
<summary>What does the normalization threshold percentage mean?</summary>

The threshold (default 50%) determines how strict the normalization trigger is:

| Threshold | Behavior | When to Use |
|-----------|----------|-------------|
| **10–30%** | Transform when few groups are non-normal | Conservative approach; prefer normality |
| **50%** (default) | Balance between raw and transformed | General purpose recommendation |
| **70–90%** | Only transform when most groups are non-normal | Lenient; preserve raw values when possible |

Example: With a 50% threshold and 6 X-axis groups, normalization triggers when 4+ groups are non-normal. With a 30% threshold, it triggers when only 2+ groups are non-normal.
</details>

<details>
<summary>What transformation method will be applied to my data?</summary>

The `bestNormalize` package automatically evaluates multiple transformations and selects the one that produces the best Shapiro-Wilk normality statistics:

| Method | Best For | Notes |
|--------|----------|-------|
| **Box-Cox** | Positive continuous data with skewness | Requires strictly positive values |
| **Yeo-Johnson** | Continuous data with zero or negative values | Box-Cox extension allowing non-positive values |
| **Log** | Right-skewed data, multiplicative processes | Natural log transformation |
| **Square-root** | Mildly right-skewed count data | Less aggressive than log |
| **OrderNorm** | Any distribution, especially non-standard | Rank-based; forces normality but loses some information |

The selected method is shown in the transform information when normalization is applied.
</details>

<details>
<summary>Should I show transformed values in plots or keep raw values?</summary>

**Raw values (default)** are generally recommended for:

- Publication figures where readers expect original measurement units
- Interpretability — transformed values (e.g., log-scale) can be harder to explain
- Comparability with external datasets or literature values

**Transformed values** may be useful for:

- Internal exploration to visually assess normality improvement
- Teaching or demonstrations of transformation effects
- Cases where the transformation is well-known in your field (e.g., log-transformed gene expression)

Enable **Show transformed values in plots** only when you specifically need to visualize the normalized scale. The statistical tests always use the transformed values internally when normalization is enabled, regardless of this display setting.
</details>

<details>
<summary>Why is normalization disabled when I enable trimming?</summary>

Normalization is automatically disabled when trimming is active (>0%) because:

1. **Robust tests don't require normality** — Trimming prepares data for robust statistical methods (using WRS2 package) that are designed to handle non-normal distributions without transformation
2. **Mutual exclusivity** — Both operations modify your data values; applying both would create double-processing that's hard to interpret
3. **Philosophical consistency** — Trimming + robust tests is an alternative approach to normalization + parametric tests, not a complementary one

If you need normalization, set trimming to 0%. If you need trimming, rely on robust statistical methods instead of normality-dependent tests.
</details>

<details>
<summary>How do I interpret the normality check banner?</summary>

The banner combines normality and variance information:

| Color | Normality Status | Interpretation | Action |
|-------|------------------|----------------|--------|
| **Green** | All groups normal | Parametric tests appropriate | Proceed with standard analyses |
| **Yellow** | Some groups non-normal, below threshold | Parametric tests likely OK | Consider normalization if results seem questionable |
| **Red** | Non-normal groups exceed threshold | Transformation recommended | Enable normalization or use robust tests with trimming |

The variance component shows Levene's test result:
- **Equal variances assumed** — Classical ANOVA is appropriate
- **Unequal variances detected** — Consider Welch's ANOVA (handles unequal variances)
</details>

---

##### Outlier Detection

<details>
<summary>Which outlier detection method should I choose?</summary>

Select based on your data distribution and sample size:

| Distribution | Recommended Method | Factor/Threshold |
|-------------|-------------------|------------------|
| Normal/symmetric | Z-Score | 2.5–3.0 (default: 3.0) |
| Skewed | Modified Z-Score or Adjusted Boxplot | 3.5–4.5 (default: 3.5) |
| Heavy-tailed/unknown | IQR | 1.5–3.0 (default: 1.5) |
| Multimodal (multiple peaks) | KDE | 0.05–0.15 (default: 0.05) |
| Large dataset (n > 1000) | Isolation Forest | 0.05–0.10 (default: 0.05) |
| Density clusters present | LOF | 0.05–0.15 (default: 0.05) |
| Small sample (n < 20) | Bootstrap | 1.5–2.5, 1000+ samples |

When uncertain, start with **IQR** — it's robust, widely understood, and works reasonably well across most distribution types.
</details>

<details>
<summary>What happens to detected outliers in the plots?</summary>

Detected outliers are:

1. **Flagged internally** — Marked with the `{column}_outlier` flag in the processed data
2. **Excluded from statistical calculations** — Outliers don't contribute to median, SD, or normality tests
3. **Shown as hollow points in plots** — Outliers remain visible but appear as open circles, not filled points
4. **Included in export** — Outlier status is preserved when data is exported

Outliers are removed per measurement column independently. A row can be an outlier for `Asfc` but not for `epLsar`, and will appear as hollow in the `Asfc` plot but filled in the `epLsar` plot.
</details>

<details>
<summary>Why are some outlier methods disabled or falling back to IQR?</summary>

Certain methods require R packages that may not be installed:

| Method | Required Package | Fallback Behavior |
|--------|------------------|-------------------|
| Adjusted Boxplot | `robustbase` | Falls back to standard IQR |
| Isolation Forest | `isotree` | Falls back to IQR with factor 1.5 |
| LOF | `dbscan` | Falls back to IQR with factor 1.5 |

Install the required packages to enable advanced methods, or rely on IQR which requires no additional dependencies.
</details>

---

##### Filtering and Data Selection

<details>
<summary>Why would I hide columns in the Filter Data tab?</summary>

Hide columns that have high cardinality (many unique values) to prevent UI clutter:

- **SAMPLE_ID** with hundreds of unique IDs — creates hundreds of checkboxes, overwhelming the interface
- **DATE** or continuous metadata — not meaningful as discrete filter categories
- **Free-text fields** — notes, comments, or descriptions with unique entries per row

Hidden columns remain available for **Tooltip** selection, so you can still see the information when hovering over plot points. They simply don't appear as filter checkboxes.
</details>

<details>
<summary>How does the nested X-axis work with multiple columns?</summary>

When you select 2 or 3 columns in **X-Axis**, the plot creates hierarchical groupings:

**Example with SPECIES and SITE selected:**

```
X-Axis positions:
├─ Homo_sapiens
│  ├─ Site_A
│  └─ Site_B
└─ Homo_neanderthalensis
   ├─ Site_A
   └─ Site_B
```

- The **first selected column** (`SPECIES`) is the outermost grouping
- The **second selected column** (`SITE`) nests within each level of the first
- Up to 3 levels of nesting are supported

Both columns must be selected in **Descriptive columns** to appear in the X-Axis dropdown. The order of selection determines the nesting hierarchy.
</details>

---

##### Plot Styling

<details>
<summary>How does the Custom Colors panel work?</summary>

Color pickers appear dynamically based on your **Color by** selection:

1. **Default behavior**: If no columns are explicitly selected in **Color by**, colors are assigned based on the **X-Axis** column combinations
2. **Custom behavior**: Select specific columns in **Color by** to group by different criteria than the X-Axis

**Example scenario:**
- X-Axis = `SPECIES`
- Color by = `SITE`
- Result: X-axis positions grouped by species, but colors represent sites (consistent colors across species groups)

The number of color pickers equals the number of unique combinations in your selected color columns. With 2 species × 3 sites = 6 combinations, you'll see 6 color pickers.
</details>

<details>
<summary>Why is the legend not showing even when I enable it?</summary>

The legend requires either **Shape by** or **Color by** to be configured:

- **Legend Position** = "none" — no legend (default)
- **Legend Position** = "right/top/bottom/left" — legend appears only if shapes or colors are actively grouping data

If both **Shape by** and **Color by** are empty, there's nothing to put in the legend, so it won't appear regardless of position setting. Select at least one grouping column to enable the legend.
</details>

---

##### Troubleshooting

<details>
<summary>My plots are empty or show "No data available"</summary>

Check these common causes:

1. **No measurement columns selected** — Select at least one column in **Measurement columns (Y-Axis)**
2. **Filtering excluded all data** — In **Filter Data**, verify that checkboxes aren't all unchecked; at least one value per column must be selected
3. **All data flagged as outliers** — If outlier detection is too aggressive (low factor), all points may be flagged; increase the factor or disable outlier detection
4. **X-Axis columns incompatible** — Ensure X-Axis selections are a subset of Descriptive columns

If the issue persists, contact the application support: tobias.meissner(at)medizin.uni-leipzig.de.
</details>

<details>
<summary>Plots appear with overlapping points I can't distinguish</summary>

Increase point separation using:

- **Jitter** (in Points panel) — Increase from 0.15 to 0.3–0.5 for more horizontal spread
- **Point Size** — Reduce from 4 to 2–3 for smaller, less overlapping points
- **Transparency (Alpha)** — Reduce from 0.6 to 0.3–0.4 to see overlapping density

For severe overlap with many data points (>100 per group) and if that is usually the case with your data, please reach out to the application support tobias.meissner(at)medizin.uni-leipzig.de for a feature request for different types of plots (box plots, violin plots, ...).
</details>

<details>
<summary>The Shapiro-Wilk test shows "identical values" for some groups</summary>

This occurs when all values in a group are exactly the same (zero variance). Common causes:

- **Measurement precision** — Values were rounded or truncated to the same number
- **True homogeneity** — The group genuinely has no variation (e.g., all control samples at baseline)
- **Data error** — Duplicate rows or import issues creating identical entries

Groups with identical values are excluded from the normality proportion calculation (they don't count as "normal" or "non-normal"). If many groups show this, verify your data quality before proceeding.
</details>

---

##### Technical Questions

<details>
<summary>Why can I only select up to 3 X-Axis columns?</summary>

The 3-column limit balances visualization clarity with analytical flexibility:

- **1 column**: Standard comparison (e.g., by species)
- **2 columns**: Interaction analysis (species × site)
- **3 columns**: Complex designs (species × site × period)

Beyond 3 columns, the X-axis becomes overcrowded, labels overlap, and plots become difficult to interpret. For designs requiring more factors, consider using the **Filter Data** tab to subset by additional criteria rather than adding more X-axis levels.
</details>

<details>
<summary>How are median and SD lines calculated?</summary>

| Statistic | Calculation | Outlier Handling |
|-----------|-------------|------------------|
| **Median** | Middle value of sorted data | Excludes outliers and trimmed points |
| **SD** | Standard deviation | Excludes outliers and trimmed points |

The median line spans horizontally with thickness controlled by **Median Width**. The SD bar extends from (median - SD) to (median + SD) with thickness controlled by **SD Width**.

When **Trimming** is active, trimmed points are also excluded from these calculations. When **Outlier Detection** is enabled, flagged outliers don't contribute to the statistics.
</details>

<details>
<summary>Which R packages power the Plotting module?</summary>

| Package | Purpose | Citation |
|---------|---------|----------|
| **dbscan** | LOF outlier detection | Hahsler, M., & Piekenbrock, M. (2025). *dbscan: Density-Based Spatial Clustering of Applications with Noise (DBSCAN) and Related Algorithms*. <https://doi.org/10.32614/CRAN.package.dbscan> |
| **ggiraph** | Interactive SVG plots | Gohel, D., & Skintzos, P. (2026). *ggiraph: Make 'ggplot2' Graphics Interactive*. <https://doi.org/10.32614/CRAN.package.ggiraph> |
| **ggplot2** | Plot generation and styling | Wickham, H. (2016). *ggplot2: Elegant Graphics for Data Analysis*. Springer. <https://ggplot2.tidyverse.org> |
| **isotree** | Isolation Forest outlier detection | Cortes, D. (2026). *isotree: Isolation-Based Outlier Detection*. <https://doi.org/10.32614/CRAN.package.isotree> |
| **legendry** | Extended legends and axes | van den Brand, T. (2026). *legendry: Extended Legends and Axes for 'ggplot2'*. <https://doi.org/10.32614/CRAN.package.legendry> |
| **robustbase** | Adjusted boxplot outlier detection | Maechler, M., Rousseeuw, P., Croux, C., Todorov, V., Ruckstuhl, A., Salibian-Barrera, M., Verbeke, T., Koller, M., Conceicao, E. L. T., & di Palma, M. A. (2026). *robustbase: Basic Robust Statistics*. <http://robustbase.r-forge.r-project.org/> |

</details>
