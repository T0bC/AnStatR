#### Frequently Asked Questions

<details>
<summary>Why are some columns removed during median calculation?</summary>

Descriptive columns (UPPERCASE names like `SAMPLE_ID`, `SITE`) that **vary within groups** are automatically removed. This happens because:

- Median calculation aggregates multiple rows into one per group
- A column like `MEASUREMENT_TYPE` might differ for each row within a sample
- It is impossible to assign a single value to the aggregated row

**Solution**: Only columns that are constant within your grouping level will be retained. If you need to preserve varying metadata, use a different grouping level or process data in separate batches.

</details>

<details>
<summary>What is the difference between grouping and not grouping?</summary>

| Mode | Behavior | Use Case |
|------|----------|----------|
| **No grouping** | Quality filter only; returns all filtered rows | Review/filter data without aggregation |
| **With grouping** | Calculates median per group; one row per unique group combination | Summarize replicate measurements per sample |

Example: If you have 5 measurements per sample and group by `SAMPLE_ID`, the result will have one row per sample with median values across the 5 replicates.

</details>

<details>
<summary>How does group-aware quality filtering work?</summary>

When grouping is enabled, the filter preserves groups that have only low-quality measurements:

- **Group with good + bad values**: Bad rows removed, group retained with good values only
- **Group with only bad values**: Entire group kept intact (not removed from dataset)

This prevents losing samples entirely when all their measurements happen to be low quality. Without grouping, bad rows are simply deleted.

</details>

<details>
<summary>My quality column is not detected correctly</summary>

Quality column auto-detection may misclassify in these cases:

- **Integer codes (1, 2, 3, 4, 5) with >10 unique values**: Treated as numeric instead of categorical
- **Percentage stored as 0-100 with few unique values**: May be detected as categorical

**Workaround**: The filter interface will still work — just select the appropriate values or threshold manually. The detection is for convenience only.

</details>

<details>
<summary>What threshold should I use for quality filtering?</summary>

Guidelines by quality column type:

| Type | Typical Good Threshold | Interpretation |
|------|----------------------|----------------|
| **Percentage (0-1)** | ≥0.8 | 80% or higher quality |
| **Percentage (0-100)** | ≥80 | 80% or higher quality |
| **Numeric (e.g., signal-to-noise)** | Domain-specific | Higher values = better quality |
| **Categorical (grades)** | Exclude known bad grades | e.g., exclude "Poor", "Failed" |

When in doubt, compare the distribution of quality values in your data preview to identify a natural cutoff.

</details>

<details>
<summary>Why is the median result table empty?</summary>

Possible causes:

- **No measurement columns**: Check that your data has mixed-case column names (e.g., `Asfc`, not `ASFC`)
- **All values filtered out**: Quality filter too aggressive; check filter settings
- **No numeric data after filtering**: All rows removed by quality or grouping constraints

Check the Processing Summary for messages about what was filtered or removed.

</details>

<details>
<summary>Can I group by measurement columns?</summary>

No — grouping is limited to **descriptive columns** (UPPERCASE names). Measurement columns contain the values being aggregated, so they cannot define the groups.

If you need to group by a measurement-derived category, create a categorical version in your source data (e.g., `SIZE_CATEGORY` with values "Small"/"Medium"/"Large" derived from a numeric measurement).

</details>

<details>
<summary>How are missing values (NA) handled in median calculation?</summary>

- **Median calculation**: `median(x, na.rm = TRUE)` — NAs are ignored per group
- **Quality filtering**: NAs in the quality column are treated as **bad values** (below threshold)
- **Result**: If all values in a group are NA, the median will be NA for that measurement

</details>

<details>
<summary>The downloaded file does not match what I see in the table</summary>

The download button exports the **DT-filtered data** — meaning any active column filters in the table are applied to the export. To download all data:

1. Clear all column filters in the table header (select all values in each dropdown)
2. Click **Download Filtered Data**

The downloaded file will match the currently visible table rows.

</details>

<details>
<summary>What are the two charts below the results table for?</summary>

They show what your grouping selection does to the data, while you can still change it:

| Chart | Question it answers |
|-------|--------------------|
| **Design Balance** | How many observations land in each group combination, and which groups are too thin to trust? |
| **Missing by Group** | Are measurement gaps spread evenly, or concentrated in particular groups? |

Both are computed from the data **before** median aggregation, since that is the data the medians are drawn from. They update together with the results table whenever you change the grouping or quality settings.

</details>

<details>
<summary>Why are some bars in Design Balance highlighted?</summary>

Those groups contain fewer than **3** observations. A median over one or two values is not a meaningful summary — it is simply one of the values, or the average of two — so any downstream comparison involving those groups rests on very little.

If highlighted bars are common, consider grouping at a coarser level: dropping the most granular column (facet, for instance) merges thin groups into larger ones. The caption reports the total count so you can judge the scale of the problem at a glance.

</details>

<details>
<summary>What does the coverage percentage in Design Balance mean?</summary>

It compares the group combinations that actually occur in your data against the number the full crossing of your selected columns would allow. Selecting three columns with 5, 4 and 3 levels allows 60 combinations; if only 20 occur, coverage is roughly 33%.

Low coverage is normal for material that is incomplete by origin — not every individual contributes every tooth, not every facet is measurable, some attributes are unrecorded. It is a description of how sparse your design is, not an error. It matters because sparse designs support fewer comparisons than the raw row count suggests.

</details>

<details>
<summary>The Design Balance axis labels disappeared</summary>

Your selection produced more group combinations than can be labelled legibly, so the tick labels are omitted and the caption carries the counts instead.

The chart is still informative: the shape of the distribution shows whether observations are spread evenly or pile up in a few combinations. To get labels back, group by fewer columns, or by columns with fewer levels — high-cardinality identifiers such as `SAMPLE_ID` multiply the combination count quickly.

</details>

<details>
<summary>How do I read the axis brackets in Design Balance?</summary>

The brackets show the hierarchy of your grouping columns. The **first** column you select forms the outermost bracket, the **last** forms the innermost tick labels, with intermediate columns nested between them.

Reordering your selection therefore changes how the chart is organised without changing the underlying counts — put the column you want to compare across first, so its groups read as the top-level blocks.

</details>

<details>
<summary>Missing by Group is empty — is that a problem?</summary>

No, it is the good outcome. The heatmap covers only measurement columns that contain at least one missing value; when none do, there is nothing to plot and the panel says so.

It means every measurement is present for every row in the current grouping, and no group comparison is weakened by absent values.

</details>

<details>
<summary>One group shows 100% missing for a measurement — what do I do?</summary>

That measurement cannot contribute to any comparison involving that group, regardless of how many rows the group contains. The median for that cell will be NA.

Options, in rough order of preference:

1. **Exclude the measurement** from analyses that compare across that grouping, if it is absent for a whole group
2. **Merge the group** into a coarser level where the measurement is present
3. **Exclude the group** from that particular comparison, and say so when reporting

Whichever you choose, record it — a measurement absent by group is a structural property of the dataset, not random noise, and it will otherwise resurface as an unexplained result in PCA, LDA, or clustering.

</details>

<details>
<summary>Can I use multiple quality columns?</summary>

No — only one quality column can be selected at a time. To combine multiple quality criteria:

1. Pre-filter your data externally, or
2. Create a composite quality score in your source data (e.g., `OVERALL_QUALITY` derived from multiple metrics)

</details>

<details>
<summary>Which R packages are used for the design charts?</summary>

| Package | Purpose | Citation |
|---------|---------|----------|
| **ggplot2** | Design balance and missingness charts | Wickham, H. (2016). *ggplot2: Elegant Graphics for Data Analysis*. Springer-Verlag New York. <https://doi.org/10.1007/978-3-319-24277-4> |
| **legendry** | Nested grouping axis | van den Brand, T. (2025). *legendry: Extended Legends and Axes for 'ggplot2'*. <https://doi.org/10.32614/CRAN.package.legendry> |

</details>
