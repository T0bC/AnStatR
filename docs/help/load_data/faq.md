#### Frequently Asked Questions

<details>
<summary>Why are the data overview panels important?</summary>

They help assess data quality before analysis. Missing data patterns directly impact statistical reliability — if a column exceeds a threshold of missing values (typically >20-30%), results may be unreliable or biased. The panels enable you to:

- Identify columns with excessive missing data that may need exclusion
- Establish whether gaps are scattered or arrive in whole blocks
- Detect data import errors (e.g., wrong delimiters causing merged columns)
- Spot unexpected value distributions indicating formatting issues
- Verify that data types were interpreted correctly during import

Spending a minute here is far cheaper than tracing a puzzling PCA or LDA result back to a data fault several tabs later.

</details>

<details>
<summary>What does the banner above the panels mean?</summary>

It lists structural faults found in the data — problems that summary statistics alone do not reveal. Each entry names the affected columns:

| Flag | Meaning | What to do |
|------|---------|------------|
| **Capitalisation collision** | Two column names are identical apart from case, e.g. `FDI` and `fdi` | Rename one in the source file; otherwise downstream selections may pick the wrong column |
| **Mostly missing** | The column is missing **≥50%** of its values | Decide whether to keep it before grouping or modelling |
| **Single value throughout** | The column has one distinct value | Harmless, but it cannot serve as a grouping variable |
| **Entirely empty** | The column has no values at all | Safe to remove from the source file |

No banner means no structural faults were found — the checks ran and passed.

</details>

<details>
<summary>Why does the missing values chart show fewer columns than my file has?</summary>

By design. All three **Missing Values** views chart only the columns that actually contain gaps. A file with 60 columns where 20 have missing values produces 20 bars, not 60 — the remaining 40 would be empty bars carrying no information, and they are what makes a chart unreadable on wide datasets.

The caption below the **By column** chart states how many columns are complete, so the full picture is still available.

</details>

<details>
<summary>Which of the three missing values views should I use?</summary>

Each answers a different question:

| View | Question it answers |
|------|--------------------|
| **By column** | Which variables are incomplete, and how badly? |
| **By row** | Are the gaps scattered across observations, or do whole blocks share them? |
| **Co-occurrence** | Which columns tend to go missing together? |

Start with **By column** to see the scale of the problem, then use **By row** to judge whether the missingness is structured. **Co-occurrence** is most useful when several columns come from the same source, instrument, or processing step — if they always go missing together, that points at the shared origin.

</details>

<details>
<summary>What does the By row raster actually show?</summary>

One horizontal line per observation, one vertical band per affected column, with missing cells marked. Rows are sorted by their missingness pattern so that observations sharing a pattern sit next to each other.

- **Contiguous bands** mean gaps arrive in blocks — whole batches, runs, or collection episodes are absent
- **Scattered speckle** means gaps arise independently per measurement

The distinction matters for your analysis: block-wise missingness can remove entire groups from a model, while scattered missingness merely thins them.

Very large datasets are sampled evenly for display; when that happens, the caption states how many rows are shown out of the total.

</details>

<details>
<summary>Co-occurrence says my missingness is scattered — what does that mean?</summary>

It means every distinct missingness pattern in your data affects only a single row: no two observations are missing exactly the same set of columns. There is no shared structure to chart, so the panel reports this rather than showing a ranking that would imply a pattern that is not there.

This is a normal and generally benign result — it suggests gaps arose independently rather than from a systematic failure. Scattered missingness is usually easier to handle than block-wise missingness.

</details>

<details>
<summary>My columns are not detected correctly</summary>

Check that your delimiter setting matches the file. Open the file in a text editor to verify the actual delimiter used. Common issues:

- **CSV files**: European systems often use semicolons (`;`) instead of commas
- **Tab-delimited files**: Ensure "Tab" is selected as the delimiter
- **Quoted fields**: If data contains commas within text fields, verify the correct quote character is selected

</details>

<details>
<summary>Numbers are imported as text</summary>

This typically occurs due to locale differences:

- **Decimal separator**: Ensure numeric columns use periods (`.`) as decimal separators, not commas
- **Thousands separators**: Remove commas used as thousands separators (e.g., change `1,234.56` to `1234.56`)
- **Currency symbols**: Strip currency symbols or units from numeric cells
- **Whitespace**: Remove leading/trailing spaces around numbers

</details>

<details>
<summary>Metadata columns are treated as measurements (or vice versa)</summary>

Review the naming conventions in the Details tab. Rename columns to follow the expected patterns:

- Change `sample_id` to `SAMPLE_ID` (metadata)
- Change `S10` to `S10z` if it is a measurement (adds lowercase letter)
- Change `asfc` to `Asfc` (mixed case for measurements)

Columns with UPPERCASE names containing digits (e.g., `S10`) trigger warnings because they are ambiguous.

</details>

<details>
<summary>The Overview says 0 complete rows — is my data broken?</summary>

Not necessarily. **Complete rows** counts rows with no gaps in *any* column, including optional metadata such as lot numbers, dates, or processing steps. In a wide table with many optional descriptive fields it is common for every row to be missing something, giving a count of zero even when all measurement columns are fully populated.

Read it alongside **Cells missing**: a low complete-row count with a small missing-cell percentage means the gaps are spread thinly across many rows, which is rarely a problem. Use the **By column** view to confirm the gaps sit in metadata rather than in the measurements you intend to analyse.

</details>

<details>
<summary>Why do the metadata and measurement counts not add up to the total columns?</summary>

Because ambiguous names are counted separately. The **Columns** figure shows badges for metadata, measurement, and — when present — ambiguous columns. Ambiguous means UPPERCASE containing digits, such as `LOT_STEP1` or `S10`, which fits neither convention cleanly.

See the Details tab for the naming convention specifications, and rename these columns if you want them classified deterministically.

</details>

<details>
<summary>How much missing data is acceptable?</summary>

As a general guideline:

- **< 5% missing**: Excellent — minimal impact on analysis
- **5-20% missing**: Acceptable — standard imputation methods work well
- **20-30% missing**: Caution — consider the pattern of missingness; MAR (Missing At Random) is preferable to MNAR (Missing Not At Random)
- **> 30% missing**: Problematic — consider excluding the column unless the data is critical

The threshold depends on your analysis method. Multivariate techniques (PCA, clustering) require stricter completeness than univariate tests.

</details>

<details>
<summary>The Missing Values views show unexpected patterns</summary>

Systematic patterns in missing data often indicate data collection issues:

- **Column-wise gaps** (**By column**): Specific instruments or methods failed for entire variables
- **Row-wise gaps** (**By row**): Certain samples had multiple measurement failures
- **Block patterns** (**By row**, contiguous bands): Data entry errors or batch processing issues
- **Linked columns** (**Co-occurrence**): Several variables share a common source that was unavailable for some records

Document these patterns before proceeding, as they may introduce bias into your analysis. If a pattern maps onto a grouping variable you intend to compare, the bias is not random — check the **Missing by Group** card in the Median tab to confirm.

</details>

<details>
<summary>Data Summary shows wrong variable types</summary>

If numeric columns appear as character/text or vice versa:

1. Check the original file for mixed data types (text entries in numeric columns)
2. Verify decimal separators are consistent
3. Look for special characters or units appended to numbers
4. Re-import with adjusted CSV settings if needed

Clean the source data and reload for best results.

</details>

<details>
<summary>How do I interpret the Data Summary statistics?</summary>

| Field | Meaning | Action |
|-------|---------|--------|
| **Type** | Data class (numeric, character, factor) | Verify matches expectations |
| **Valid** | Non-missing observations | Compare to total observations |
| **Distinct** | Unique values | High for measurements, low for metadata |
| **Mean/Median** | Central tendency | Check for impossible values |
| **Distribution** | Visual frequency plot | Identify outliers or skewness |

</details>

<details>
<summary>Can I use my data if it has no header row?</summary>

Yes, but it is not recommended. Disable "CSV includes header row" in settings, and columns will be named `V1`, `V2`, `V3`, etc. You will need to manually identify columns in downstream analysis. Adding headers to your source file is strongly preferred.

</details>

<details>
<summary>Why does my Excel file import only partial data?</summary>

The application imports only the **first sheet** of XLSX files. If your data is on another sheet:

1. Move the target data to the first sheet, or
2. Save the specific sheet as a separate CSV file

Also verify there are no empty rows at the top of the sheet, as these are skipped during import.

</details>

<details>
<summary>Which R packages are used for data loading and summary?</summary>

| Package | Purpose | Citation |
|---------|---------|----------|
| **openxlsx** | Reading xlsx files | Schauberger, P., & Walker, A. (2025). *openxlsx: Read, Write and Edit xlsx Files*. <https://doi.org/10.32614/CRAN.package.openxlsx> |
| **summarytools** | Data summary statistics | Comtois, D. (2026). *summarytools: Tools to Quickly and Neatly Summarize Data*. <https://doi.org/10.32614/CRAN.package.summarytools> |
| **ggplot2** | Missing values charts | Wickham, H. (2016). *ggplot2: Elegant Graphics for Data Analysis*. Springer-Verlag New York. <https://doi.org/10.1007/978-3-319-24277-4> |

</details>
