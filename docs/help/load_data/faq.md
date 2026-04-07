#### Frequently Asked Questions

<details>
<summary>Why are the data visualization panels important?</summary>

The visualization panels help assess data quality before analysis. Missing data patterns directly impact statistical reliability — if a column exceeds a threshold of missing values (typically >20-30%), results may be unreliable or biased. The panels enable you to:

- Identify columns with excessive missing data that may need exclusion
- Detect data import errors (e.g., wrong delimiters causing merged columns)
- Spot unexpected value distributions indicating formatting issues
- Verify that data types were interpreted correctly during import

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
<summary>How much missing data is acceptable?</summary>

As a general guideline:

- **< 5% missing**: Excellent — minimal impact on analysis
- **5-20% missing**: Acceptable — standard imputation methods work well
- **20-30% missing**: Caution — consider the pattern of missingness; MAR (Missing At Random) is preferable to MNAR (Missing Not At Random)
- **> 30% missing**: Problematic — consider excluding the column unless the data is critical

The threshold depends on your analysis method. Multivariate techniques (PCA, clustering) require stricter completeness than univariate tests.

</details>

<details>
<summary>The Missing Values chart shows unexpected patterns</summary>

Systematic patterns in missing data often indicate data collection issues:

- **Column-wise gaps**: Specific instruments or methods failed for entire variables
- **Row-wise gaps**: Certain samples had multiple measurement failures
- **Block patterns**: Data entry errors or batch processing issues

Document these patterns before proceeding, as they may introduce bias into your analysis.

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
