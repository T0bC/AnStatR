# Detailed Guide

## File Requirements

Your data file should contain:

- **Header row** — Column names in the first row (recommended)
- **Numeric columns** — Measurement values for analysis
- **Grouping columns** — Optional categorical columns for grouping (e.g., Site, Period, Type)

## CSV Import Options

When importing CSV files, you can customize:

| Setting | Options | Default |
|---------|---------|---------|
| Delimiter | Comma, Semicolon, Tab | Comma |
| Quote character | Double quote, Single quote, None | Double quote |
| Header row | Yes / No | Yes |

## Excel Import

For XLSX files:

- Only the **first sheet** is imported
- Empty rows at the top are automatically skipped
- Column types are auto-detected

## Troubleshooting

<details>
<summary>My columns are not detected correctly</summary>

Check that your delimiter setting matches the file. Open the file in a text editor to verify the actual delimiter used.

</details>

<details>
<summary>Numbers are imported as text</summary>

This can happen if your file uses a different decimal separator (e.g., comma instead of period). Ensure numeric columns use period as decimal separator.

</details>
