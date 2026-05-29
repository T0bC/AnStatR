# Changelog

## [2026.11] - 2026-05-29

### Added

- **Repeated measures non-parametric post-hoc analysis**: Paired Wilcoxon signed-rank tests for within-subject comparisons in repeated measures designs
- **Paired Cliff's delta**: Effect size computation for within-subject nonparametric post-hoc comparisons; ART-only columns are blanked in mixed designs
- **Repeated-measures annotations**: Explanatory notes in post-hoc report and table clarifying paired vs unpaired test usage and effect size differences
- **t-distribution critical values**: Used for paired difference confidence intervals instead of normal approximation
- **Comprehensive test coverage**: Added repeated measures ANOVA tests for 1-way, 2-way, and 3-way designs with balanced data validation; added repeated measures post-hoc tests for 1-way and 3-way designs
- **AnStatR branding**: New icon and logo with faceted low-poly background and biplot motif, including ASR monogram variant; navbar now displays the full logo SVG at 32 px height
- **License documentation**: Added License section to README with GPL-3 and USC-RL exception for `Rallfun-v43.R`
- **Header documentation for `Rallfun-v43.R`**: Source attribution, modification notes, and license clarification

### Changed

- **Complete project rebrand**: Renamed from TexAn / TexAn2.0 to AnStatR throughout README, issue templates, documentation, UI components, CSS classes (`texan-sidebar` → `anstatr-sidebar`), and debug functions (`_texanDbg` → `_anstatrDbg`)
- **Package citations updated**: Documentation now includes DOIs and URLs for cited packages; inline citations removed from cluster analysis, LDA, and prediction documentation details sections
- **Deploy script**: Changed from `git pull` to `fetch+reset` for forced sync with `origin/main`
- **renv configuration**: Configured with Bioconductor 3.23 and enabled lockfile sanitization
- **Default clustering algorithm**: Changed from K-Means to Hierarchical with reordered algorithm choices to match the new default
- **`docs/forPublication/`**: Added to `.gitignore`

### Fixed

- Removed merge conflict marker and duplicate code from `nonparametric_tests.R`
- **Docker image build**: Removed `www/` directory from the build to reduce image size

## [2026.10] - 2026-05-22

### Added

- **3D Cluster Plot**: Interactive 3D scatter plot in the Cluster module with PCA projection and raw data visualization modes; supports cluster-colored points, centroid markers, and metadata-based grouping
- **Outlier overlay for violin/boxplot**: Optional outlier and trimmed-point layers for violin and boxplot plot types with distinct shapes and independent visibility control
- **Median & Mean point markers**: Per-group ◆ (median) and ⊕ (mean) point overlays available across all plot types with a dedicated stats legend
- **Independent alpha controls**: Separate alpha sliders for scatter points and box/violin fills when using "with points" plot types
- **Improved shape rendering**: Fillable shapes (pch 21–25) now always render with white borders; shape choices restricted to exclude solid-filled shapes for consistent border support
- **3D dimension selectors**: Auto-switching dimension selectors that update both 2D and 3D inputs when toggling between PCA and raw reduction methods

### Fixed

- **3D Cluster Plot**: Corrected return value so `renderPlotly` receives the plotly figure directly instead of a wrapped list; fixes empty/2D plot rendering
- **Cluster count calculation**: Factor cluster vectors are now converted to integer before filtering, eliminating factor comparison warnings in log output

## [2026.9] - 2026-05-22

### Added

- **Boxplot plot type**: New boxplot option in the plotting module with interactive layers and optional overlaid scatter points
- **Violin plot type**: New violin plot option with density visualization and optional overlaid scatter points
- **Plot type selector**: Dropdown to switch between scatter, boxplot, and violin plot types; appears when an X-axis grouping variable is selected
- **Black points option**: Toggle to render overlaid scatter points in black (instead of group colors) for boxplot and violin plots, improving readability when color is already used for grouping
- **Plot-type-specific settings panels**: Style and stat options panels now show only the controls relevant to the selected plot type
- **Independent alpha controls**: When "Boxplot with points" or "Violin with points" is selected, the single Alpha input splits into separate "Alpha Points" and "Alpha Box" controls, allowing independent transparency for the scatter layer and the box/violin fill
- **Median & Mean point markers**: New "Median Point" (◆) and "Mean Point" (⊕) checkboxes in the "Median & SD Lines" accordion overlay per-group summary markers on all plot types; markers are always black with fixed shapes (pch 18 / pch 13)
- **Median & SD lines extended to box/violin+points**: The existing Median crossbar and SD errorbar overlays are now available for "Boxplot with points" and "Violin with points" in addition to scatter; line thickness/width controls are now visible for all plot types
- **Stats legend**: When Legend Position is set to anything other than "none" and at least one stat overlay is active, a separate "Stats" legend box is rendered on the plot listing only the currently enabled overlays (Median line, SD, Median point, Mean point) with correct glyphs

### Changed

- Scatter points are now rendered beneath boxplot and violin layers for improved visual clarity
- **Statistics checkboxes unified**: The "Median" and "SD" checkboxes in Legend & Grid are now shown for all plot types (previously scatter-only); defaults are automatically set to Median + SD checked for scatter and violin+points, and unchecked for boxplot and boxplot+points
- **Median & SD Lines accordion**: Thickness and width controls are no longer hidden for non-scatter plot types

## [2026.8] - 2026-05-20

### Added

- **Plotting style module**: Unicode symbol previews added to shape dropdown choices for visual representation of R `pch` shapes 0–25
- **Plotting style module**: Custom shape mapping support with per-group shape dropdowns; dropdowns disable automatically when the shape-by aesthetic is active
- **Plotting style module**: Drag-and-drop factor ordering with nested sortable tree UI for multi-level X-axis groupings, with integrated color pickers per group
- **Plotting style module**: Color pickers display hex value input with a visible color swatch add-on for improved visibility
- **Dependencies**: `sortable` package added for drag-and-drop functionality

## [2026.7] - 2026-05-13

### Added

- **Plotting filter module**: "All / None" toggle link added to each checkbox group label in the filter sidebar, allowing users to select or deselect all levels of a metadata column in a single click

## [2026.6] - 2026-05-11

### Changed

- **File upload limit**: Increased maximum upload size for `.xlsx` and `.csv` files from the previous limit to **600 MB**

### Fixed

- **Statistics module**: Added immediate UI feedback when clicking "Compute Statistics" — users now see a toast notification and progress bar regardless of which statistical approach is selected, preventing the app from appearing frozen during long computations

## [2026.5] - 2026-05-05

### Changed

- **Plotting module**: Plot cards are no longer horizontally resizable; width is fixed at 100% of the container
- **Plot resize handle** (`index.js`): Drag-to-resize is vertical-only (height), with a minimum height of 150 px and double-click reset to 35 % viewport height
- **Plot card layout** (`plotting.R`): `girafeOutput` uses `height = "auto"` and `width = "100%"` inside a `responsive-plot` wrapper, ensuring plots fill available width without manual horizontal adjustment

## [2026.4] - 2026-04-20

### Added

- **Power Analysis module** — New submodule for planning sample sizes and estimating statistical power for 1-way, 2-way, and 3-way factorial ANOVA designs
- **Three solve modes** — Compute *Sample Size*, *Power*, or *Minimum Detectable Effect (MDE)* from the remaining two parameters
- **Three statistical approaches** — Parametric (F-distribution via `pwr` package), Robust (Monte Carlo with trimmed means / Welch ANOVA), and Non-Parametric (Monte Carlo with Kruskal-Wallis rank test)
- **Standardized and raw effect size input** — Enter Cohen's *f* directly or specify group means and SDs; the tool converts raw inputs to Cohen's *f* automatically
- **Median + IQR input mode** — Alternative raw input using medians and IQRs, with distribution-aware conversion to mean/SD parameters for normal, log-normal, and exponential distributions
- **Multi-distribution support** — Normal, log-normal, and exponential data-generating distributions for simulation; non-normal distributions automatically trigger Monte Carlo fallback
- **Import from Data mode** — Load pilot or completed study data in the Load Data module and import its factor structure, group means, SDs, sample sizes, and Cohen's *f* directly into the power module; distribution shape auto-detected via Shapiro-Wilk test
- **Three import workflows** — Pilot-to-full study planning (recommended sample size for full study), post-hoc power analysis (achieved power of a completed study), and sensitivity analysis (MDE given actual sample size)
- **Power curve visualization** — Interactive plot showing power vs. sample size with target power and computed *n* markers; both parametric and simulation-based curve generation supported
- **Simulated data preview** — Scatter plot of expected group data pattern based on configured parameters
- **Design table output** — Summary of *N* per cell for 1-way designs and total *N* for factorial designs
- **Binary search for sample size and MDE** — Efficient bisection algorithm (max n = 500, f range 0.01–2.0) with convergence warnings when target power is unachievable
- **Progress reporting** — Scaled progress callbacks for long-running Monte Carlo computations
- **Input validation** (`validate.R`) — Comprehensive checks for alpha, power target, group counts, effect size, distribution compatibility, and simulation parameters
- **Factor name sanitization** — Strips special characters and ensures unique factor/level names to prevent R formula parsing errors
- **Comprehensive help documentation** for Power Analysis module — Overview, Details (with import mode workflows, effect size guidance, statistical approach comparison, and best practices), and FAQ

## [2026.3] - 2026-04-10

### Added

- **Silhouette plot analysis** for Cluster module with interactive visualization and metadata grouping support
- **Silhouette plot display options** with sorting controls and average line toggle
- **Silhouette plot download handlers** with PNG/SVG export support
- **Comprehensive help documentation** for Cluster Analysis module with algorithm comparison, quality metrics, scaling guidance, and troubleshooting
- **Comprehensive help documentation** for LDA/QDA/MDA module with method comparison, PCA integration, validation strategies, and troubleshooting
- **Comprehensive help documentation** for PCA module with KMO diagnostics, scaling guidance, and component selection methods
- **Comprehensive help documentation** for Statistics module with test selection guidance, method details, and troubleshooting
- **Comprehensive help documentation** for Summary module with statistics reference and filtering behavior
- **Correlation matrix diagnostics** and visualization controls documentation to PCA help
- **LaTeX formatting** for LDA/QDA/MDA mathematical notation (converted from plain text)
- **h4/h5 heading styling** in help documentation for improved visual hierarchy (color and weight)
- **Help documentation** now included in Docker image builds

### Changed

- Updated cluster analysis help documentation - removed references section
- Removed Cross & Jain (1982) reference from Hopkins statistic documentation
- Disabled markdown linting rules for HTML tags, list spacing, first-line headings, and MD036 (emphasis vs heading)

## [2026.2] - 2026-04-07

### Fixed

- **Modified Z-Score outlier detection**: Corrected double-scaling bug where the 0.6745 constant was applied on top of an already-scaled MAD (constant=1.4826). Now uses raw MAD with proper 0.6745 scaling per Iglewicz & Hoaglin (1993)
- **Adjusted Boxplot outlier detection**: Corrected exponential coefficients to match Hubert & Vandervieren (2008). Changed from (-3.5, 4) / (-4, 3.5) to correct values (-4, 3) / (-3, 4) for MC ≥ 0 and MC < 0 respectively
- Fixed unclosed HTML `<details>` tag in plotting FAQ causing nested collapsible sections

### Changed

- Updated plotting help documentation with correct formulas and scientific references
- Revised default factor recommendations: Z-Score now defaults to 3.0, Modified Z-Score to 3.5
- Improved help documentation for load data, median calculation, and plotting modules

## [2026.1] - 2026-04-02

### Changed

- Refactored help modal with tabbed content (Overview, Details, FAQ sections)
- Help documentation restructured to per-module folders (`docs/help/{module}/`)
- Help sidebar is now user-resizable via drag handle
- Fixed cross-platform path resolution for help files (now works on Linux servers)

## [2026.0] - 2026-03-04

### Added

- Feature complete release of TexAn 2.0
- Complete rewrite using Rhino framework
- LDA (Linear Discriminant Analysis) module
- Cluster analysis module
- Median calculation module
- Interactive 2D and 3D biplots
- Theme selection in settings
- Data loading with Excel and CSV support
- Column selection and filtering capabilities

### Changed

- Modernized UI with bslib theming
- Improved data validation and error handling
