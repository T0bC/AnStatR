# Changelog

## [2.0.5] - 2026-05-05

### Changed

- **Plotting module**: Plot cards are no longer horizontally resizable; width is fixed at 100% of the container
- **Plot resize handle** (`index.js`): Drag-to-resize is vertical-only (height), with a minimum height of 150 px and double-click reset to 35 % viewport height
- **Plot card layout** (`plotting.R`): `girafeOutput` uses `height = "auto"` and `width = "100%"` inside a `responsive-plot` wrapper, ensuring plots fill available width without manual horizontal adjustment

## [2.0.4] - 2026-04-20

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

## [2.0.3] - 2026-04-10

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

## [2.0.2] - 2026-04-07

### Fixed

- **Modified Z-Score outlier detection**: Corrected double-scaling bug where the 0.6745 constant was applied on top of an already-scaled MAD (constant=1.4826). Now uses raw MAD with proper 0.6745 scaling per Iglewicz & Hoaglin (1993)
- **Adjusted Boxplot outlier detection**: Corrected exponential coefficients to match Hubert & Vandervieren (2008). Changed from (-3.5, 4) / (-4, 3.5) to correct values (-4, 3) / (-3, 4) for MC ≥ 0 and MC < 0 respectively
- Fixed unclosed HTML `<details>` tag in plotting FAQ causing nested collapsible sections

### Changed

- Updated plotting help documentation with correct formulas and scientific references
- Revised default factor recommendations: Z-Score now defaults to 3.0, Modified Z-Score to 3.5
- Improved help documentation for load data, median calculation, and plotting modules

## [2.0.1] - 2026-04-02

### Changed

- Refactored help modal with tabbed content (Overview, Details, FAQ sections)
- Help documentation restructured to per-module folders (`docs/help/{module}/`)
- Help sidebar is now user-resizable via drag handle
- Fixed cross-platform path resolution for help files (now works on Linux servers)

## [2.0.0] - 2026-03-04

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
