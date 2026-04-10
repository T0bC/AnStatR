# Changelog

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
