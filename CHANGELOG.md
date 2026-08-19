# Changelog

## [2026.14] - 2026-08-19

### Added

- **PLS-DA / sPLS-DA**: New "PLS-DA" and "sPLS-DA (sparse)" analysis types in the LDA module (via the `mixOmics` package), designed for measurement-parameter sets with more variables than specimens per group and/or highly collinear variables — situations where LDA/QDA/MDA warn or fail outright. PLS-DA extracts latent components maximizing covariance between measurements and group membership; sPLS-DA additionally performs sparse variable selection (**keepX** per component), directly identifying which measurement parameters drive group separation, shown in a new **Selected Variables** results panel
- **keepX auto-tuning**: Opt-in "Optimise variable selection (recommended)" button runs a cross-validated grid search (`mixOmics::tune.splsda`) to suggest keepX values per component, filling the manual inputs while remaining user-editable
- **Component Diagnostics (perf) panel**: Independent, on-demand repeated k-fold cross-validation (`mixOmics::perf`) reporting Overall Error and Balanced Error Rate per component count, used to justify the chosen number of components for PLS-DA/sPLS-DA (button labelled "Check component count (recommended)")
- **Decision boundary overlay for PLS-DA/sPLS-DA**: The existing "Show Decision Boundaries" shaded-background overlay (previously LDA/QDA/MDA only) now also covers PLS-DA/sPLS-DA, approximated via nearest-neighbour classification on training scores in the plotted 2D projection
- **Prediction module support**: PLS-DA/sPLS-DA models can be exported as `.rds` bundles and loaded into the Prediction module to classify unknown specimens and overlay them on the Component Scores plot, reusing the existing LDA/MDA overlay infrastructure
- **Documentation**: Help files (Overview/Details/FAQ) for LDA updated with PLS-DA/sPLS-DA method descriptions, component/keepX selection guidance, and multicollinearity interpretation notes; package citation table extended with mixOmics
- **Dependencies**: `mixOmics` (Bioconductor) locked in `renv.lock`
- **"Best axes to plot" recommendation**: The Dimension Evaluation panel now names the two axes with the highest ANOVA R² — the axes carrying the most group separation — and, when those are not the first two, points to the Plotting Controls tab to change Dim.X/Dim.Y. Unlike LDA (whose axes are ordered by discriminating power by construction), PLS-DA/sPLS-DA components are ordered by X–group covariance, so the strongest group signal is not always on Comp1/Comp2
- **keepX tuning status**: The Selected Variables panel carries a "keepX tuned" / "keepX not tuned" badge plus an in-panel note, so a variable list produced by an untuned sidebar default is never mistaken for a cross-validated result. The badge tracks the values actually used by the fitted model, reverting to "not tuned" if keepX is hand-edited after tuning
- **Reporting guidance**: New FAQ entry "What should I report in a paper or thesis?" consolidating which numbers and plots belong in a manuscript, which metrics are misleading when quoted alone, and a placeholder template summary; the pattern is documented in `.llm/help_modal_guide.md` for reuse across other analysis modules
- **Dimension Evaluation test coverage**: New `tests/testthat/test-dimension_eval.R` covering the grouping-column-available and grouping-column-missing paths
- **Component error curve**: The Component Diagnostics panel now leads with an interactive ggiraph plot of cross-validated Overall Error and BER per component — the classification-error analogue of the PCA scree plot, read the same way (keep components up to the elbow) but able to show error *rising* again, which is direct evidence of overfitting that a variance-based scree plot cannot give. A dashed line marks the smallest component count within one percentage point of the best BER
- **Decision boundary rule selector (PLS-DA/sPLS-DA)**: New "Boundary rule" control in the Plotting Controls tab exposing the three `dist` rules mixOmics offers via `background.predict()` — maximum distance (default, matches the reported accuracy), centroid distance, and Mahalanobis distance. All three are computed on demand from the fitted model, so switching is instant. Maximum- and centroid-distance backgrounds reproduce `mixOmics::predict()` exactly at the training points; the Mahalanobis background uses the covariance of the two plotted components rather than all fitted ones, as a 2D picture necessarily can
- **Validation transparency**: The Summary panel now states plainly when a result is unvalidated — an accuracy measured on the same specimens the model was fitted to is optimistic by construction and should not be reported as performance — and points at the Validation setting. When Leave-one-out CV or Train/Test Split is active, the validated and resubstitution figures are shown side by side with the gap between them in percentage points, colour-coded (under 10 pp generalises well, 10–15 pp some overfitting, over 15 pp fitting noise). Previously users had to run the analysis twice and compare by hand to see this. Resubstitution is now computed in LOO-CV mode for LDA/QDA/MDA via a single extra full-data fit, negligible next to LOO-CV's n fits

### Changed

- **Method-aware UI labels**: The results accordion is now titled after the selected method ("sPLS-DA Results" rather than always "LDA Results"), via a single `analysis_type_label()` helper shared with the results summary. The compute button, empty-state header, and plotting-controls title were made method-neutral ("Compute Discriminant Analysis", "Discriminant Analysis Plotting Controls") instead of enumerating only LDA/QDA/MDA, and the Dim.X/Dim.Y tooltips now mention Comp for PLS-DA/sPLS-DA
- **Group Means table transposed**: Variables are now rows and groups columns, matching the Coefficients/VIP panels. Datasets normally have far more measurement parameters than groups, so the previous orientation forced horizontal scrolling that pushed the row label off-screen. The Excel export keeps groups as rows for spreadsheet convenience
- **Button labels state purpose rather than mechanism**: "Auto-tune keepX (slow)" → "Optimise variable selection (recommended)" and "Run Component Diagnostics (perf)" → "Check component count (recommended)"; the runtime warning moved into the helper text so it informs without discouraging a step users should almost always take. The keepX tooltip now explains that keepX is a count of variables rather than a threshold, and that it should be chosen by cross-validation rather than guessed
- **Variable Contributions plot height**: The SVG height cap was raised from 8 to 24 in both the LDA and PCA jitter plots. The previous cap saturated at roughly 14 variables per axis, guaranteeing overlapping labels for the 40+-parameter datasets the module targets
- **In-UI caveats surfaced from the docs**: The Explained Variance table carries a PLS-DA/sPLS-DA-specific note that a low proportion does not imply weak group separation, and the Confusion Matrix panel explains (when `ncomp > 2`) that classification uses all components while the scores plot shows only two — so the two can legitimately disagree

### Fixed

- **Dimension Evaluation could report fabricated significance**: When the grouping column was not also selected as a metadata column, `get_grouping()` silently substituted the model's own predicted classes for the true group labels. The ANOVA then regressed the model's scores on its own predictions — circular by construction, and near-guaranteed to produce an inflated R²/F/p-value with no indication to the user. The fallback was removed; the panel is now omitted with an explicit message instead
- **MDA could display a negative explained-variance proportion**: `mda::mda()`'s `percent.explained` is documented as cumulative, but EM instability or dimension truncation can return a non-monotonic vector, which differencing turned into a negative Proportion and a decreasing Cumulative column that propagated silently into the Variable Contributions plot. Negative values are now clamped to zero with a logged warning, and Cumulative is recomputed to stay consistent
- **VIP highlighting applied to one component only**: The VIP table's caption claimed "Variables with VIP > 1 (highlighted)" while styling was applied only to the Comp1 column; it now covers every component column
- **Documentation corrections**: The MDA per-group sample-size rule was stated three different ways across the FAQ, overview table, and sidebar tooltip — all now state the enforced `max(subclasses, p + 1)` minimum, distinguished from the softer "about 10 per subclass" stability recommendation. The Excel export description listed 6 of the 14 sheets actually written and is now a complete conditional table; the stale "up to nine sub-panels" count was removed
- **Group Means panel could crash for MDA under LOO-CV**: MDA in cross-validation mode fits no single full-data model and therefore has no group means, but the panel was built unconditionally — after the table was transposed this turned an empty table into an error that took down the whole results panel. The panel is now skipped when means are unavailable
- **Two failing `run_plsda_perf` tests**: The tests read `perf_res$result` directly, but the function returns `list(errors =, stability =)`; the assertions were corrected to target `$errors`. Production code was already consuming the result correctly

## [2026.13] - 2026-08-18

### Added

- **Cluster prediction (K-Means / PAM)**: K-Means and PAM clusters fit on raw measurement data can now be exported as an `.rds` bundle (**Download RDS (for Prediction)** in the Cluster Results panel) and loaded into the **Prediction** module to assign new/unknown samples to the nearest centroid (K-Means) or medoid (PAM) — the same out-of-sample rule the algorithms use internally. Hierarchical clustering, DBSCAN, and clusters built on PCA/LDA scores remain unsupported for export, since none has a principled out-of-sample assignment rule (or, for scores, would require nesting an upstream PCA/LDA bundle)
- **Documentation**: Help files (Overview/Details) for Cluster and Prediction updated to document cluster export/prediction and its scope

## [2026.12] - 2026-08-18

### Added

- **Parameter screening mode**: New "Disable plots (parameter screening mode)" checkbox in the Plotting tab's Data Selection sidebar skips plot and diagnostics generation, enabling fast screening of large measurement-parameter sets (40+ columns) by passing selections straight through to the Statistics tab
- **Parameter separation ranking**: The Statistics tab now computes a "Parameter Screening — Separation Ranking" table when screening mode is active, ranking every measurement parameter by p-value and effect size across all pairwise group comparisons using the already-computed post-hoc results, and deriving a recommended parameter subset
- **Recommendation banners in PCA, LDA, and Cluster**: A shared "Apply recommended parameters" banner (`app/view/shared/recommendation_banner.R`) lets users apply the Statistics tab's recommended parameter set with one click in the PCA, LDA, and Cluster (raw-data mode only) Data Selection tabs, refining the parameter-preselection workflow introduced by the ['trident'](https://doi.org/10.24072/pcjournal.467) Shiny app for dental microwear texture analysis (Thiery et al., 2024) into a dataset-agnostic screening step
- **Auto-select metadata columns in GroupBiplot**: The PCA GroupBiplot metadata selector now auto-populates from the selected metadata columns when empty
- **Documentation**: Help files (Overview/Details/FAQ) for Plotting, Statistics, PCA, LDA, and Cluster updated to document the parameter screening workflow and its citation

### Changed

- Recommended parameters selected via the banner are applied through the same column-selection mechanism as manual selection, so PCA/LDA `.rds` bundle export and Prediction module alignment behave identically regardless of how parameters were chosen — no code changes were required in Prediction

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

- Feature complete release of AnStatR
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
