#' @export
box::use(
  app/logic/pca/biplot[create_biplot, biplot_error_parser],
  app/logic/pca/var_contrib[
    create_var_contrib_plot, var_contrib_error_parser
  ],
  app/logic/pca/kmo[calculate_kmo],
  app/logic/pca/na_handling[clean_na_rows],
  app/logic/pca/optimal_components[calculate_optimal_components],
  app/logic/pca/pca[validate_inputs, run_pca],
  app/logic/pca/pca_export[create_pca_excel],
  app/logic/pca/scaling[scale_data],
)
