#' @export
box::use(
  app/logic/pca/biplot3d[biplot3d_error_parser, create_biplot3d],
  app/logic/pca/biplot[biplot_error_parser, create_biplot],
  app/logic/pca/eigencorplot[
    compute_eigencor_data,
    create_eigencor_plot,
    eigencor_error_parser
  ],
  app/logic/pca/ind_contrib[
    create_ind_contrib_plot,
    ind_contrib_error_parser
  ],
  app/logic/pca/kmo[calculate_kmo],
  app/logic/pca/optimal_components[calculate_optimal_components],
  app/logic/pca/pca[
    extract_variance_explained,
    run_pca,
    run_pca_tune_keepx,
    validate_inputs
  ],
  app/logic/pca/pca_export[create_pca_excel],
  app/logic/pca/pca_stats[
    compute_ind_contrib,
    compute_ind_cos2,
    compute_var_contrib,
    compute_var_coord,
    compute_var_cos2
  ],
  app/logic/pca/scaling[scale_data],
  app/logic/pca/tune_plot[create_tune_spca_plot],
  app/logic/pca/var_contrib_jitter[
    create_var_contrib_jitter_plot,
    var_contrib_jitter_err_parser
  ],
  app/logic/preprocessing/na_handling[clean_na_rows],
)
