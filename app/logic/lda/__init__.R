#' @export
box::use(
  app/logic/lda/data_splitting[
    create_stratified_split,
  ],
  app/logic/lda/lda[
    lda_error_parser,
    run_lda,
    run_predict,
    run_qda,
    validate_inputs,
  ],
  app/logic/lda/lda_export[
    create_lda_excel,
  ],
)
