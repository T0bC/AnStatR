#' @export
box::use(
  app/logic/load_data/example_data[
    example_path,
    list_examples,
    load_example
  ],
  app/logic/load_data/load_data[
    normalize_quote_char,
    read_data_file,
    validate_data,
    validate_file_extension
  ],
)
