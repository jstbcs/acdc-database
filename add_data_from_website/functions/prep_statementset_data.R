prep_statementset_data <- function(statementset_data, db_overview){
  possible_columns = db_overview[db_overview$table == "statement_table", "column_name"]
  
  possible_columns = gsub("_id$", "_identifier", possible_columns)
  
  clean_statementset_data = dplyr::select(statementset_data, dplyr::any_of(possible_columns))
  
  numeric_columns = db_overview |>
    dplyr::filter(table == "statement_table") |>
    dplyr::filter(data_type %in% c("BOOLEAN", "INTEGER", "FLOAT")) |>
    dplyr::pull(column_name)
  
  clean_statementset_data = dplyr::mutate(
    clean_statementset_data,
    dplyr::across(dplyr::any_of(numeric_columns), ~as.numeric(sub(",", ".", ., fixed = TRUE)))
    )
  
  clean_statementset_data <- clean_char_columns(clean_statementset_data, db_overview, "statement_table")
  
  return (clean_statementset_data)
}