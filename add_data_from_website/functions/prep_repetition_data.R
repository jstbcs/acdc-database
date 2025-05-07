prep_repetition_data <- function(repetition_data, db_overview){
  numeric_columns = db_overview |>
    dplyr::filter(table == "procedure_table") |>
    dplyr::filter(data_type %in% c("BOOLEAN", "INTEGER", "FLOAT")) |>
    dplyr::pull(column_name)
  
  clean_repetition_data = dplyr::mutate(
    repetition_data, 
    dplyr::across(dplyr::any_of(numeric_columns), ~as.numeric(sub(",", ".", ., fixed = TRUE)))
  )
  
  clean_repetition_data = dplyr::rename(
    clean_repetition_data,
    "procedure_identifier" = "presentation_identifier",
  )
  
  clean_repetition_data <- clean_char_columns(clean_repetition_data, db_overview, "procedure_table")
  
  
  return(clean_repetition_data)
}