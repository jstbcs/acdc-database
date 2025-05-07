clean_char_columns <- function(data, db_overview, table_name){
  character_columns = db_overview |>
    dplyr::filter(table == table_name) |>
    dplyr::filter(grepl("VARCHAR", data_type)) |>
    dplyr::pull(column_name)
  
  data = dplyr::mutate(
    data, 
    dplyr::across(dplyr::any_of(character_columns), ~tolower(trimws(.)))
  )
  
  return(data)
}