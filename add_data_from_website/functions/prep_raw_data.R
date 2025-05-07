prep_raw_data <- function(raw_data, db_overview){
  numeric_columns = db_overview |>
    dplyr::filter(table == "observation_table") |>
    dplyr::filter(data_type %in% c("BOOLEAN", "INTEGER", "FLOAT")) |>
    dplyr::pull(column_name)
  
  raw_data = dplyr::rename(
    raw_data,
    "procedure_identifier" = "presentation_identifier",
  )
  
  raw_data[grepl("^true$", raw_data, ignore.case = TRUE)] = 1
  raw_data[grepl("^false$", raw_data, ignore.case = TRUE)] = 0
  raw_data[grepl("^yes$", raw_data, ignore.case = TRUE)] = 1
  raw_data[grepl("^no$", raw_data, ignore.case = TRUE)] = 0
  
  raw_data[raw_data == TRUE] = 1
  raw_data[raw_data == FALSE] = 0
  
  clean_raw_data = raw_data |>
    dplyr::mutate(
      dplyr::across(dplyr::any_of(numeric_columns), ~as.numeric(sub(",", ".", ., fixed = TRUE)))
    )
  
  if ("rt" %in% colnames(clean_raw_data)){
    clean_raw_data$rt = ifelse(clean_raw_data$rt > 20, clean_raw_data$rt / 1000, clean_raw_data$rt)
    
    clean_raw_data$rt = ifelse(clean_raw_data$rt > 20, NA, clean_raw_data$rt)
  }
  
  clean_raw_data$response = max_normalize(clean_raw_data$response)
  
  clean_raw_data <- clean_char_columns(clean_raw_data, db_overview, "observation_table")
  
  return(clean_raw_data)
}
