prep_study_data <- function(study_data, db_overview){
  study_data$statementset_idx = ifelse(
    study_data$statementset_name == "no information",
    0,
    readr::parse_number(study_data$statementset_name)
  )
  
  study_data = dplyr::rename(
    study_data,
    "raw_data_link" = "open_data_link",
    "filler_task_yesno" = "secondary_tasks",
    "filler_task_type" = "secondary_task_type",
    "filler_task_name" = "secondary_task_name"
  )
  numeric_columns = db_overview |>
    dplyr::filter(table == "study_table") |>
    dplyr::filter(data_type %in% c("BOOLEAN", "INTEGER", "FLOAT")) |>
    dplyr::pull(column_name)
  
  study_data = dplyr::mutate(
    study_data, 
    dplyr::across(dplyr::any_of(numeric_columns), ~as.numeric(sub(",", ".", ., fixed = TRUE)))
  )
  
  study_data = clean_char_columns(study_data, db_overview, "study_table")
  
  return (study_data)
}
