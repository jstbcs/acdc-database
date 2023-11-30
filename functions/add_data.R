#' Add Data to Database
#'
#' This function adds a new data entry to a database. It associates the data with a specified study, group keys, and other relevant information. The function first determines the next available IDs for various tables, adds the data to those tables, and replaces keys in the data as necessary.
#'
#' @param conn The connection object or database connection string.
#' @param entry_data A list or data frame containing the data to be added, including tables for tasks, datasets, within-subject factors, conditions, and observations.
#' @param study_id The ID of the study to which the data will be associated.
#'
#' @export
add_data <- function(conn, entry_data, study_id){
  # Add task and get the task id
  task_id = find_next_free_id(conn, "task_table")
  entry_data$task_table$task_id = task_id
  entry_data$dataset_table$task_id = task_id
  task_table = entry_data$task_table
  add_table(conn, task_table, "task_table")
  
  # Get dataset id
  dataset_id = find_next_free_id(conn, "dataset_table")
  
  entry_data$dataset_table$dataset_id = dataset_id
  entry_data$dataset_table$study_id = study_id
  entry_data$dataset_table$task_id = task_id
  
  entry_data$within_table$dataset_id = dataset_id

  entry_data$observation_table$dataset_id = dataset_id
  
  # Add within
  within_id = find_next_free_id(conn, "within_table")
  for (row in 1:nrow(entry_data$within_table)){
    entry_data$within_table$within_id[row] = within_id
    within_id = within_id + 1
  }
  
  within_keys = obtain_keys(entry_data$within_table,
                            "within")
  
  # Sometimes condition, between, or within might be NA, replace those
  if (all(is.na(entry_data$observation_table$within))){
    entry_data$observation_table$within = 1
  }
  if (all(is.na(entry_data$condition_table$within_name))){
    entry_data$condition_table$within_name = 1
  }
  
  # Replace group, within, condition in data
  entry_data$observation_table = entry_data$observation_table %>% 
    replace_id_keys_in_data(., within_keys, "within")
  
  # Add all tables
  dataset_table = entry_data$dataset_table
  add_table(conn, dataset_table, "dataset_table")
  
  within = entry_data$within_table
  add_table(conn, within, "within_table")
  
  # Find next free subject number
  sql_query = paste0(
    "SELECT max(subject) FROM observation_table"
  )
  
  max_subject = DBI::dbGetQuery(conn, sql_query)[1, 1]
  if (is.na(max_subject)){
    max_subject = 0
  }
  entry_data$observation_table$subject = dplyr::dense_rank(entry_data$observation_table$subject) + max_subject
  
  observation = as.data.frame(entry_data$observation_table)
  
  add_table(conn, observation, "observation_table")
}