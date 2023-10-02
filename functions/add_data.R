add_data <- function(conn, entry_data, study_id, group_keys){
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
  entry_data$condition_table$dataset_id = dataset_id
  
  entry_data$observation_table$dataset_id = dataset_id
  
  # Add within
  within_id = find_next_free_id(conn, "within_table")
  for (row in 1:nrow(entry_data$within_table)){
    entry_data$within_table$within_id[row] = within_id
    within_id = within_id + 1
  }
  
  within_keys = obtain_keys(entry_data$within_table,
                            "within")
  
  # Add condition
  condition_id = find_next_free_id(conn, "condition_table")
  for (row in 1:nrow(entry_data$condition_table)){
    entry_data$condition_table$condition_id[row] = condition_id
    condition_id = condition_id + 1
  }
  
  condition_keys = obtain_keys(entry_data$condition_table,
                               "condition")
  
  
  # Sometimes condition, between, or within might be NA, replace those
  if (all(is.na(entry_data$observation_table$between))){
    entry_data$observation_table$between = 1
  }
  if (all(is.na(entry_data$observation_table$within))){
    entry_data$observation_table$within = 1
  }
  if (all(is.na(entry_data$observation_table$condition))){
    entry_data$observation_table$condition = 1
  }
  
  # Replace group, within, condition in data
  entry_data$observation_table = entry_data$observation_table %>% 
    replace_id_keys_in_data(., group_keys, "between") %>% 
    replace_id_keys_in_data(., within_keys, "within") %>% 
    replace_id_keys_in_data(., condition_keys, "condition")
  
  # Replace between, within in condition_table
  entry_data$condition_table = entry_data$condition_table %>% 
    replace_id_keys_in_data(., group_keys, "between") %>% 
    replace_id_keys_in_data(., within_keys, "within")
  
  # Add all tables
  dataset_table = entry_data$dataset_table
  add_table(conn, dataset_table, "dataset_table")
  
  within = entry_data$within_table
  add_table(conn, within, "within_table")
  
  condition = entry_data$condition_table
  add_table(conn, condition, "condition_table")
  
  observation = as.data.frame(entry_data$observation_table)
  
  add_table(conn, observation, "observation_table")
}