# This function takes a data_[i] list element and checks its entire structure, 
# including the column names of the sub-element data frames 
check_data_level_structure <- function(data_i){
  
  # check if input is a list
  if (inherits(data_i, "list") == FALSE)
  {
    stop("data_NUMBER object must be a list")
  } 
  
  # check if list contains correct elements
  correct_elements_in_data_list(data_i)
  
  # check if each element in data_i list is a df and contains required columns
  correct_cols_in_task_table(data_i$task_table)
  correct_cols_in_dataset_table(data_i$dataset_table)
  correct_cols_in_within_table(data_i$within_table)
  correct_cols_in_condition_table(data_i$condition_table)
  correct_cols_in_observation_table(data_i$observation_table)
  
  # check if number of within condition in data equals number of within_names in within_table
  correct_n_of_withinid(data_i$within_table, data_i$observation_table)
}