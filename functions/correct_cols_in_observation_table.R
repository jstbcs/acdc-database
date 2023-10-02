# This function checks whether all required columns of the data table are provided 
correct_cols_in_observation_table <- function(observation_table){
  colnames = colnames(observation_table)
  
  # check if object is data frame 
  if(!is.data.frame(observation_table)){
    stop("Data table must be a dataframe")
  }
  
  # stop if required column names are not present
  confirm_object_names(observation_table, entry_list_info$observation_table)
}