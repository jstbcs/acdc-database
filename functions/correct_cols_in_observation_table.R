#' Check if required columns in the observation table are provided.
#'
#' This function verifies the presence of required columns in the observation table.
# It ensures that the 'observation_table' argument is a data frame and then compares the column names with the required column names from the database.
# If any required columns are missing, an error is generated.
# 
#' @param observation_table The observation table as a data frame.
# 
#' @details The 'observation_table' is a component of the data structure. This function checks if all the required columns in the observation table are provided. 
# It validates whether the input is a data frame and confirms the presence of required columns.
# 
#' @export
correct_cols_in_observation_table <- function(observation_table){
  entry_list_info = get_database_info()
  colnames = colnames(observation_table)
  
  # check if object is data frame 
  if(!is.data.frame(observation_table)){
    stop("Data table must be a dataframe")
  }
  
  # stop if required column names are not present
  confirm_object_names(observation_table, entry_list_info$observation_table)
}