#' Check if required columns in the task table are provided.
#'
#' This function verifies the presence of required columns in the task table.
# It ensures that the 'task_table' argument is a data frame and then compares the column names with the required column names from the database.
# If any required columns are missing, an error is generated.
# 
#' @param task_table The task table as a data frame.
# 
#' @details The 'task_table' is a component of the data structure. This function checks if all the required columns in the task table are provided. 
# It validates whether the input is a data frame, confirms the presence of required columns, and raises an error if any are missing.
# 
#' @export
correct_cols_in_task_table <- function(task_table){
  entry_list_info = get_database_info()
  colnames = colnames(task_table)
  
  # check if object is data frame 
  if(!is.data.frame(task_table)){
    stop("task_table must be a dataframe")
  }
  # check if object contains more than 1 entry
  if(nrow(task_table) > 1){
    stop("The task_table data frame can only contain 1 row")
  }
  
  # stop if required column names are not present
  confirm_object_names(task_table, entry_list_info$task_table)
}