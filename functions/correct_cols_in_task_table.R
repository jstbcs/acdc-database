# This function checks whether all required columns of the task_table table are provided
correct_cols_in_task_table <- function(task_table){
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