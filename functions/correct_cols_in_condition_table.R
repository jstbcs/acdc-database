# This function checks whether all required columns of condition_table are provided 
correct_cols_in_condition_table <- function(condition_table){
  colnames = colnames(condition_table)
  
  # check if object is data frame
  if(!is.data.frame(condition_table)){
    stop("condition_table must be a dataframe")
  }
  
  # stop if required column names are not present
  confirm_object_names(condition_table, entry_list_info$condition_table)
}