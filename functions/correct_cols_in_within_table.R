# This function checks whether all required columns of the within_table table are provided 
correct_cols_in_within_table <- function(within_table){
  colnames = colnames(within_table)
  
  # check if object is data frame 
  if(!is.data.frame(within_table)){
    stop("within_table must be a dataframe")
  }
  
  # stop if required column names are not present
  confirm_object_names(within_table, entry_list_info$within_table)
}