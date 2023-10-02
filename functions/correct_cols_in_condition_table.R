#' Check if all required columns in the condition table are present.
#'
#' This function verifies whether all the mandatory columns required in the condition table are present.
# It ensures that the 'condition_table' argument is a data frame and then compares the column names with the required column names from the database.
# If any of the mandatory columns are missing, the function raises an error.
# 
#' @param condition_table The condition table as a data frame.
# 
#' @details The 'condition_table' is a critical component of the data structure. This function ensures that the required columns in the condition table are provided.
# It validates whether the input is a data frame and then confirms that all the mandatory columns are present. If any required columns are missing,
# it raises an error, preventing the data from being inserted into the database.
#'
#' @export
correct_cols_in_condition_table <- function(condition_table){
  entry_list_info = get_database_info()
  
  colnames = colnames(condition_table)
  
  # check if object is data frame
  if(!is.data.frame(condition_table)){
    stop("condition_table must be a dataframe")
  }
  
  # stop if required column names are not present
  confirm_object_names(condition_table, entry_list_info$condition_table)
}