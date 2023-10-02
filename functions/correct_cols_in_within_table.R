#' Check if required columns in the within table are provided.
#'
#' This function verifies the presence of required columns in the within table.
# It ensures that the 'within_table' argument is a data frame and then compares the column names with the required column names from the database.
# If any required columns are missing, an error is generated.
# 
#' @param within_table The within table as a data frame.
# 
#' @details The 'within_table' is a component of the data structure. This function checks if all the required columns in the within table are provided. 
# It validates whether the input is a data frame, confirms the presence of required columns, and raises an error if any are missing.
# 
#' @export
correct_cols_in_within_table <- function(within_table){
  entry_list_info = get_database_info()
  colnames = colnames(within_table)
  
  # check if object is data frame 
  if(!is.data.frame(within_table)){
    stop("within_table must be a dataframe")
  }
  
  # stop if required column names are not present
  confirm_object_names(within_table, entry_list_info$within_table)
}