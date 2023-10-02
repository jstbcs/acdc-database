#' Check if optional columns in the dataset table are provided.
#'
#' This function verifies the presence of optional columns in the dataset table.
# It ensures that the 'dataset_table' argument is a data frame and then compares the column names with the optional column names from the database.
# If any optional columns are missing, a warning is issued to prompt user input on whether to continue with missing columns.
# 
#' @param dataset_table The dataset table as a data frame.
# 
#' @details The 'dataset_table' is a component of the data structure. This function checks if optional columns in the dataset table are provided.
# It validates whether the input is a data frame and confirms the presence of optional columns. If any optional columns are missing, a warning is issued, and the user is asked whether to proceed with the missing columns.
#'
#' @export
correct_cols_in_dataset_table <- function(dataset_table){
  entry_list_info = get_database_info()
  colnames = colnames(dataset_table)
  
  # check if object is data frame 
  if(!is.data.frame(dataset_table)){
    stop("dataset_table must be a dataframe")
  }
  # check if object contains more than 1 entry
  if(nrow(dataset_table) > 1){
    stop("The dataset_table data frame can only contain 1 row")
  }
  
  confirm_object_names(dataset_table, entry_list_info$dataset_table)
}