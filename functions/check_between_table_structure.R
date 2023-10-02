#' Check the structure of the between_table in a study object.
#'
#' This function ensures the proper structure and content of the between_table.
#'
#' @param between_table A data frame representing the 'between_table' in a study object.
# 
#' @return This function does not return a value. It raises an error if the 'between_table' does not meet the expected structure and content.
# 
#' @details This function checks the provided 'between_table' to confirm that it meets the expected
#' structure and content based on the database information.
# 
#' If 'between_table' contains more than one row, it checks if the 'group_description' column is present.
# 
#' If the 'group_description' column is present, the function ensures that it does not contain empty or missing entries.
# 
#' @seealso \code{\link{get_database_info}}, \code{\link{confirm_object_names}}
# 
#' @export

check_between_table_structure <- function(between_table) {
  # Get the database column information.
  entry_list_info = get_database_info()
  
  # Get the column names of the between_table.
  names = names(between_table)
  
  # Check if the between_table is a dataframe.
  if (is.data.frame(between_table) == FALSE) {
    stop("Between-Table is not a dataframe")
  }
  
  # If there is more than one row in the between_table, perform additional checks.
  if (nrow(between_table) > 1) {
    # Check if 'group_description' is present in the column names.
    if (!"group_description" %in% names) {
      stop("Object needs to have a 'group_description' element")
    }
    
    # Check for empty or missing 'group_description' entries.
    if (
      any(is.na(between_table$group_description)) ||
      any(is.null(between_table$group_description)) ||
      any(between_table$group_description == "")
    ) {
      stop("group_description can not be empty")
    }
  }
  
  # Confirm that the column names in between_table match the expected names from the database info.
  confirm_object_names(between_table, entry_list_info$between_table)
}
