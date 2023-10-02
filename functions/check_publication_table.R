#' Check the structure of the publication table.
#'
#' This function validates the structure of the provided publication table. It ensures that the column names of the publication table match the expected column names defined in the database information.
#'
#' @param pub_table A data frame representing the publication table to be validated.
#'
#' @details This function checks the structure of the provided publication table, including the presence of expected columns as defined in the database information.
#'
#' @return This function does not return a value but raises errors if the publication table structure does not match the expected criteria.
#'
#' @seealso \code{\link{get_database_info}}, \code{\link{confirm_object_names}}
#'
#' @export
check_publication_table <- function(pub_table){
  entry_list_info = get_database_info()
  
  names = names(pub_table)
  stop_if_names_duplicated(names)
  
  confirm_object_names(pub_table, entry_list_info$publication_table)
}