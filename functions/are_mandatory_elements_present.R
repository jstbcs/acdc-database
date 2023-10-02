#' Check Mandatory Columns in Data Object
#'
#' This function checks if a list object contains mandatory columns specified by their names.
#'
#' @param object A list object to be checked for the presence of mandatory columns.
#' @param mandatory_colnames A character vector containing the names of mandatory columns that must be present in the list object.
#'
#' @return This function verifies if all specified mandatory columns are present in the object. If any column is missing, it raises an error with a message indicating the missing column.
#'
#' @export
are_mandatory_elements_present <- function(object, mandatory_colnames){
  names = names(object)
  for (name in mandatory_colnames){
    if (!name %in% names){
      msg = paste("The column:", name, "must be specified in the data.")
      stop(msg)
    }
  }
}