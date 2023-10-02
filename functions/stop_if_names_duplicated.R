#' Check for Duplicated Names in a Vector and Stop Execution
#' 
#' This function checks if there are duplicated names in a given vector and raises 
#' an error if duplicates are found. It is useful for ensuring that elements in an 
#' object have unique names.
#' 
#' @param names A character vector containing element names to be checked for duplication.
#' 
#' @details If any duplicate names are found in the 'names' vector, this function raises
#' an error with a message indicating which element name is not unique. Elements in the 
#' object must be uniquely named.
#' 
#' @examples
#' # Check for duplicated names in a vector
#' element_names <- c("name1", "name2", "name1")
#' stop_if_names_duplicated(element_names)
# 
#' @export
stop_if_names_duplicated <- function(names){
  if (any(duplicated(names)))
  {
    error_name = names[which(duplicated(names))]
    error_message = paste(
      "Element-name:",
      error_name,
      "is not unique.",
      "Elements in object must be uniquely named."
    )
    stop(error_message)
  }
}