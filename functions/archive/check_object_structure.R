#' Check the structure of an object.
#'
#' This function validates the overall structure of the provided object, ensuring it adheres to the expected format.
#'
#' @param object An object to be validated.
#'
#' @details This function checks the provided object to verify that it meets the expected structure and content requirements. It ensures the object is a list, followed by specific checks for its structure at various levels (publication, study, and data).
#'
#' @return This function does not return a value. It raises an error if the structure of the provided object does not meet the expected criteria.
#'
#' @seealso \code{\link{check_publication_level_structure}}, \code{\link{check_study_level_structure}}, \code{\link{check_data_structure}}
#'
#' @export
#' 
check_object_structure <- function(object){
  stop_if_not_top_level(object)
  if (inherits(object, "list") == FALSE)
  {
    stop("Object not a list")
  } 
  check_publication_level_structure(object)
  
  #for loop over all studies here
  check_study_level_structure(object)
  
  # For loop for all data structures in all studies here
  check_data_structure(object)
}