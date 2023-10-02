#' Check the structure of the study-level object.
#'
#' This function validates the structure of the provided study-level object, ensuring that it has the correct components and ordering. It also verifies the structure of the study information and group information.
#'
#' @param object A list representing the study-level object to be validated.
#'
#' @details This function checks the structure of the provided study-level object, including the presence and order of its components.
#'
#' @return This function does not return a value but raises errors if the study-level object structure does not match the expected criteria.
#'
#' @export
check_study_level_structure <- function(object){
  stop_if_not_study_level(object)
  names = names(object)
  length = length(object)
  # This speed up processing if all elements are in correct order
  if (!all(names == c("study_table", "between_table", paste0("data", 1:(length-2)))))
  {
    which_element_wrong_study(object)
  }
  
  # Check the study info element
  check_study_table_structure(object$study_table)
  
  # Check group
  check_between_table_structure(object$between_table)
}