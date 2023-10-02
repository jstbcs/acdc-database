#' Check if Object is at the Study Level
#' 
#' This function checks if the given object contains essential elements typically found 
#' in a study-level object. It ensures that the required elements related to study details
#' are present in the object. If these elements are not found, it raises an error, indicating 
#' that the object is not at the study level.
#' 
#' @param object An object to be checked for study-level elements.
#' 
#' @details This function verifies that the input object contains essential elements 
#' typically found at the study level. Specifically, it checks if the elements related to
#' study details are present. If these elements are not found, it suggests that the object is not 
#' at the study level.
#' @export
stop_if_not_study_level <- function(object){
  if(do_elements_exist(c("study_table", "between_table", "data1"), object) == FALSE)
  {
    stop("This function takes a study-level object")
  }
}