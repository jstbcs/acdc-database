#' Check if Object is at the Publication Level
#' 
#' This function checks if the given object contains essential columns typically found 
#' in a publication-level object. It ensures that the required columns related to study names
#' are present in the object. If these columns are not found, it raises an error, indicating 
#' that the object is not at the publication level.
#' 
#' @param object An object to be checked for publication-level columns.
#' 
#' @details This function verifies that the input object contains essential columns 
#' typically found at the publication level. Specifically, it checks if the columns related to
#' study names are present. If these columns are not found, it suggests that the object is not 
#' at the publication level.
#' @export
stop_if_not_publication_level <- function(object){
  regex_matches_study_names = get_appropriate_regex_pattern("study_names")
  
  if (!all(str_detect(colnames(object), regex_matches_study_names))){
    stop("This function takes a publication-level object")
  }
}