#' Check the structure of elements at the publication level.
#'
#' This function validates the structure of elements within the provided publication-level object. It performs checks to ensure the object adheres to the expected publication and study hierarchy.
#'
#' @param object A publication-level object to be validated.
#'
#' @details This function verifies the structure of the provided publication-level object. It checks for specific naming conventions, validates the publication table, and ensures that the object follows the expected publication and study hierarchy.
#'
#' @return This function does not return a value. It is used solely for structural validation and raises errors if the structure of the provided publication-level object does not meet the expected criteria.
#'
#' @seealso \code{\link{check_publication_table}}, \code{\link{get_database_info}}
#'
#' @export
check_publication_level_structure <- function(object){
  regex_matches_study_names = get_appropriate_regex_pattern("study_names")
  entry_list_info = get_database_info()
  
  names = names(object)
  stop_if_not_publication_level(object)
  stop_if_names_duplicated(names)
  if (!all(stringr::str_detect(names, regex_matches_study_names) | stringr::str_detect(names, "publication_table")))
  {
    "Names can only be study[NUMBER] or publication_table."
  }
  check_publication_table(object$publication_table, entry_list_info)
}
