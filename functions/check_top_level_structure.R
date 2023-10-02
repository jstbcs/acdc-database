#' Check the structure of the top-level elements.
#'
#' This function verifies the structure of the provided top-level elements, ensuring that they match the expected pattern of 'publication[number]'. It also checks for duplicated names within the object.
#'
#' @param object The top-level object to be validated, typically representing multiple publications.
#'
#' @details This function validates the structure of the top-level object, ensuring the names follow the 'publication[number]' pattern and that there are no duplicated names.
#'
#' @return This function does not return a value but raises errors if the top-level object structure does not match the expected criteria.
#'
#' @export
check_top_level_structure <- function(object){
  regex_matches_publication_names = get_appropriate_regex_pattern("publication_names")
  
  stop_if_not_top_level(object)
  stop_if_names_duplicated(object)
  if (!all(str_detect(names, regex_matches_publication_names)))
  {
    "Names can only be publication[number]"
  }
}