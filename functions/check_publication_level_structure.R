# This function checks elements on publication level
check_publication_level_structure <- function(object, entry_list_info){
  names = names(object)
  stop_if_not_publication_level(object)
  stop_if_names_duplicated(names)
  if (!all(stringr::str_detect(names, regex_matches_study_names) | stringr::str_detect(names, "publication_table")))
  {
    "Names can only be study[NUMBER] or publication_table."
  }
  check_publication_table(object$publication_table, entry_list_info)
}
