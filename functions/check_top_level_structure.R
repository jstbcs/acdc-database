# This function checks elements on top level
check_top_level_structure <- function(object){
  stop_if_not_top_level(object)
  stop_if_names_duplicated(object)
  if (!all(str_detect(names, regex_matches_publication_names)))
  {
    "Names can only be publication[number]"
  }
}