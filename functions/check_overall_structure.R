#' Check the overall structure of an entry list.
#'
#' This function validates the overall structure of the provided entry list, ensuring it adheres to the expected format. It checks the nested hierarchy of publications, studies, and data elements.
#'
#' @param entry_list An entry list to be validated.
#'
#' @details This function verifies the structure of the provided entry list to ensure it follows the expected hierarchy, starting from publications and proceeding to studies and data elements. It performs structure checks at each level and raises errors if the structure does not meet the expected criteria.
#'
#' @return This function does not return a value. It is used solely for structural validation and raises errors if the structure of the provided entry list does not meet the expected criteria.
#'
#' @seealso \code{\link{check_publication_level_structure}}, \code{\link{check_study_level_structure}}, \code{\link{check_data_level_structure}}
#'
#' @export
check_overall_structure <- function(entry_list){
  regex_matches_publication_names = get_appropriate_regex_pattern("publication_names")
  regex_matches_study_names = get_appropriate_regex_pattern("study_names")
  regex_matches_data_names = get_appropriate_regex_pattern("data_names")
  # Its possible that multiple publications are in one entry_list
  pub_names = which_elements_match(names(entry_list), regex_matches_publication_names)
  
  # If length pub_names is 0, wrong object passed
  if (length(pub_names) == 0){
    stop("Object has no valid publication elements")
  }
  
  for (publication in pub_names){
    # check publication level 
    check_publication_level_structure(entry_list[[publication]])
    
    # Now get names of all the study elements in that publication
    study_names = which_elements_match(names(entry_list[[publication]]), regex_matches_study_names)
    # loop over each study element and test structure on study level
    for(study in study_names) {
      check_study_level_structure(entry_list[[publication]][[study]])
      
      # now loop over data names
      data_names = which_elements_match(
        names(entry_list[[publication]][[study]]),
        regex_matches_data_names
      )
      
      # loop over each data element within each study element
      for(data in data_names) {
        check_data_level_structure(entry_list[[publication]][[study]][[data]])
      }
    }
  }
}