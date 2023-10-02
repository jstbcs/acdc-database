# this function takes nested list element 'entry_list' and checks entire structure

check_overall_structure <- function(entry_list){
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