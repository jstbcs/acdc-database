append_db_study <- function(conn, object, details){
  # Need to return the id depending on the details
  # Details should be a list of publication_code or study_id
  check_study_level_structure(object)
  
  # now loop over data names
  data_names = which_elements_match(
    names(object),
    regex_matches_data_names
  )
  
  # loop over each data element within each study element
  for(data in data_names) {
    check_data_level_structure(object[[data]])
  }
  
  # If a publication code is given, return the id
  if (stringr::str_detect(details, regex_matches_publication_code) == TRUE){
    pub_id = return_publication_id(conn, details)
  } else { # else the id is just the details
    pub_id = details
  }
  
  add_study(conn, object, pub_id)
}