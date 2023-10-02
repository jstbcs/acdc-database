#' Append Study to Database
#'
#' This function appends study-related data to a specific database, ensuring that it gets the necessary publication and group identifiers.
#'
#' @param conn The database connection object.
#' @param object A list containing the study-related data to append, typically structured as a nested list with study-level and data-level components.
#' @param details A list of details containing the study-level identifiers, either publication codes or study IDs.
#'
#' @return This function appends the provided study-related data to the specified database.
#'
#' @export
append_db_study <- function(conn, object, details) {
  # Regex patterns used later
  regex_matches_data_names = get_appropriate_regex_pattern("data_names")
  regex_matches_publication_code = get_appropriate_regex_pattern("publication_code")
    
  # Ensure the structure of the study data
  check_study_level_structure(object)
  
  # Loop over data names and their components
  data_names = which_elements_match(names(object), regex_matches_data_names)
  for (data in data_names) {
    check_data_level_structure(object[[data]])
  }
  
  # Determine the publication ID based on details
  if (stringr::str_detect(details, regex_matches_publication_code) == TRUE) {
    pub_id = return_publication_id(conn, details)
  } else {
    pub_id = details
  }
  
  # Add the study data to the database
  add_study(conn, object, pub_id)
}
