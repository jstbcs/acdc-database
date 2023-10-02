#' Add Study to Database
#'
#' This function adds study-related information, including study details, between-group information, and associated datasets to the database.
#'
#' @param conn The database connection object or connection string.
#' @param study_add The object containing study-related information.
#' @param pub_id The publication ID to which the study belongs.
#'
#' @return NULL. The function inserts the study information, between-group information, and datasets into the database.
#'
#' @export
add_study <- function(conn, study_add, pub_id){
  # Regex patterns used later
  regex_matches_data_names = get_appropriate_regex_pattern("data_names")
    
    
  study_id = find_next_free_id(conn, "study_table")
  # Add study id to study_info and group_info
  study_add$study_table$study_id = study_id
  study_add$between_table$study_id = study_id
  
  # Also add the publication id
  study_add$study_table$publication_id = pub_id
  
  # Then add the study table
  study_table = study_add$study_table
  add_table(conn, study_table, "study_table")
  
  between_id = find_next_free_id(conn, "between_table")
  
  # This adds the global group-id to the group_id table
  for (row in 1:nrow(study_add$between_table)){
    study_add$between_table$between_id[row] = between_id
    between_id = between_id + 1
  }
  
  between_keys = obtain_keys(study_add$between_table,
                             "between")
  
  # Then add the group table
  between_table = study_add$between_table
  
  add_table(conn, between_table, "between_table")
  
  # Now moving to dataset
  data_names = which_elements_match(names(study_add), regex_matches_data_names)
  
  for (data_element in data_names){
    add_data(
      conn,
      study_add[[data_element]],
      study_id,
      between_keys
    )
  }
  
}