#' Add Object to Database
#'
#' This function adds an object to the database and inspects the object's structure. It checks for publication codes to ensure they are unique and adds them to the database. It then processes study names within the publication and adds them to the respective publication in the database.
#'
#' @param conn The connection object or database connection string.
#' @param object The object to be added to the database.
#'
#' @export
add_object <- function(conn, object){
  # Regex patterns used later
  regex_matches_publication_names = get_appropriate_regex_pattern("publication_names")
  regex_matches_study_names = get_appropriate_regex_pattern("study_names")
  
  pub_names = which_elements_match(names(object), regex_matches_publication_names)
  
  for (publication in pub_names){
    pub_code = object[[publication]]$publication_table$publication_code
    if (does_publication_code_exist(conn, pub_code) == TRUE){
      stop("This publication code already exists. Please use the append_db function to add to a specific publication.")
    }
    
    # Find and add pub id
    pub_id = find_next_free_id(conn, "publication_table")
    
    pub_info = object[[publication]]$publication_table
    
    # Then add that to db
    add_table(conn, pub_info, "publication_table")
    
    # Now for study names
    study_names = which_elements_match(names(object[[publication]]), regex_matches_study_names)
    
    for (study in study_names){
      add_study(
        conn,
        object[[publication]][[study]],
        pub_id
      )
    }
  }
}