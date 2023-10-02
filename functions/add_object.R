# Publication
add_object <- function(conn, object){
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