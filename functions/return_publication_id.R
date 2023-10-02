return_publication_id <- function(conn, code){
  # Finds the next free pub code if it doesnt already exist
  if (does_publication_code_exist(conn, code) == FALSE) 
  {
    continue_after_warning("This publication code does not currently exist. Want to add it to the study-table?")
    add_study_info(conn, object)
    find_study_id(conn, object)
  } else 
  {
    find_study_id(conn, object)
  }
}