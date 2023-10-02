# Finds the study id in table of that code
return_study_id <- function(conn, object){
  stop_if_not_top_level(object)
  if (does_study_id_exist(conn, object) == FALSE) 
  {
    continue_after_warning("This study code does not currently exist. Want to add it to the study-table?")
    add_study_info(conn, object)
    find_study_id(conn, object)
  } else 
  {
    find_study_id(conn, object)
  }
}