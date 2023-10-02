append_db_data <- function(conn, object, details, keys){
  # Data should get the study-level ids (pub, group and id)
  # but create its own within and condition ids
  check_data_level_structure(object)
  
  study_id = details
  
  add_data(conn, object, study_id, keys)
  
}