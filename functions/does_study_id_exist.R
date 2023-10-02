# Checks to see if a given study_code exists in study-lookup table
does_study_id_exist <- function(conn, object){
  code = get_study_code(object)
  study_table = tbl(conn, "study")
  study_id = study_table %>% 
    filter(study_code == code) %>% 
    pull(study_id)
  length = length(study_id)
  if (length == 0){
    return(FALSE)
  } else if (length == 1){
    return(TRUE)
  } else {
    stop("This study code was already found twice in the database. Please investigate what went wrong here.")
  }
}