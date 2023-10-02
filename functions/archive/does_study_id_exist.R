# WARNING: This function is broken. But probably not used?

# Checks to see if a given study_code exists in study-lookup table
does_study_id_exist <- function(conn, object){
  code = get_study_code(object)
  sql_query = paste0(
    "SELECT study_id FROM study_table WHERE publication_code = ",
    code
  )
  study_id = DBI::dbGetQuery(conn, sql_query)
  length = length(study_id)
  if (length == 0){
    return(FALSE)
  } else if (length == 1){
    return(TRUE)
  } else {
    stop("This study code was already found twice in the database. Please investigate what went wrong here.")
  }
}