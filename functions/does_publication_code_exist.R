#' Check If a Publication Code Exists in the Database
#'
#' This function checks if a given publication code exists in the database.
#'
#' @param conn A database connection object.
#' @param code A character string representing the publication code.
#'
#' @return TRUE if the publication code exists in the database; FALSE if not found.
#'
#' @details This function queries the database to determine whether the provided publication code is already present.
# If the code is found, it returns TRUE; if not found, it returns FALSE.
# If the code is found more than once, indicating an issue, it throws an error.
# 
#' @export
does_publication_code_exist <- function(conn, code){
  sql_query = paste0(
    "SELECT publication_id FROM publication_table WHERE publication_code = ",
    code
  )
  pub_id = DBI::dbGetQuery(conn, sql_query)
  length = length(pub_id)
  if (length == 0){
    return(FALSE)
  } else if (length == 1){
    return(TRUE)
  } else {
    stop("This publication code was already found twice in the database. Please investigate what went wrong here.")
  }
}