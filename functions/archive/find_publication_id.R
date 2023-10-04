#' Find the Publication ID for a Given Publication Code
#'
#' This function retrieves the publication ID associated with a given publication code.
#'
#' @param conn A database connection object.
#' @param code A character string representing the publication code.
#'
#' @return An integer representing the publication ID or NULL if not found.
#'
#' @details This function queries the database to find the publication ID associated with the provided publication code.
#' If no match is found, it returns NULL.
# 
#' @export
find_publication_id <- function(conn, code){
  sql_query = paste0(
    "SELECT publication_id FROM publication_table WHERE publication_code = ",
    code
  )
  pub_id = DBI::dbGetQuery(conn, sql_query)
  return(pub_id)
}
