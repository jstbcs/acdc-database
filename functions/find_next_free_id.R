#' Find the Next Available ID in a Database Table
#'
#' This function retrieves the next available ID in a specified database table.
# It is useful for generating unique IDs for new entries in the table.
# 
#' @param conn A database connection object.
#' @param type A character string representing the name of the table.
#'
#' @return The next available ID for the given table.
#'
#' @details This function queries the database to find the maximum ID currently
#' in use for the specified table. It then increments this maximum ID to provide
#' the next available unique ID.
# 
#' @export
find_next_free_id <- function(conn, type){
  column = return_id_name_from_table(type)
  
  sql_query = paste0(
    "SELECT max(",
    column,
    ") FROM ",
    type
  )
  
  max = DBI::dbGetQuery(conn, sql_query)[1, 1]
  
  if (is.na(max) | max <= 0){
    max = 0
  }
  next_free = max + 1
  return(next_free)
}