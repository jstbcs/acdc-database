#' Create SQL Insertion Query
#'
#' This function generates an SQL insertion query to insert data into a specified table.
#'
#' @param table A character string representing the name of the table where the data will be inserted.
#' @param columns A character vector containing the column names into which data will be inserted.
#'
#' @return An SQL insertion query as a character string.
#' @export
write_sql_insert <- function(table, columns){
  warning("Using probably outdated function: 'write_sql_insert()'")
  n_cols = length(columns)
  insert = paste0(
    "INSERT INTO ",
    table,
    " (",
    paste(columns, collapse = ", "),
    ") ",
    "VALUES (",
    paste(rep("?", n_cols), collapse = ", "),
    ");"
  )
  return(insert)
}