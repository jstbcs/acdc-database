#' Add Data Table to Database
#'
#' This function adds a data table to the database. It only inserts columns that are relevant to the specified table type.
#'
#' @param conn The database connection object or connection string.
#' @param table The data table to be inserted into the database.
#' @param type The type or name of the table in the database (e.g., "study_table" or "between_table").
#'
#' @return NULL. The function inserts the specified data table into the database.
#'
#' @export
add_table <- function(conn, table, type){
  # conn is connection
  # Table is data
  # Type is the table name in db
  # Depending on the type, read out the column names from name list
  
  # Only read possible cols
  table_info_db = get_database_info()
  
  possible_cols = table_info_db[[type]]
  
  insert = table[which_elements_exist(possible_cols$column, table)]
  
  dbWriteTable(
    conn = conn,
    name = type,
    value = insert,
    append = TRUE
  )
  
  print(paste("Added to", type, "table"))
}