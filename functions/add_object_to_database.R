#' Add Object to Database
#'
#' This function adds a top-level object, such as a study, to a database. It validates the object's structure and prepares it for insertion into the database. It iteratively adds data tables to the database and updates IDs accordingly.
#'
#' @param conn The connection object or database connection string.
#' @param object The top-level object to be added to the database.
#'
#' @export
add_object_to_database <- function(conn, object){
  stop_if_not_top_level(object)
  
  n_data = length(object) - 1
  
  data_names = paste0("data_", 1:n_data)
  
  check_object_structure(object)
  
  prep = prepare_object_ids(conn, object)
  
  for (i in data_names)
  {
    free_data_id = return_next_free_data_id(conn)
    add_overview_table(conn, prep[[i]])
    
    if (free_data_id != return_next_free_data_id(conn) - 1)
    {
      stop("Looks like there was no overview row added. Check the SQL Database")
    }
    prep[[i]] = add_data_id(conn, prep[[i]])
    add_raw_data_table(conn, prep[[i]])
  }
}