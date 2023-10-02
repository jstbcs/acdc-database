#' Add Data ID to Object
#'
#' This function adds a data ID to an R object to associate it with a specific data record in a database.
#'
#' @param conn The connection object or database connection string.
#' @param object The R object to which the data ID will be added.
#'
#' @return The updated R object with the data ID added.
#' @export
add_data_id <- function(conn, object){
  stop_if_not_data_level(object)
  data_id = return_next_free_data_id(conn) - 1 # because we want the id of the last added overview
  object$data$data_id = data_id
  return(object)
}