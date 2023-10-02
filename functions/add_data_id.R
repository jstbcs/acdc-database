add_data_id <- function(conn, object){
  stop_if_not_data_level(object)
  data_id = return_next_free_data_id(conn) - 1 # because we want the id of the last added overview
  object$data$data_id = data_id
  return(object)
}