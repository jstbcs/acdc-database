#' Add Task ID to Overview Data
#'
#' This function adds a task ID to the overview data, which is required to link the overview data to a specific task.
#'
#' @param conn The database connection object.
#' @param object A list or data frame containing the overview data.
#'
#' @return A modified version of the input `object` with the task ID added to the overview data.
#'
#' @export
add_task_id_overview <- function(conn, object) {
  # Ensure the object structure is appropriate
  stop_if_not_data_level(object)
  
  # Get the task ID from the database
  task_id = return_task_id(conn, object)
  
  # Add the task ID to the overview data
  object$overview$task_id = task_id
  
  # Return the modified object
  return(object)
}