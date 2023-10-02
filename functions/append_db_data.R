#' Append Data to Database
#'
#' This function appends data to a specific database, ensuring that it gets the necessary study-level identifiers (publication, group, and task).
#'
#' @param conn The database connection object.
#' @param object A list or data frame containing the data to append.
#' @param details A list of details containing the study-level identifiers (publication, group, and task).
#' @param keys A list of keys for within and condition variables.
#'
#' @return This function appends the provided data to the specified database.
#'
#' @export
append_db_data <- function(conn, object, details, keys) {
  # Ensure the structure of the data
  check_data_level_structure(object)
  
  # Extract the study ID from the details
  study_id = details
  
  # Add the data to the database with study-level identifiers and keys
  add_data(conn, object, study_id, keys)
}
