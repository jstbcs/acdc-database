#' Append Data to Database
#'
#' This function appends data to a specific database, ensuring that it gets the necessary study-level identifiers (publication, group, and task).
#'
#' @param conn The database connection object.
#' @param object A list or data frame containing the data to append.
#' @param study_id A string representing the study-level identifier to associate with the data being appended.
#'
#' @return This function appends the provided data to the specified database.
#'
#' @export
append_db_data <- function(conn, object, study_id) {
  # Ensure the structure of the data
  check_data_level_structure(object)
  
  # Add the data to the database with study-level identifiers
  add_data(conn, object, study_id)
}
