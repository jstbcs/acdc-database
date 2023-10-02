#' Add Study Information to Database
#'
#' This function adds information about studies, such as study codes, authors, conducted dates, added dates, country, and contact details to the database.
#'
#' @param conn The database connection object or connection string.
#' @param object The main object containing study information.
#'
#' @return NULL. The function inserts the study information into the database.
#'
#' @export
add_study_info <- function(conn, object){ # add more study variables here
  stop_if_not_top_level(object)
  study_columns = c("study_code", "authors", "conducted", "added", "country", "contact")
  insert_study_info = object$study[which_elements_exist(study_columns, object$study)]
  dbWriteTable(
    conn = conn,
    name = "study",
    value = insert_study_info,
    append = TRUE
  )
}