#' Add Study ID to Overview
#'
#' This function adds the study ID to the 'overview' table within each sub-object in the main object. It helps to link each sub-object (e.g., studies) with a common study ID in the database.
#'
#' @param conn The database connection object or connection string.
#' @param object The main object containing multiple sub-objects (e.g., studies).
#'
#' @return The updated main object with study IDs added to the 'overview' table in each sub-object.
#'
#' @export
add_study_id_overview <- function(conn, object){
  stop_if_not_top_level(object)
  study_id = return_study_id(conn, object)
  names = names(object)[which(names(object) != "study")]
  for (i in names)
  {
    object[[i]]$overview$study_id = study_id
  }
  return(object)
}