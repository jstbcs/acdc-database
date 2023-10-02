#' Prepare Object IDs
#'
#' This function prepares a nested object with database IDs for the specified object,
#' such as a publication, study, or data entry, by adding the appropriate IDs to each level.
# 
#' @param conn A database connection.
#' @param object The nested object for which you want to prepare database IDs.
#' 
#' @return A nested object with database IDs added for each level (publication, study, and data).
# 
#' @export
prepare_object_ids <- function(conn, object){
  stop_if_not_top_level(object)
  n_data = length(object) - 1
  names = paste0("data_", 1:n_data)
  study_add = add_study_id_overview(conn, object)
  for (i in names)
  {
    study_add[[i]] = add_task_id_overview(conn, study_add[[i]])
  }
  return(study_add)
}