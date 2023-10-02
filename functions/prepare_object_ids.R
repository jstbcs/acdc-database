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