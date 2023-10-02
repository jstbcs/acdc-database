# add the returned task id and study id to overview column
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