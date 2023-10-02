# Add a study variable to database
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