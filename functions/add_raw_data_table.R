add_raw_data_table <- function(conn, object){
  stop_if_not_data_level(object)
  if (do_elements_exist("data_id", object$data) == FALSE)
  {
    stop("Variable 'data_id' needs to be present in data")
  }
  data_columns = c("data_id", "rt", "accuracy", "congruency",
                   "subject", "block", "trial", "age_group")
  data_inject = object$data[which_elements_exist(data_columns, object$data)]
  dbWriteTable(
    conn = conn,
    name = "data",
    value = data_inject,
    append = TRUE
  )
}