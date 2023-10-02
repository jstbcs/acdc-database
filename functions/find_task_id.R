find_task_id <- function(conn, object){
  name = get_task_names(object)
  if (length(name) != 1)
  {
    error_message = paste0(
      "get_task_names returned more than 1 output, suggesting more than one entry in .$overview$task_name. Please go investigate"
    )
    stop(error_message)
  }
  task_table = tbl(conn, "task")
  task_id = task_table %>% 
    filter(task_name == name) %>% 
    pull(task_id)
  return(task_id)
}