# Finds the task id in table of that code
return_task_id <- function(conn, object){
  if (does_task_id_exist(conn, object) == FALSE)
  {
    stop("This task was not found in our task-database. Please considers adding it")
  }
  find_task_id(conn, object)
}