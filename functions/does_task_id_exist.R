# Checks to see if a given task_name exists in task-lookup table
# Returns TRUE/FALSE if task ID exsits
does_task_id_exist <- function(conn, object){
  name = get_task_names(object)
  task_table = tbl(conn, "task")
  task_id = task_table %>% 
    filter(task_name == name) %>% 
    pull(task_id)
  length = length(task_id)
  if (length == 0){
    return(FALSE)
    stop(paste("Task Name:", name, "not found in database"))
  } else if (length == 1){
    return(TRUE)
  } else {
    stop("This task name was already found twice in the database. Please investigate what went wrong here.")
  }
}