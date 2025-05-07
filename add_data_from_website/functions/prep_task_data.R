prep_task_data <- function(task_data){
  task_data = clean_char_columns(task_data)
  return(task_data)
}
