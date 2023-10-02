# Returns vector of task names found in inject object
get_task_names <- function(object){
  stop_if_not_data_level(object)
  task_names = c()
  for (i in 2:length(object)){
    task_names[i - 1] = object$overview$task_name
  }
  return(task_names)
}