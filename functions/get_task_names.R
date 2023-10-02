#' Get Task Names
#'
#' This function retrieves a vector of task names from the input object.
# 
#' @param object An object containing task information.
# 
#' @return A character vector containing task names.
# 
#' @export
get_task_names <- function(object){
  stop_if_not_data_level(object)
  task_names = c()
  for (i in 2:length(object)){
    task_names[i - 1] = object$overview$task_name
  }
  return(task_names)
}