#' Check if Object is at the Data Level
#' 
#' This function checks if the given object contains essential data-level tables, including
#' task, dataset, within, condition, and observation tables. It raises an error if any of 
#' these tables are missing, indicating that the object does not meet the data-level 
#' requirements.
#' 
#' @param object An object to be checked for data-level tables.
#' 
#' @details This function ensures that the input object contains the necessary tables 
#' typically found at the data level: task_table, dataset_table, within_table, 
#' condition_table, and observation_table. If any of these tables are missing, it indicates
#' that the object is not at the data level.
#' @export
stop_if_not_data_level <- function(object){
  if (do_elements_exist(c("task_table", "dataset_table", "within_table", "condition_table", "observation_table"), object) == FALSE)
  {
    stop("This function takes a data-level object")
  }
}