stop_if_not_data_level <- function(object){
  if (do_elements_exist(c("task_table", "dataset_table", "within_table", "condition_table", "observation_table"), object) == FALSE)
  {
    stop("This function takes a data-level object")
  }
}