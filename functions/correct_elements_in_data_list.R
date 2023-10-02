#' Verify the elements within a data list.
#'
#' This function confirms the presence and structure of specific elements within a data list.
# It checks if the data list consists of the required elements: task table, condition table, dataset table, within table, and observation table. It also detects and warns about duplicate names in the list.
# 
#' @param object The data list to verify.
# 
#' @details The 'correct_elements_in_data_list' function ensures the data list contains the mandatory elements for proper database integration. It checks for required components such as 'task_table', 'condition_table', 'dataset_table', 'within_table', and 'observation_table'. Additionally, it warns about duplicate names in the list.
# 
#' @export
correct_elements_in_data_list <- function(object){
  names = names(object)
  
  # check if duplicates
  stop_if_names_duplicated(names)
  
  # give warning if more than 4 elements in object
  if(length(names) > 5){
    warning("The study object contains more than 4 elements. Only the task, overview, data, condition and within
            element will be extracted")
  }
  # error if not all required objects are present
  names_should <- c("task_table", "condition_table", "dataset_table", "within_table", "observation_table")
  for(element in names_should){
    if(!(element %in% names)){
      stop(c(element, " element is required but missing in data_NUMBER list"))
    }
  }
}
