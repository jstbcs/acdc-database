# This function checks whether an element on the data-level consists of one task element,
# one overview element, one data element, and one within element 

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
