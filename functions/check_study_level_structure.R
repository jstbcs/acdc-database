# This function checks the entries on study level to see if they have proper structure
# Former: check_object_elements
check_study_level_structure <- function(object){
  stop_if_not_study_level(object)
  names = names(object)
  length = length(object)
  # This speed up processing if all elements are in correct order
  if (names != c("study_info", "group_info", paste0("data", 1:(length-2))))
  {
    which_element_wrong_study(object)
  }
  
  # Check the study info element
  check_study_info_structure(object$study_info)
  
  # Check group
  check_group_info_structure(object$group_info)
}