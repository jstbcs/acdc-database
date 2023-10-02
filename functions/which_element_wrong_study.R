# This function checks whether element on study level contains:
# 1. "study_info"
# 2. "group_info"
# 3. at least one data[NUMBER] element, e.g. data1
# 4. no duplicated names
which_element_wrong_study <- function(object){
  stop_if_not_study_level(object)
  names = names(object)
  length = length(object)
  if (!"study_info" %in% names)
  {
    stop("Object needs to have a 'study_info' element")
  } 
  if (!"group_info" %in% names)
  {
    stop("Object needs to have a 'group_info' element")
  }
  # This if checks if all names are valid
  if (!all(stringr::str_detect(names,
                               paste(
                                 regex_matches_data_names,
                                 "study_info",
                                 "group_info",
                                 sep = "|")
  )
  )
  )
  {
    error_name = names[which(stringr::str_detect(names,
                                                 paste(
                                                   regex_matches_data_names, 
                                                   "study_info",
                                                   "group_info",
                                                   sep = "|"
                                                 ),
                                                 negate = TRUE)
    )]
    error_message = paste(
      "Element-name:",
      error_name,
      "invalid.",
      "Elements can only be named 'study_info', 'group_info' or 'data_[NUMBER]"
    )
    stop(error_message)
  }
  
  # This if checks if there is at least one data entry
  if (!any(stringr::str_detect(names, regex_matches_data_names)))
  { 
    error_name = names
    error_message = paste(
      "Object must contain at least one data element named 'data_[NUMBER].",
      "Current names:",
      error_name
    )
    stop(error_message) 
  }
  
  stop_if_names_duplicated(names)
}
