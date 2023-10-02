#' Check Validity of Elements on Study Level
#' 
#' This function checks whether the elements of a given object on the study level contain
#' "study_info," "group_info," and at least one "data[NUMBER]" element (e.g., "data1"). 
#' It also ensures that there are no duplicated names in the object.
#' 
#' @param object An object to be checked for the validity of its elements at the study level.
#' 
#' @details This function verifies that the elements of the input object on the study level meet certain requirements:
#' 1. It must contain a "study_info" element.
#' 2. It must contain a "group_info" element.
#' 3. It should include at least one "data[NUMBER]" element, such as "data1."
#' 4. There should be no duplicated names within the object.
#'
#' If any of these requirements are not met, the function raises an error.
#'
#' @return This function does not return a value; it performs error checks and raises an error if conditions are not met.
#' @export
which_element_wrong_study <- function(object){
  regex_matches_data_names = get_appropriate_regex_pattern("data_names")
  
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
