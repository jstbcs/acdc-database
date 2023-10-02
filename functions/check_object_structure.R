# Check object structure, parent function
check_object_structure <- function(object){
  stop_if_not_top_level(object)
  if (inherits(object, "list") == FALSE)
  {
    stop("Object not a list")
  } 
  check_publication_level_structure(object)
  
  # TODO: for loop over all studies here
  check_study_level_structure(object)
  
  # For loop for all data structures in all studies here
  check_data_structure(object)
}