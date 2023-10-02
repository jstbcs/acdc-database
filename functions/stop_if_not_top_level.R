#' Check if Object is at the Top Level
#' 
#' This function verifies that the given object is specified at the top level, 
#' which means it should contain only the 'publication' element. If the object is 
#' not at the highest level and includes data-sublist objects, it raises an error.
#' 
#' @param object An object to be checked for top-level specification.
#' 
#' @details This function checks if the input object is located at the highest level, 
#' which should include only the 'publication' element and not data-sublist objects.
#' If the object contains data-sublist objects, it will raise an error.
#' @export
stop_if_not_top_level <- function(object){
  if (do_elements_exist(c("publication"), object) == FALSE)
  {
    stop("This function takes the overall list as input. \nMake sure the object passed to it is on highest level and not a data-sublist object")
  }
}