# Helper stopping functions that make sure object is specified at the right depth
stop_if_not_top_level <- function(object){
  if (do_elements_exist(c("publication"), object) == FALSE)
  {
    stop("This function takes the overall list as input. \nMake sure the object passed to it is on highest level and not a data-sublist object")
  }
}