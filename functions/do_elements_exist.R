#' Check if All Elements Exist in a Data Frame
#'
#' This function checks whether a vector of column names exists within a given data frame.
#' It returns TRUE if all specified elements are present in the data frame and FALSE otherwise.
#' Additionally, it issues a warning for each missing element.
#'
#' @param colnames A character vector of column names to check.
#' @param object The data frame to check for the existence of column names.
#'
#' @return Returns TRUE if all elements in colnames exist in the object data frame; otherwise, it returns FALSE.
#' @export
do_elements_exist <- function(colnames, object){
  vec = c()
  for (i in seq_along(colnames))
  {
    vec[i] = exists(colnames[i], object)
    if (!exists(colnames[i], object))
    {
      warning(paste("Column", colnames[i], "not present in data specified"))
    } 
  }
  return(all(vec))
}