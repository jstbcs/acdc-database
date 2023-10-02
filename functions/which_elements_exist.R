#' Return Elements of Column Names that Exist in Specified Object
#' 
#' This function returns the elements of a character vector of column names that exist in the specified R object. 
#'
#' @param colnames A character vector of column names you want to check for existence in the object.
#' @param object The R object in which you want to check for the existence of column names.
#'
#' @return A character vector containing the column names that exist in the specified object.
#'
#' @examples
#' # Create a sample data frame
#' my_data <- data.frame(Name = c("Alice", "Bob", "Charlie"),
#'                      Age = c(28, 32, 22))
#'
#' # Check for the existence of specific column names
#' existing_columns <- which_elements_exist(c("Name", "Height"), my_data)
#' print(existing_columns)  # Output: [1] "Name"
#'
#' @export
which_elements_exist <- function(colnames, object){
  vec = c()
  for (i in seq_along(colnames))
  {
    vec[i] = exists(colnames[i], object)
  }
  return(colnames[which(vec == TRUE)])
}
