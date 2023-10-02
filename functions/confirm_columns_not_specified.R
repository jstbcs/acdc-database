#' Inform the user about columns not present in the object and require input to continue.
#'
#' This function checks if specific column names are present in the object and informs the user about missing columns. 
#' It then asks the user if they want to continue with missing columns, in which case 'NULL' will be entered into the database.
#'
#' @param colnames A character vector of column names to check for in the object.
#' @param object The object or dataframe to check for the specified column names.
#'
#' @details If any of the specified column names are missing in the object, a warning message is displayed to inform the user.
# The message includes the missing column names and asks whether the user wants to continue with missing columns.
#'
#' @export
confirm_columns_not_specified <- function(colnames, object){
  vec = c()
  for (i in seq_along(colnames))
  {
    vec[i] = exists(colnames[i], object)
  }
  if (all(vec) == FALSE)
  {
    message = paste(
      "Caution, you have not specified the columns: \n",
      paste(colnames[which(vec == FALSE)], collapse = "; "),
      "\ndo you want to continue adding the data anyway? \nNULL will be entered into the database in columns not specified"
    )
    continue_after_warning(message)
  }
}