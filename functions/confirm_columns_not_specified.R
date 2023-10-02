# Informs user about columns not present in object
# Requires input to continue
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