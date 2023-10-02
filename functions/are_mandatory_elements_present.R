are_mandatory_elements_present <- function(object, mandatory_colnames){
  names = names(object)
  for (name in mandatory_colnames){
    if (!name %in% names){
      msg = paste("The column:", name, "must be specified in the data.")
      stop(msg)
    }
  }
}