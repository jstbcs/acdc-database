#' Obtain Keys
#'
#' This function extracts key-value pairs from the input information table, such as the mapping
# between IDs and names, based on the specified method.
# 
#' @param info_table A data frame containing information with IDs and names.
#' @param method The method for obtaining keys (e.g., "publication" or "study").
# 
#' @return A data frame with two columns: key and ID, where the key can be a numeric or character value.
# 
#' @export
obtain_keys <- function(info_table, method){
  id_name = paste0(method, "_id")
  key_name = paste0(method, "_name")
  
  if (!id_name %in% names(info_table)){
    stop("Remember to add the proper db ids to the table first")
  }
  
  keys = info_table[, c(key_name, id_name)]
  
  if (is.character(keys[, key_name])){
    keys[, key_name] = readr::parse_number(keys[, key_name])
    
  }
  
  return(keys)
}