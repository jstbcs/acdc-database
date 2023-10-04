#' Replace ID Keys in Data
#'
#' This function replaces ID keys in a data frame with their corresponding names from a keys data frame.
# 
#' @param data The input data frame containing the ID keys.
# @param keys The keys data frame with two columns: the key names and their corresponding IDs.
# @param method The method name, such as "within" or "between."
# 
#' @return A data frame with the ID keys replaced by their corresponding names.
# 
#' @export
replace_id_keys_in_data <- function(data, keys, method, suffix = NULL){
  id_name = paste0(method, "_id")
  
  # Keys has colums: within_name and within_id, we want within, within_id
  if (is.null(suffix)){
    colname_key_var = method
  } else {
    colname_key_var = paste0(method, suffix)
  }

  colnames(keys) = c(colname_key_var, id_name)
  
  data = data %>% 
    dplyr::left_join(., keys) %>% 
    dplyr::select(-{{colname_key_var}}) 
  return(data)
}
