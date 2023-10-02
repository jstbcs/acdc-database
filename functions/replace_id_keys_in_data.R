replace_id_keys_in_data <- function(data, keys, method){
  id_name = paste0(method, "_id")
  
  # Keys has colums: within_name and within_id, we want within, within_id
  colnames(keys) = c(method, id_name)
  
  data = data %>% 
    dplyr::left_join(., keys) %>% 
    select(-{{method}}) 
  return(data)
}