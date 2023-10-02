obtain_keys <- function(info_table, method){
  id_name = paste0(method, "_id")
  key_name = paste0(method, "_name")
  
  if (!id_name %in% names(info_table)){
    stop("Remember to add the proper db ids to the table first")
  }
  
  keys = info_table[, c(key_name, id_name)]
  
  if (is.character(keys[, key_name])){
    keys[, key_name] = parse_number(keys[, key_name])
    
  }
  
  return(keys)
}