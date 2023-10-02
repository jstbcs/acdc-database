return_table_name_from_id <- function(id_name){
  name = stringr::str_replace(id_name, "id$", "table")
  return(name)
}