return_id_name_from_table <- function(table_name){
  name = stringr::str_replace(table_name, "table$", "id")
  return(name)
}