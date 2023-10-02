check_publication_table <- function(pub_table, entry_list_info){
  names = names(pub_table)
  stop_if_names_duplicated(names)
  
  confirm_object_names(pub_table, entry_list_info$publication_table)
}