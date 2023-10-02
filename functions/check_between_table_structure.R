# This checks structure of study$between_table
check_between_table_structure <- function(between_table, entry_list_info){
  names = names(between_table)
  if(is.data.frame(between_table) == FALSE)
  {
    stop("Between-Table is not a dataframe")
  }
  
  if (nrow(between_table) > 1){
    if (!"group_description" %in% names){
      stop("Object needs to have a 'group_description' element")
    }
    
    if (any(is.na(between_table$group_description)) | any(is.null(between_table$group_description)) | any(between_table$group_description == "")){
      stop("group_description can not be empty")
    }
  }
  
  confirm_object_names(between_table, entry_list_info$between_table)
}