# check study info structure
check_study_table_structure <- function(study_table, entry_list_info){
  names = names(study_table)
  if(is.data.frame(study_table) == FALSE)
  {# check study info is data frame
    stop("Study-Info is not a dataframe")
  } 
  if (nrow(study_table) != 1){# check the number of rows in that dataframe, should be 1
    stop("Study-Info contains more than one row")
  } 
  
  confirm_object_names(study_table, entry_list_info$study_table)
  
  if (is.na(study_table$comment) | is.null(study_table$comment) | study_table$comment == ""){
    stop("Comment can not be empty")
  }
}
