#' Check the structure of the study table.
#'
#' This function validates the structure of the provided study table, ensuring it is a data frame with a single row. It also confirms that the expected column names are present and checks that the 'comment' field is not empty.
#'
#' @param study_table A data frame representing the study table to be validated.
#'
#' @details This function verifies the structure of the study table in terms of data frame format, column names, and the 'comment' field.
#'
#' @return This function does not return a value but raises errors if the study table structure does not match the expected criteria.
#'
#' @export
check_study_table_structure <- function(study_table){
  entry_list_info = get_database_info()
  
  names = names(study_table)
  if(is.data.frame(study_table) == FALSE)
  {# check study info is data frame
    stop("Study-Info is not a dataframe")
  } 
  if (nrow(study_table) != 1){# check the number of rows in that dataframe, should be 1
    stop("Study-Info contains more than one row")
  } 
  
  confirm_object_names(study_table, entry_list_info$study_table)
  
  if (is.na(study_table$study_comment) | is.null(study_table$study_comment) | study_table$study_comment == ""){
    stop("Comment can not be empty")
  }
}
