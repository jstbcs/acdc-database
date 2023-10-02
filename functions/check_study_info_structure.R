#' Check the structure of the study information table.
#'
#' This function validates the structure of the provided study information table. It ensures that the table is a data frame, contains only one row, and confirms that certain columns are not specified.
#'
#' @param study_info A data frame representing the study information table to be validated.
#'
#' @details This function checks the structure of the provided study information table, verifying that it meets specific criteria.
#'
#' @return This function does not return a value but raises errors if the study information table structure does not match the expected criteria.
#'
#' @export
check_study_info_structure <- function(study_info){
  if(is.data.frame(study_info) == FALSE)
  {# check study info is data frame
    stop("Study-Info is not a dataframe")
  } 
  if (nrow(study_info != 1)){# check the number of rows in that dataframe, should be 1
    stop("Study-Info contains more than one row")
  } 
  
  # No need for n_groups. n_tasks and comment to be specified for entry
  
  confirm_columns_not_specified(c("n_groups", "n_tasks", "commend"), study_info)
}