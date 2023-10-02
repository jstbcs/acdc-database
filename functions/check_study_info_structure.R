# check study info structure
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