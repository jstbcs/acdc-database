# This checks structure of study$group_info
check_group_info_structure <- function(group_info){
  if(is.data.frame(group_info) == FALSE)
  {
    stop("Group-Info is not a dataframe")
  }
  if(nrow(group_info != 1))
  {
    stop("Group-Info contains more than one row")
  }
  confirm_columns_not_specified(c("mean_age", "percentage_female",
                                  "n_participants", "group_description"))
}