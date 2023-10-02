#' Check the structure of a study's group_info element.
#'
#' This function validates the structure of the group_info element within a study, ensuring it adheres to the expected format.
#'
#' @param group_info A data frame representing group information to be validated.
#'
#' @details This function checks the provided 'group_info' data frame to ensure it meets the expected structure and content requirements. It verifies that 'group_info' is a data frame with a single row and confirms that specific columns exist.
#'
#' @return This function does not return a value. It raises an error if the 'group_info' structure does not meet the expected criteria.
#'
#' @seealso \code{\link{confirm_columns_not_specified}}
#'
#' @export
#' 
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