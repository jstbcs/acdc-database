#' Check the structure of a data_[i] list element.
#'
#' This function validates the entire structure of a data_[i] list element, including the column names of the sub-element data frames.
#'
#' @param data_i A list element representing data_[i] structure to be validated.
#'
#' @return This function does not return a value. It raises an error if the 'data_i' structure does not meet the expected criteria.
#'
#' @details This function checks the provided 'data_i' structure to ensure it adheres to the expected format. It verifies the presence of required elements within 'data_i' and validates the structure of the sub-element data frames.
#'
#' @seealso \code{\link{correct_elements_in_data_list}}, \code{\link{correct_cols_in_task_table}}, \code{\link{correct_cols_in_dataset_table}}, \code{\link{correct_cols_in_within_table}}, \code{\link{correct_cols_in_condition_table}}, \code{\link{correct_cols_in_observation_table}}, \code{\link{correct_n_of_withinid}}
#'
#' @export
check_data_level_structure <- function(data_i){
  
  # check if input is a list
  if (inherits(data_i, "list") == FALSE)
  {
    stop("data_NUMBER object must be a list")
  } 
  
  # check if list contains correct elements
  correct_elements_in_data_list(data_i)
  
  # check if each element in data_i list is a df and contains required columns
  correct_cols_in_task_table(data_i$task_table)
  correct_cols_in_dataset_table(data_i$dataset_table)
  correct_cols_in_within_table(data_i$within_table)
  correct_cols_in_condition_table(data_i$condition_table)
  correct_cols_in_observation_table(data_i$observation_table)
  
  # check if number of within condition in data equals number of within_names in within_table
  correct_n_of_withinid(data_i$within_table, data_i$observation_table)
}