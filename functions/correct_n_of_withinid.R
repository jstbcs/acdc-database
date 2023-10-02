#' Verify the number of unique within conditions and check for duplicates.
#'
#' This function checks the number of unique within conditions and whether there are duplicate within_names in the within_table. It also ensures that the number of unique within conditions in the observation table matches the number of within_names in the within_table.
# 
#' @param within_table The within table to verify.
#' @param observation_table The observation table to verify.
# 
#' @details The 'correct_n_of_withinid' function verifies the consistency of within conditions between the within_table and the observation table. It checks for duplicate within_names in the within_table, ensuring that they are unique. It also ensures that the number of unique within conditions in the observation table matches the number of within_names in the within_table.
# 
#' @export
correct_n_of_withinid <- function(within_table, observation_table){
  # check if within ids are unique
  if(length(unique(within_table$within_name)) != nrow(within_table)){
    stop("Duplicate within_name in within_table found. Make sure within_name is unique")
  }
  
  # check if number of within id matches within columns in data table
  if(length(unique(observation_table$within)) > length(unique(within_table$within_name))){
    stop("Number of unique within conditions in data table is larger than in within_table table. 
         \nMake sure all within conditions are included in within_table.")
  } 
  # else if(length(unique(observation_table$within)) < length(unique(within_table$within_name))){
  #   stop("The within_table table contains more unique within_names than the data table does. 
  #        \nMake sure the within column in the data table is coded correctly and the 
  #        within_table table contains only relevant within conditions")
  # }
}
