#' Get the Number of Participants in a Dataset
#'
#' This function calculates the number of unique participants in a dataset.
# 
#' @param df_test A data frame containing the dataset.
# 
#' @return The number of participants as an integer.
# 
#' @export
get_n <- function(df_test){
  n <- length(unique(df_test$subject))
  return(n)
}