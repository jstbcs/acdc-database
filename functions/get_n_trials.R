#' Get the Number of Trials in a Dataset
#'
#' This function calculates the number of trials in a dataset by counting the unique trial values.
# 
#' @param df_test A data frame containing the dataset.
# 
#' @return The number of trials as an integer.
# 
#' @export
get_n_trials <- function(df_test){
  n_trials <- length(unique(df_test$trial))
  return(n_trials)
}