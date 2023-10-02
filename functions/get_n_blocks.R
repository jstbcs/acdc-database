#' Get the Number of Blocks in a Dataset
#'
#' This function calculates the number of blocks in a dataset by counting the unique block values.
# 
#' @param df_test A data frame containing the dataset.
# 
#' @return The number of blocks as an integer.
# 
#' @export
get_n_blocks <- function(df_test){
  n_blocks <- length(unique(df_test$block))
  return(n_blocks)
}