#' Check for presence of Neutral Trials in a Dataset
#'
#' This function identifies whether a dataset has neutral trials. Neutral trials are identified by a congruency code of 3.
# 
#' @param df_test A data frame containing the dataset.
# 
#' @return Has neutral trials? 1, 0
# 
#' @export
get_neutral_trials <- function(df_test){
  return(ifelse(3 %in% df_test$congruency, 1, 0))
}