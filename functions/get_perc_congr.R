#' Calculate Percentage of Congruent Trials
#'
#' This function calculates the percentage of congruent trials in a dataset.
# 
#' @param df_cond A data frame containing the dataset.
# 
#' @return The percentage of congruent trials as a numeric value, rounded to two decimal places.
# 
#' @export
get_perc_congr <- function(df_cond){
  perc_congr <- round(sum(df_cond$congruency == 1) / length(df_cond$congruency),2)
  return(perc_congr)
}