#' Calculate Percentage of Neutral Trials
#'
#' This function calculates the percentage of neutral trials in a dataset.
# 
#' @param df_cond A data frame containing the dataset.
# 
#' @return The percentage of neutral trials as a numeric value, rounded to two decimal places.
# 
#' @export
get_perc_neut <- function(df_cond){
  perc_neut <- round(sum(df_cond$congruency == 3, na.rm = TRUE) / length(df_cond$congruency),2)
  return(perc_neut) 
}