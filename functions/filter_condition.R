#' Filter a data frame by condition.
#'
#' This function takes a data frame and filters it to return only the rows where the condition ID matches the provided value.
#'
#' @param df_test A data frame without practice trials.
#' @param cond The condition ID to filter by.
#'
#' @return A filtered data frame containing only the rows where the condition ID matches \code{cond}.
#'
#' @export
# filter data frame by condition
filter_condition <- function(df_test, cond = 1) {
  # inputs:
  # - df_test: data frame (without practice trials)
  # - cond: condition_id to filter by 
  
  df_cond <- df_test[df_test$condition == cond, ]
  return(df_cond)
}