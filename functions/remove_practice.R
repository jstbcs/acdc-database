#' Remove Practice Trials
#'
#' This function creates a new data frame by removing practice trials (blocks with -999) from the input data frame.
# 
#' @param df The input data frame, typically containing practice trials.
# 
#' @return A new data frame without practice trials (blocks with -999).
# 
#' @export
remove_practice <- function(df) {
  df_test <- df[df$block != -999, ]
  return(df_test)
}