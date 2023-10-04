#' Get Mean Reaction Time (RT)
#'
#' This function calculates the mean reaction time (RT) from a data frame that includes an RT column.
# It provides the mean RT as a numeric value, rounded to three decimal places.
# Missing (NA) values in the RT column are omitted during the calculation.
# 
#' @param df A data frame containing an 'rt' (reaction time) column.
# 
#' @return The mean RT as a numeric value rounded to three decimal places.
# 
#' @examples
#' # Create a sample data frame with an 'rt' column
#' data_frame <- data.frame(
#'   rt = c(250, 300, 210, 280, NA, 320, 270)
#' )
# 
#' # Get the mean RT
#' mean_rt <- get_mean_rt(data_frame)
# print(mean_rt)
# 
#' # Output:
#' # [1] 271.667
# 
#' @export
get_mean_rt <- function(df){
  return(round(mean(df$rt, na.rm = TRUE),3))
}