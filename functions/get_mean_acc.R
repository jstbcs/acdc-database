#' Get Mean Accuracy
#'
#' This function calculates the mean accuracy from a data frame that includes an accuracy column.
# It provides the mean accuracy as a numeric value, rounded to three decimal places.
# Missing (NA) values in the accuracy column are omitted during the calculation.
# 
#' @param df A data frame containing an 'accuracy' column.
# 
#' @return The mean accuracy as a numeric value rounded to three decimal places.
# 
#' @examples
#' # Create a sample data frame with an 'accuracy' column
#' data_frame <- data.frame(
#'   accuracy = c(0.95, 0.88, 0.75, 0.92, NA, 0.78, 0.87)
#' )
# 
#' # Get the mean accuracy
#' mean_accuracy <- get_mean_acc(data_frame)
# print(mean_accuracy)
# 
#' # Output:
#' # [1] 0.857
# 
#' @export
get_mean_acc <- function(df){
  return(round(mean(df$accuracy, na.rm = TRUE),3))
}