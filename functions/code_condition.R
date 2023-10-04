#' Code the condition column based on combinations of within and between columns.
#'
#' This function generates a condition column based on the combinations of the within and between columns in the input dataframe.
#'
#' @param df A dataframe containing within and between columns.
#'
#' @details This function calculates unique conditions based on the combinations of within and between columns and assigns a condition value to each row in the dataframe.
#'
#' @return A dataframe with an additional 'condition' column, which represents the coded conditions based on within and between combinations.
#'
#' @export
code_condition <- function(df){
  # create overview of conditions
  conditions <- df %>%
    dplyr::count(within, between) %>%
    dplyr::select(within, between) %>%
    dplyr::mutate(condition = 1:nrow(.))
  
  # add respective condition value to observation
  data <- dplry::left_join(df, conditions, by = dplyr::join_by(between, within))
  
  return(data)
}