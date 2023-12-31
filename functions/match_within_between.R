#' Match Within and Between Conditions
#'
#' This function matches within and between condition IDs to the corresponding conditions.
# It extracts the information from the observations table and updates the condition table with matching IDs.
# This is useful for associating conditions with their corresponding within and between conditions.
# The function performs a left join of the condition table with the extracted information.
# 
#' @param observations_table A data frame containing observations with columns: within, between, condition.
#' @param condition_table A data frame containing condition information.
# 
#' @return A data frame with the condition table updated to include within and between condition IDs.
#' @export
match_within_between <- function(observations_table, condition_table){
  # get overview over which between and within condition ID which condition per datasetid
  info_from_observations <- observations_table %>%
    dplyr::count(within, between, condition) %>%
    dplyr::mutate(condition_name = condition) %>%
    dplyr::select(within, between, condition_name)
  
  # add matching within and between ID to condition table 
  condition_updated <- condition_table %>%
    dplyr::left_join(info_from_observations, by = dplyr::join_by(condition_name)) %>%
    dplyr::rename(within_name = within, 
                  between_name = between)
  
  return(condition_updated)
}