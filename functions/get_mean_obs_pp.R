#' Calculate the Mean Number of Observations per Participant
#'
#' This function calculates the mean number of observations per participant in a given condition.
# It groups the data by subject and counts the number of observations for each participant.
# The function then calculates the mean of these counts.
# 
#' @param df_cond A data frame containing the condition data.
#' 
#' @return The mean number of observations per participant as a rounded integer.
# 
#' @export
get_mean_obs_pp <- function(df_cond){
  mean_obs <- df_cond %>% 
    dplyr::group_by(subject) %>%
    dplyr::summarise(N = n()) %>%
    dplyr::summarise(mean(N))
  
  mean_obs <- round(mean_obs$`mean(N)`,0)
  
  return(mean_obs)
}