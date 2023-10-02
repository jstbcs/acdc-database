get_mean_obs_pp <- function(df_cond){
  mean_obs <- df_cond %>% 
    group_by(subject) %>%
    summarise(N = n()) %>%
    summarise(mean(N))
  
  mean_obs <- round(mean_obs$`mean(N)`,0)
  
  return(mean_obs)
}