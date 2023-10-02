get_n_trials <- function(df_test){
  n_trials <- length(unique(df_test$trial))
  return(n_trials)
}