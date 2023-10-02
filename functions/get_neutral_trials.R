get_neutral_trials <- function(df_test){
  return(ifelse(3 %in% df_test$congruency, 1, 0))
}