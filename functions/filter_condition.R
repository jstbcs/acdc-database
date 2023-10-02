# filter data frame by condition
filter_condition <- function(df_test, cond = 1) {
  # inputs:
  # - df_test: data frame (without practice trials)
  # - cond: condition_id to filter by 
  
  df_cond <- df_test[df_test$condition == cond, ]
  return(df_cond)
}