get_n_blocks <- function(df_test){
  n_blocks <- length(unique(df_test$block))
  return(n_blocks)
}