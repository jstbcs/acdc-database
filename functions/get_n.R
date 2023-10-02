get_n <- function(df_test){
  n <- length(unique(df_test$subject))
  return(n)
}