# create data frame without trial blocks 
remove_practice <- function(df) {
  df_test <- df[df$block != -999, ]
  return(df_test)
}