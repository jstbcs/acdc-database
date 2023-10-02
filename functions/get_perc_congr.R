# percentage congruent
get_perc_congr <- function(df_cond){
  perc_congr <- round(sum(df_cond$congruency == 1) / length(df_cond$congruency),2)
  return(perc_congr)
}