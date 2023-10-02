# percentage neutral
get_perc_neut <- function(df_cond){
  perc_neut <- round(sum(df_cond$congruency == 3) / length(df_cond$congruency),2)
  return(perc_neut) 
}