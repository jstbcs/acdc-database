prep_within_data <- function(within_data, raw_data){  
  within_data = clean_char_columns(within_data)

  df_test = remove_practice(raw_data) # remove practice trials

  added_info = data.frame(
    percentage_congruent = get_perc_congr(df_test), 
    percentage_neutral = get_perc_neut(df_test), 
    n_obs = get_n_obs(df_test),
    mean_obs_per_participant = get_mean_obs_pp(df_test), 
    mean_condition_rt = get_mean_rt(df_test),
    mean_condition_acc = get_mean_acc(df_test)
  )

  within_data = cbind(within_data, added_info)

  return(within_data)
}