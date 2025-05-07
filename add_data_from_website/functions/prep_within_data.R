prep_within_data <- function(within_data, raw_data){  
  within_data = clean_char_columns(within_data)

  # Remove practice trials
  df_test <- remove_practice(raw_data)
  
  # Split by condition and compute summary statistics
  added_info = data.frame()
  
  for (i in 1:nrow(within_data)){
    df_cond = df_test[df_test$within == within_data[i, "identifier"], ]
    
    added_info = rbind(added_info,
                       data.frame(
                         percentage_congruent = get_perc_congr(df_cond),
                         percentage_neutral = get_perc_neut(df_cond),
                         n_obs = get_n_obs(df_cond),
                         mean_obs_per_participant = get_mean_obs_pp(df_cond),
                         mean_condition_rt = get_mean_rt(df_cond),
                         mean_condition_acc = get_mean_acc(df_cond)
                       )
    )
  }
  
  
  within_data = cbind(within_data, added_info)

  return(within_data)
}
