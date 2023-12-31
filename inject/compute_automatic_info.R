# COMPUTING MISSING INFO FOR DATA BASE #--------------------------------------#

# Info computed in this script ---
# > for observation_table: 
#   - condition column
# > for dataset_table: 
#  - n_participants 
#  - n_blocks 
#  - n_trials 
#  - neutral_trials
#  - mean_dataset_rt
#  - mean_dataset_acc
# > for condition_table: 
#   - percentage_congr
#   - percentage_neut
#   - mean_obs_pp 
#   - n_obs
#   - add respective between_id and within_id 

library(dplyr)

# for between table -------------------
get_n_members <- function(pub, study, task, between_value){
  # input: nested list object with publication, study, and data level;
  # indeces of study, task, and between condition
  
  # filter observation_table by between value
  data <- pub[[study+1]][[task+2]][[4]]
  data <- data %>%
    filter(between == between_value)
  
  # count unique between IDs in observation_table of study i, task j
  n_members <- length(unique(data$subject))
  
  return(n_members)
}

# For observation_table -----------------

# code condition column based on combination of within and between column
code_condition <- function(df){
  # create overview of conditions
  conditions <- df %>%
    count(within, between) %>%
    select(within, between) %>%
    mutate(condition = 1:nrow(.))
  
  # add respective condition value to observation
  data <- df %>% 
    left_join(conditions, by = join_by(between, within))
  
  return(data)
}
  
# For dataset_table --------------------------

# create data frame without trial blocks 
remove_practice <- function(df) {
  df_test <- df[df$block != -999, ]
  return(df_test)
}


get_n <- function(df_test){
  n <- length(unique(df_test$subject))
  return(n)
}

get_n_blocks <- function(df_test){
  n_blocks <- length(unique(df_test$block))
  return(n_blocks)
}

get_n_trials <- function(df_test){
  n_trials <- length(unique(df_test$trial))
  return(n_trials)
}

get_neutral_trials <- function(df_test){
  return(ifelse(3 %in% df_test$congruency, 1, 0))
}

get_mean_rt <- function(df_test){
  return(round(mean(df_test$rt, na.rm = TRUE),3))
}

get_mean_acc <- function(df_test){
  return(round(mean(df_test$accuracy, na.rm = TRUE),3))
}


# For condition_table -------------------

# filter data frame by condition
filter_condition <- function(df_test, cond = 1) {
  # inputs:
  # - df_test: data frame (without practice trials)
  # - cond: condition_id to filter by 
  
  df_cond <- df_test[df_test$condition == cond, ]
  return(df_cond)
}


# percentage congruent
get_perc_congr <- function(df_cond){
  perc_congr <- round(sum(df_cond$congruency == 1) / length(df_cond$congruency),2)
  return(perc_congr)
}

# percentage neutral
get_perc_neut <- function(df_cond){
  perc_neut <- round(sum(df_cond$congruency == 3) / length(df_cond$congruency),2)
  return(perc_neut) 
}

# mean_obs_pp
get_mean_obs_pp <- function(df_cond){
  mean_obs <- df_cond %>% 
    group_by(subject) %>%
    summarise(N = n()) %>%
    summarise(mean(N))
  
  mean_obs <- round(mean_obs$`mean(N)`,0)
  
  return(mean_obs)
}

# n_obs
get_n_obs <- function(df_cond){
  n_obs <- nrow(df_cond)
  
  return(n_obs)
}


# get mathcing between_id and within_id for each row in condition_table
match_within_between <- function(observations_table, condition_table){
  # get overview over which between and within condition ID which condition per datasetid
  info_from_observations <- observations_table %>%
    count(within, between, condition) %>%
    mutate(condition_name = condition) %>%
    select(within, between, condition_name)
  
  # add matching within and between ID to condition table 
  condition_updated <- condition_table %>%
    left_join(info_from_observations, by = join_by(condition_name)) %>%
    rename(within_name = within, 
           between_name = between)
  
  return(condition_updated)
}


