#' Finish Data Levels When Inserting Entries to the Database
#'
#' This function completes data levels when inserting entries from an online form into the database.
#' It processes and organizes the submitted data, such as observation tables, condition tables, and dataset tables.
# 
#' @param pub A list representing the publication levels.
#' @param entry A data frame containing information about the form entry.
#' @param dataframe_list A list of data frames containing the submitted data.
#' @param n_studies The number of studies submitted.
#'
#' @details The function processes the submitted data for each study and task, organizing it into the corresponding data tables.
# It calculates various statistics, such as the number of participants, blocks, trials, and more, for the dataset table.
# Additionally, it computes information for the condition table based on the data.
# 
#' @return A modified publication list containing the organized data levels.
#'
#' @export
finish_data_level  <- function(pub, entry, dataframe_list, n_studies){
  if(n_studies == 1){
    # JUST ONE STUDY WAS SUBMITTED 
    for(i in 1:entry$Number.of.attentional.control.tasks){
      
      # create observation_table  -----------------
      if(entry$Number.of.attentional.control.tasks == 1){
        observations_name <- "processed_data_study1"
      } else {
        observations_name <- paste("processed_data_study1_task",i, sep="")
      }
      
      
      pub[[2]][[i+2]]$observation_table <- dataframe_list[[which(names(dataframe_list) == observations_name)]]
      
      # add condition column to dataset 
      # (unique combinations of between and within column values)
      pub[[2]][[i+2]]$observation_table <- code_condition(pub[[2]][[i+2]]$observation_table)
      
      
      # Compute automatic info for dataset_table --------------
      
      # create data frame without trial blocks 
      df_test <- remove_practice(pub[[2]][[i+2]]$observation_table)
      # get info for dataset_table
      pub[[2]][[i+2]]$dataset_table$n_participants <- get_n(df_test)
      pub[[2]][[i+2]]$dataset_table$n_blocks <- get_n_blocks(df_test)
      pub[[2]][[i+2]]$dataset_table$n_trials <- get_n_trials(df_test)
      pub[[2]][[i+2]]$dataset_table$neutral_trials <- get_neutral_trials(df_test)
      pub[[2]][[i+2]]$dataset_table$mean_dataset_rt <- get_mean_dataset_rt(df_test)
      pub[[2]][[i+2]]$dataset_table$mean_dataset_acc <- get_mean_dataset_acc(df_test)
      
      # create condition_table --------------------------
      
      # initiate with first condition
      # get required info
      df_condition1 <- filter_condition(df_test, cond = 1) # get data of condition 1
      perc_congr <- get_perc_congr(df_condition1)
      perc_neutral <- get_perc_neut(df_condition1)
      mean_obs_pp <- get_mean_obs_pp(df_condition1)
      n_obs <- get_n_obs(df_condition1)
      mean_cond_rt <- get_mean_condition_rt(df_condition1)
      mean_cond_acc <- get_mean_condition_acc(df_condition1)
      
      # fill table
      pub[[2]][[i+2]]$condition_table <- data.frame(
        condition_name = 1, 
        percentage_congruent = perc_congr, 
        percentage_neutral = perc_neutral, 
        n_obs = n_obs,
        mean_obs_per_participant = mean_obs_pp,
        mean_condition_rt = mean_cond_rt, 
        mean_condition_acc = mean_cond_acc
      )
      
      # if more than 1 condition: fill in following ones
      if(length(unique(df_test$condition)) > 1){
        for(k in 2:length(unique(df_test$condition))){
          # get required info
          df_condition <- filter_condition(df_test, cond = k) # get data of condition k
          perc_congr <- get_perc_congr(df_condition)
          perc_neutral <- get_perc_neut(df_condition)
          mean_obs_pp <- get_mean_obs_pp(df_condition)
          n_obs <- get_n_obs(df_condition)
          mean_cond_rt <- get_mean_condition_rt(df_condition)
          mean_cond_acc <- get_mean_condition_acc(df_condition)
          
          # append to condition_table
          pub[[2]][[i+2]]$condition_table[k, ] <- c(k, perc_congr, perc_neutral, 
                                                    n_obs, mean_obs_pp,
                                                    mean_cond_rt, mean_cond_acc)
        }
      }
      
      # last: compute n_members for each group
      for(j in 1:pub[[2]][[1]]$n_groups){
        n_members <- get_n_members(pub = pub, study = 1, task = i, between_value = j)
        # place it in right place in nested list
        pub[[2]][[2]][j,4] <- n_members
      }
    }
  } else {
    
    # IF >1 STUDIES WERE SUBMITTED
    for(i in 1:entry$Number.of.studies){
      n_inhibition_tasks <- paste("Number.of.attentional.control.tasks...STUDY.",i, sep="")
      n_inhibition_tasks <- entry[1, n_inhibition_tasks]
      
      # for each task in study i
      for(j in 1:n_inhibition_tasks){
        
        # create observation_table ------------------------
        if(n_inhibition_tasks == 1){
          observations_name <- paste("processed_data_study",i, sep="")
        } else {
          observations_name <- paste("processed_data_study",i,"_task",j, sep="")
        }
        
        pub[[i+1]][[j+2]]$observation_table <- dataframe_list[[which(names(dataframe_list) == observations_name)]]
        
        # add condition column to dataset 
        # (unique combinations of between and within column values)
        pub[[i+1]][[j+2]]$observation_table <- code_condition(pub[[i+1]][[j+2]]$observation_table)
        
        # Compute automatic info for dataset_table --------------
        
        # create data frame without trial blocks 
        df_test <- remove_practice(pub[[i+1]][[j+2]]$observation_table)
        # get info for dataset_table
        pub[[i+1]][[j+2]]$dataset_table$n_participants <- get_n(df_test)
        pub[[i+1]][[j+2]]$dataset_table$n_blocks <- get_n_blocks(df_test)
        pub[[i+1]][[j+2]]$dataset_table$n_trials <- get_n_trials(df_test)
        pub[[i+1]][[j+2]]$dataset_table$neutral_trials <- get_neutral_trials(df_test)
        pub[[i+1]][[j+2]]$dataset_table$mean_dataset_rt <- get_mean_dataset_rt(df_test)
        pub[[i+1]][[j+2]]$dataset_table$mean_dataset_acc <- get_mean_dataset_acc(df_test)
        
        # create condition_table --------------------------
        
        # initiate with first condition
        # get required info
        df_condition1 <- filter_condition(df_test, cond = 1) # get data of condition 1
        perc_congr <- get_perc_congr(df_condition1)
        perc_neutral <- get_perc_neut(df_condition1)
        mean_obs_pp <- get_mean_obs_pp(df_condition1)
        n_obs <- get_n_obs(df_condition1)
        mean_cond_rt <- get_mean_condition_rt(df_condition1)
        mean_cond_acc <- get_mean_condition_acc(df_condition1)
        
        # fill table
        pub[[i+1]][[j+2]]$condition_table <- data.frame(
          condition_name = 1, 
          percentage_congruent = perc_congr, 
          percentage_neutral = perc_neutral, 
          n_obs = n_obs,
          mean_obs_per_participant = mean_obs_pp,
          mean_condition_rt = mean_cond_rt, 
          mean_cond_acc = mean_cond_acc
        )
        
        # if more than 1 condition: fill in following ones
        if(length(unique(df_test$condition)) > 1){
          for(n in 2:length(unique(df_test$condition))){
            # get required info
            df_condition <- filter_condition(df_test, cond = n) # get data of condition k
            perc_congr <- get_perc_congr(df_condition)
            perc_neutral <- get_perc_neut(df_condition)
            n_obs <- get_n_obs(df_condition)
            mean_obs_pp <- get_mean_obs_pp(df_condition)
            mean_cond_rt <- get_mean_condition_rt(df_condition)
            mean_cond_acc <- get_mean_condition_acc(df_condition)
            
            # append to condition_table
            pub[[i+1]][[j+2]]$condition_table[n, ] <- c(n, perc_congr, perc_neutral, 
                                                        n_obs, mean_obs_pp,
                                                        mean_cond_rt,
                                                        mean_cond_acc)
          }
        }
        
        # last: compute n_members for each group
        for(k in 1:pub[[i+1]][[1]]$n_groups){
          n_members <- get_n_members(pub = pub, study = i, task = j, between_value = k)
          # place it in right place in nested list
          pub[[i+1]][[2]][k,4] <- n_members
        }
      }
    }
    
    return(pub)
  }
}


