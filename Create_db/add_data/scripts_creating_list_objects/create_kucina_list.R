# CREATE NESTED LIST OBJECT TO READ IN KUCINA ET AL DATA INTO DB ############################

# NOTE: when running all scripts in "\add_data" to replicate/ reconstruct the 
# first 40 datasets added to the db, we recommend first running "reformat_datasets.R"
# to have all raw datasets loaded when constructing the nested list objects 

files.sources = list.files("./functions", pattern = "\\.R$", full.names = TRUE, include.dirs = FALSE)
sapply(files.sources, source)
source("./Create_db/add_data/scripts_creating_list_objects/00_create_publication_study_level.R")

# Load required info from excel file -------------------------------------------------------
# read in relevant data 
publication_df <- readxl::read_excel("./Create_db/add_data/Book.xlsx", "publication_table", range = "A12:I12", 
                                     col_names = c("study", "authors", "conducted",
                                                   "added", "country","contact", 
                                                   "keywords", "APA-reference",
                                                   "publication_code")) 
study_df <- readxl::read_excel("./Create_db/add_data/Book.xlsx", "study_table", range = "A36:D36",
                               col_names = c("study", "n_groups",	"n_tasks", "comment"))
study_df$n_data <- 6 # encode number of data sets per study (by hand)
group_df <- readxl::read_excel("./Create_db/add_data/Book.xlsx", "group_table", range = "A37:G42",
                               col_names = c("study_in_publication", "study_description",
                                             "between_id",	"mean_age",	"percentage_female",
                                             "n_members",	"group_description"))
task_df <- readxl::read_excel("./Create_db/add_data/Book.xlsx", "task", range = "A34:D39", 
                              col_names = c("study_within_pub",	"Dataset", "task",
                                            "task_description"))
dataset_df <- readxl::read_excel("./Create_db/add_data/Book.xlsx", "dataset_overview_table", range = "A56:K61",
                                 col_names = c("study_within_publication", "data",	
                                               "data_excl", "n_participants",
                                               "n_blocks", "n_trials", "neutral_trials",
                                               "fixaction_cross",	"time_limit",
                                               "github",	"dataset in R"))
within_df <- readxl::read_excel("./Create_db/add_data/Book.xlsx", "within_table", range = "A73:D82",
                                col_names = c("study_within_publication",	"data set",
                                              "within_id",	"within_desciption"))
condition_df <- readxl::read_excel("./Create_db/add_data/Book.xlsx", "condition_descriptives", range = "A82:F91",
                                   col_names = c("study_in_publication",
                                                 "dataset & condition",	"percentage_congr",
                                                 "percentage_neutral",	"mean_obs_pp",	"n_obs"))

# NOTE: read in (dataset53), dataset54, dataset55, dataset56, and dataset57, 
# and dataset58 from "./Create_db/add_data/reformat_datasets.R"

# create publication and study level -------------------------------------------
pub <- list_study_level(publication_df, study_df, group_df)

# create data level ------------------------------------------------------------ 
data_added <- 0 # keep track of datasets already added
for(i in 1:nrow(study_df)){ # within each study    
  
        # TODO: change loop to 2:study_df$n_data[i] once Stroop2 issue sorted out
  for(k in 2:study_df$n_data[i]){ # loop over each dataset
    # TODO: change this to 1:study_df$n_data[i] once issue with stroop2 is sorted out!
    # CREATE DATA LIST
    pub[[i+1]][[k+1]] <- list()
    names(pub[[i+1]])[k+1] <-paste("data", k, sep = "")
    
    # assign respective raw data as observation_table and add condition column
    df <- eval(parse(text = dataset_df$`dataset in R`[k + data_added]))
    pub[[i+1]][[k+1]]$observation_table <- code_condition(df)
    
    # add dataset_table
    pub[[i+1]][[k+1]]$dataset_table <- data.frame(
      data_excl = dataset_df$data_excl[k + data_added],
      n_participants = dataset_df$n_participants[k + data_added], 
      n_blocks = dataset_df$n_blocks[k + data_added], 
      n_trials = dataset_df$n_trials[k + data_added], 
      neutral_trials = dataset_df$neutral_trials[k + data_added], 
      fixation_cross = dataset_df$fixaction_cross[k + data_added], 
      time_limit = dataset_df$time_limit[k + data_added], 
      mean_dataset_rt = NA, 
      mean_dataset_acc = NA, 
      github = dataset_df$github[k + data_added], 
      dataset_comment = NA, 
      between = unique(pub[[i+1]][[k+1]]$observation_table[6]),
      number_within_conditions = NA # code later
    )
    
    # add within_table
    pub[[i+1]][[k+1]]$within_table <- data.frame(
      within_name = within_df$within_id[k + data_added], 
      within_description = within_df$within_desciption[k + data_added]
    )
    
    # add task_table
    pub[[i+1]][[k+1]]$task_table <- data.frame(
      task_name = task_df$task[k + data_added], 
      task_description = task_df$task_description[k + data_added]
    )
    
    # add condition table
    # create df containing observations of respective condition only
    df_test <- remove_practice(pub[[i+1]][[k+1]]$observation_table) # remove practice trials
    df_cond <- filter_condition(df_test, cond = 1)  # filter by condition
    
    pub[[i+1]][[k+1]]$condition_table <- data.frame(
      condition_name = 1, 
      percentage_congruent = get_perc_congr(df_cond), 
      percentage_neutral = get_perc_neut(df_cond), 
      n_obs = get_n_obs(df_cond),
      mean_obs_per_participant = get_mean_obs_pp(df_cond), 
      mean_condition_rt = get_mean_rt(df_cond),
      mean_condition_acc = get_mean_acc(df_cond)
    )
    
    # if more than 1 condition: add rows for each condition
    if(length(unique(pub[[i+1]][[k+1]]$observation_table$condition)) > 1){
      for(condition in 2:length(unique(pub[[i+1]][[k+1]]$observation_table$condition))){
        # create df containing observations of respective condition only
        df_t <- remove_practice(pub[[i+1]][[k+1]]$observation_table) # remove practice trials
        df_con <- filter_condition(df_t, cond = condition)  # filter by condition
        
        # calculate info
        perc_congr <- get_perc_congr(df_con)
        perc_neut <- get_perc_neut(df_con)
        n_obs <- get_n_obs(df_con)
        mean_obs_pp <- get_mean_obs_pp(df_con)
        mean_condition_rt = get_mean_rt(df_con)
        mean_condition_acc = get_mean_acc(df_con)
        
        # extend condition table
        pub[[i+1]][[k+1]]$condition_table[condition, ] <- c(condition, 
                                                            perc_congr, perc_neut, 
                                                            n_obs, mean_obs_pp, 
                                                            mean_condition_rt, 
                                                            mean_condition_acc)
      }
    }
    
    # add matching within and between ids to conditon table 
    pub[[i+1]][[k+1]]$condition_table <- match_within_between(pub[[i+1]][[k+1]]$observation_table, 
                                                              pub[[i+1]][[k+1]]$condition_table)
    
    # add mean_dataset_rt and mean_dataset_acc to dataset_table
    pub[[i+1]][[k+1]]$dataset_table$mean_dataset_rt <- get_mean_rt(df_test)
    pub[[i+1]][[k+1]]$dataset_table$mean_dataset_acc <- get_mean_acc(df_test)
    
  }
  data_added <- data_added + study_df$n_data[i] # keep track of datasets added
}

# manually add additional within conditions --
within_df <- within_df[3:10, ]  # temporarly remove stroop2
within_df$data <- c(2,2,3,4,4,5,6,6) # TODO: change to c(1,1,2,2,3,4,4,5,6,6) 
within_list <- within_df %>%
  group_split(data)

for(dataset in 1:5){ # TODO: change to 1:6 once stroop2 issue sorted out
  for(i in 1:length(within_list[[dataset]]$within_id)){ # for each within condition in dataset
    pub[[2]][[dataset+2]]$within_table[i, 1] <- within_list[[dataset]]$within_id[i]
    pub[[2]][[dataset+2]]$within_table[i, 2] <- within_list[[dataset]]$within_desciption[i]
  }
}

# code number_within_conditions in dataset table: 
for(data in 1:5){
  pub[[2]][[data+2]]$dataset_table[1, 13] <- length(unique(pub[[2]][[data+2]]$observation_table[[7]]))
}

# save list object -------------------------------------------------------------
saveRDS(pub, file="./Create_db/add_data/kucina_list.RData")



