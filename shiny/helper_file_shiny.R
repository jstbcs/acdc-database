# 
# Helper functions and vectors for inhibition task db shiny app 
# 
library(stringr)

# vector storing criteria to be chosen
criteria <- c("Task type(s)" = "task_name",
              "Mean reaction time (in seconds)" = "mean_dataset_rt",
              "Mean accuracy" = "mean_dataset_acc",
              "Number of participants" = "n_participants",
              "Number of blocks per participant" = "n_blocks",
              "Number of trials per block" = "n_trials",
              "Neutral stimuli included?" = "neutral_trials",
              #"Time limit for responses (in ms)" = ,
              #"Existence of between-subject manipulation?" = , 
              #"Existence of within-subject manipulation (besides congruency)?" = ,
              #"Conducted (Year of Publication)" = ,
              "Publication Code" = "publication_code"
              )

# TODO:connect to db to update automatically
publication_codes <- c("pratte_2010_exploring",
               "mermet_2018_should ",
               "hedge_2018_reliability",
               "vonbastiaan_2015_evidence",
               "whitehead_2020",
               "tang_2022_dual",
               "chetverikov_2017_blame",
               "stahl_2014_behavioral",
               "ebersole_2016_many")


# function to choose default value of "value" fields 
get_default_value <- function(criterion, operator){
  # only execute once operator has been chosen
  if(!is.null(operator)){
    default_value <- switch(criterion,
                            "mean_dataset_rt" = 0.7, 
                            "mean_dataset_acc" = 0.8, 
                            "n_participants" = 100, 
                            "n_blocks" = 5, 
                            "n_trials" = 30, 
                            #"Time limit for responses (in ms)" = 2000,
                            #"Conducted (Year of Publication)" = 2010
    )
    
    # optionally: second default value for between operator 
    default_value_b <- ifelse(operator == "between",
                              ifelse(criterion == "Conducted (Year of Publication)", # for "conducted" choose current year
                                     format(Sys.Date(), "%Y"),
                                     default_value * 1.2),  # in all other cases: increase by 20%
                              NA)
    
    
        return(c(default_value, default_value_b))
    
  }
}

# column names for suited_data_df
colnames_suited <- c("Dataset ID", "Publication Code", "Authors", "Conducted", "Task Type", 
                     "Sample Size", "Blocks per participant", "Trials per block", 
                     "Existence of neutral trials", "Within manipulation?", "Between manipulation?")

# column names for descriptives_df 
colnames_descriptives <- c("Dataset ID", "Publication ID", "Study ID", "Condition ID", "Task Type", 
                           "Sample Size", "Tials per block", "Mean number of trials per participant", 
                           "Percentage congruent", "Percentage of neutral trials", "Time limit (in ms)",
                           "Mean reaction time (dataset)", "Mean accuracy (dataset)", "Within manipulation", "Between manipulation")

# function for server: merge existing lists 
merge_lists <- function(x, y) {
  c(x, y)
}


# function to get overview_df or descriptive_df based on chosen arguments
get_filtered_df <- function(argument_list, conn, type=c("overview", "descriptives", "detailed")){
  if(length(argument_list[[1]]) > 0){ # once first argument has been chosen
    # update arguments 
    arguments <- list()
    for(i in 1:length(argument_list[[1]])){
      # if more than one value, split into vector 
      if(grepl(";", argument_list[[3]][i])) values=str_split(argument_list[[3]][i], "; ")[[1]] else values=argument_list[[3]][i]
      # loop over list adn update arguments
      arguments <- arguments %>%
        add_argument(
          conn, 
          argument_list[[1]][i], 
          argument_list[[2]][i], 
          values
        )
    }
    # get chosen data frame 
    if(type == "overview"){
      df <- get_overview_information(conn, arguments, "and")
      df <- df[, c(1,2,12,4,5,6,7,8,9,10,11)] # reorder and delete authors 
      colnames(df) <- colnames_suited
      df[,9] <- ifelse(df[,9] == 0, FALSE, TRUE) # convert numeric input to logical
      df[,10] <- ifelse(df[,10] == 0, FALSE, TRUE)
      df[,11]<- ifelse(df[,11] == 0, FALSE, TRUE)
      
    } else if (type == "descriptives"){
      df <- get_descriptive_information(conn, arguments, "and")
      colnames(df) <- colnames_descriptives
      
    } else if (type == "detailed"){
      df <- get_detailed_information(conn, arguments, "and")
    }
    
  } else { # when no arguments are chosen 
    df <- data.frame()
  }
  return(df)
}


# TODO: pipe same line as code (loop); indentions; seperate ";", make variables characters, 
# function to create R code based on chosen arguments
get_R_code <- function(argument_list){
  Rcode <- "if (!require('acdcquery')) install.packages('acdcquery') \nlibrary(acdcquery) \n \n # create connection to SQL data base \nconn <- connect_to_db('acdc.db')\n"
  
  
  if(length(argument_list[[1]]) > 0){ # once first argument has been chosen
    
  # install, load, connect to db -------
  set_up <- "\n \n # specify filter arguments\n arguments <- list() \n"
  
  # modify arguments based on user input ----
  arguments <- c()
  for(i in 1:length(argument_list[[1]])){
    # turn non-numeric elements into characters
    if(!is.numeric(argument_list[[1]][i])) arg1=paste0("'",argument_list[[1]][i],"'") else arg1=argument_list[[1]][i]
    if(!is.numeric(argument_list[[2]][i])) arg2=paste0("'",argument_list[[2]][i],"'") else arg2=argument_list[[2]][i]
    if(!is.numeric(argument_list[[3]][i])) arg3=paste0("'",argument_list[[3]][i],"'") else arg3=argument_list[[3]][i]
    
    # if between operator: print values as
    if(arg2 == "'between'") arg3=paste0("c(", str_split(arg3, "; ")[[1]][1], ",", str_split(arg3, "; ")[[1]][2], ")") else arg3=arg3
    # elements to be separated by comma
    comma_separated <- paste("    conn",
                             arg1, 
                             arg2, 
                             arg3,
                             sep =", \n")
    new_argument <- paste("%>%",
                          "add_argument(",
                          comma_separated,
                          ")", sep = "\n ")
       arguments <- paste(arguments, new_argument, collapse = "")
  }
  
  query <- paste("\n \n query_db(conn,\n", "         arguments,\n", 
                 "         target_vars = `default`,\n", 
                 "         target_table = 'observation_table',\n",
                 "         argument_relation = 'and')")
  
  # TODO: which code shall we provide?  
  Rcode <- cat(Rcode, set_up, arguments, query, sep=" ")
  
  } 
  
  return(Rcode)
}


# merge tables to get list of all publications, studies, and datasets
get_overview_df <- function(conn){
  pub <- dbReadTable(conn, "publication_table")
  stud <- dbReadTable(conn, "study_table")
  dat <- dbReadTable(conn, "dataset_table")
  task <- dbReadTable(conn, "task_table")
  
  overview_df <- pub %>%
    left_join(stud, by = "publication_id") %>%
    left_join(dat, by = "study_id") %>%
    left_join(task, by = "task_id") %>%
    select(publication_id, apa_reference, publication_code, study_id, study_comment,
           dataset_id, task_name, task_description)
  colnames(overview_df) <- c("Publication ID", "APA Refrence", "Publication Code",
                             "Study ID", "Study description", "Dataset ID",
                             "Task type", "Task description")
  
  return(overview_df)
}
  


