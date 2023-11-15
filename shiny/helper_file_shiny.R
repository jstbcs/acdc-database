# 
# Helper functions and vectors for inhibition task db shiny app 
# 


# vector storing criteria to be chosen
criteria <- c("Task type(s)" = "task_name",
              #"Mean reaction time (in ms)" = "mean_dataset_rt",
              #"Mean accuracy" = "mean_dataset_acc",
              "Number of participants" = "n_participants",
              #"Number of blocks per participant" = ,
              "Number of trials per block" = "n_trials",
              #"Neutral stimuli included?" = ,
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
                            "mean_dataset_rt" = 700, 
                            "mean_dataset_acc" = 0.8, 
                            "n_participants" = 100, 
                            #"Number of blocks per participant" = 5, 
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
colnames_suited <- c("Publication Code", "Authors", "Conducted", "Dataset ID", 
                     "Between person manipulation", "Within person manipulation",
                     "Sample size", "Blocks per participant", "Trials per block")

# column names for descriptives_df 
colnames_descriptives <- c("Dataset ID", "Mean number of trials per participant", 
                           "Percentage congruent", "Mean reaction time", 
                           "Mean accuracy", "Number of conditions", "Time limit (in ms)",
                           "Data exclusion criteria")

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
      arguments <- arguments %>%
        add_argument(
          conn, 
          argument_list[[1]][i], 
          argument_list[[2]][i], 
          argument_list[[3]][i]
        )
    }
    # get chosen data frame 
    if(type == "overview"){
      df <- get_overview_information(conn, arguments, "and")
    } else if (type == "descriptives"){
      df <- get_descriptive_information(conn, arguments, "and")
    } else if (type == "detailed"){
      df <- get_detailed_information(conn, arguments, "and")
    }
    
  } else { # when no arguments are chosen 
    df <- data.frame()
  }
  return(df)
}


# function to create R code based on chosen arguments
get_R_code <- function(argument_list){
  if(length(argument_list[[1]]) > 0){ # once first argument has been chosen
    
  # install, load, connect to db -------
  set_up <- "if (!require('acdcquery')) install.packages('acdcquery') \n library(acdcquery) \n # create connection to SQL data base \nconn <- connect_to_db('acdc.db')\n # specify filter arguments\n arguments <- list() \n"
  
  # modify arguments based on user input ----
  arguments <- c()
  for(i in 1:length(argument_list[[1]])){
    # elements to be separated by comma
    comma_separated <- paste("conn",
                             argument_list[[1]][i], 
                             argument_list[[2]][i], 
                             argument_list[[3]][i],
                             sep =", \n ")
    new_argument <- paste("%>%",
                          "add_argument(",
                          comma_separated,
                          ")", sep = "\n ")
    arguments <- paste(arguments, new_argument, collapse = "")
  }
  # TODO: which code shall we provide?  
  Rcode <- cat(set_up, arguments, sep=" ")
  
  } else { # if no argument chosen yet
    Rcode <- "if (!require('acdcquery')) install.packages('acdcquery') \n
    library(acdcquery) \n 
    # create connection to SQL data base \n 
    conn <- connect_to_db('acdc.db')"
  }
  
  return(Rcode)
}


  
  
  


