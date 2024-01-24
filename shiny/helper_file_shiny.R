# 
# Helper functions and vectors for inhibition task db shiny app 
# 
library(stringr)

# setting choice option names -------------------------------------------------- 
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

#publication_codes <- c("pratte_2010_exploring",
#               "mermet_2018_should ",
#               "hedge_2018_reliability",
#               "vonbastiaan_2015_evidence",
#               "whitehead_2020",
#               "tang_2022_dual",
#               "chetverikov_2017_blame",
#               "stahl_2014_behavioral",
#               "ebersole_2016_many")

get_pub_code <- function(){
  # query all datasets
  arguments <- list() %>% 
    add_argument(
      conn,
      "dataset_id",
      "greater",
      0
    )
  # get publication codes
  query_results <- query_db(conn,
                            arguments,
                            target_vars = 'publication_code',
                            target_table = 'observation_table',
                            argument_relation = 'and')
  
  return(unique(query_results$publication_code))
}


# function to choose default value of "value" fields ---------------------------
get_default_value <- function(criterion, operator){
  # only execute once operator has been chosen
  if(!is.null(operator)){
    default_value <- switch(criterion,
                            "mean_dataset_rt" = 0.55, 
                            "mean_dataset_acc" = 0.9, 
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

# set column names of data frames to be displayed to user -----------------------

# column names for suited_data_df
colnames_suited <- c("Dataset ID", "Publication Code", "Authors", "Conducted", "Task Type", 
                     "Sample Size", "Blocks per participant", "Trials per block", 
                     "Existence of neutral trials", "Within manipulation?", "Between manipulation?")

# column names for descriptives_df 
colnames_descriptives <- c("Dataset ID", "Publication ID", "Study ID", "Sample", "Task Type", 
                           "Sample Size", "Tials per block", "Mean number of trials per participant", 
                           "Percentage congruent", "Percentage of neutral trials", "Time limit (in ms)",
                           "Mean reaction time (dataset)", "Mean accuracy (dataset)", 
                           "Number of within conditions", "Within manipulation")

# function for server: merge existing lists  ------------------------------------
merge_lists <- function(x, y) {
  c(x, y)
}


# function to get overview_df or descriptive_df based on chosen arguments ---------
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
      # if no hits: return empty df 
      if(!is.data.frame(df)){
        df <- data.frame()
      } else {  # if hits, format output df 
        df <- df[, c(1,2,12,4,5,6,7,8,9,10,11)] # reorder columns and delete authors column 
        colnames(df) <- colnames_suited
        df[,9] <- ifelse(df[,9] == 0, FALSE, TRUE) # convert numeric input to logical
        df[,10] <- ifelse(df[,10] == 0, FALSE, TRUE)
        df[,11]<- ifelse(df[,11] == 0, FALSE, TRUE)
      }
      
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



# for data_id_for_plot choice: only show IDs of datasets that fit criteria -------
filtered_dataset_ids <- function(suited_overview_df){
  # input is reactive dataframe of suited datasets
  ids <- unique(suited_overview_df$dataset_id)
  
  return(ids)
}


# function to create R code based on chosen arguments ----------------------------
get_R_code <- function(argument_list){
  # R code for setup---------
  Rcode <- "if (!require('acdcquery')) install.packages('acdcquery') \nif (!require('dplyr')) install.packages('dplyr') \nlibrary(dpylr) \nlibrary(acdcquery)\n \n# create connection to SQL data base \nconn <- connect_to_db('acdc.db')\n"

  # query to get data at end of code ---
  query <- paste("\n \nquery_result <- query_db(conn,\n", 
                 "                        arguments,\n", 
                 "                        target_vars = 'default',\n", 
                 "                        target_table = 'observation_table',\n",
                 "                        argument_relation = 'and')")
  
  # once first argument has been chosen
  if(length(argument_list[[1]]) > 0){ 
    
  # install, load, connect to db -------
  set_up <- "\n# specify filter arguments\narguments <- list() %>%\n"
  
  # add first arguments without starting with "%>%" ------
  # convert value to character if necessary
  if(grepl("\\d", argument_list[[3]][1])) first_value=argument_list[[3]][1] else first_value=paste0("'",argument_list[[3]][1],"'")
  first_arguments <- paste0("   conn", ", \n", 
                           "    '", argument_list[[1]][1], "', \n",
                           "    '", argument_list[[2]][1], "', \n",
                           "    ", first_value)
  first_arg <- paste("add_argument(",
                     first_arguments, 
                     ")", sep = "\n ")
  
  # if more than 1 argument, add others ------
  if(length(argument_list[[1]]) > 1){
    arguments <- c()
    for(i in 2:length(argument_list[[1]])){
      # turn non-numeric elements into characters
      #arg1=paste0("'",argument_list[[1]][i],"'") 
      #arg2=paste0("'",argument_list[[2]][i],"'")
      if(grepl("\\d", argument_list[[3]][i])) arg3=argument_list[[3]][i] else arg3=paste0("'",argument_list[[3]][i],"'")
      
      # if between operator: print values as vector 
      if(argument_list[[2]][i] == "'between'") arg3=paste0("c(", str_split(arg3, "; ")[[1]][1], ",", str_split(arg3, "; ")[[1]][2], ")") else arg3=arg3
      # elements to be separated by comma
      comma_separated <- paste0("   conn", ", \n", 
                               "    '", argument_list[[1]][i], "', \n",
                               "    '", argument_list[[2]][i], "', \n",
                               "    ", arg3)
      new_argument <- paste("%>%",
                            "add_argument(",
                            comma_separated,
                            ")", sep = "\n ")
      arguments <- paste(arguments, new_argument, collapse = "")
    }
    
    # merge all 
    Rcode <- paste0(Rcode, set_up, first_arg, arguments, query)
  
  } else{ # if just 1 argument
    Rcode <- paste0(Rcode, set_up, first_arg, query)
    }
  } 
  
  return(Rcode)
}


# merge tables to get list of all publications, studies, and datasets ---------------
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
  

# download data  ---------------------------------------------------------------------
download_data <- function(ids, conn){
  arguments <- list() %>% 
    add_argument(
      conn,
      "dataset_id",
      "greater",
      0
    )
 
  query_results <- query_db(conn,
                     arguments,
                     target_vars = 'default',
                     target_table = 'observation_table',
                     argument_relation = 'and')
  
  filtered_results <- query_results %>%
    filter(dataset_id %in% ids)
  
  return(filtered_results)
}


# java script code for "show more" button in data table table in last tab -------------#
js <- "
function(cell) {
  var $cell = $(cell);
  $cell.contents().wrapAll('<div class=\\\"content\\\"></div>');
  var $content = $cell.find('.content');
  $cell.append($('<button>Read more</button>'));
  $btn = $cell.find('button');
  $content.css({
    height: '50px',
    overflow: 'hidden'
  });
  $cell.data('isLess', true);
  $btn.click(function () {
    var isLess = $cell.data('isLess');
    $content.css('height', isLess ? 'auto' : '50px');
    $(this).text(isLess ? 'Read less' : 'Read more');
    $cell.data('isLess', !isLess);
  });
}
"
