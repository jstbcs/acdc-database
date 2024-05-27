#' Get Database Information
#'
#' This function provides information about the structure of the database tables,
#' including the columns and their mandatory status.
# 
#' @return A list containing data frames with information about various tables in the database.
# Each data frame includes columns "column" (column names) and "mandatory" (mandatory status).
# 
#' @export
get_database_info <- function(){
  publication_table_columns <- c(
    "publication_id",
    "authors",
    "conducted",
    "added",
    "country",
    "contact",
    "keywords",
    "apa_reference",
    "publication_code"
  ) 
  
  publication_table_mandatory <- c(0, 0, 0, 0, 0, 0, 0, 1, 1)
  
  study_table_columns <- c(
    "study_id",
    "publication_id",
    "n_groups",
    "n_tasks",
    "study_comment"
  )
  
  study_table_mandatory <- c(0, 0, 1, 1, 0) # TODO: Switch comment back to mandatory 
  
  dataset_table_columns <- c(
    "study_id",
    "dataset_id",
    "task_id",
    "data_excl",
    "codebook",
    "n_participants",
    "n_blocks",
    "n_trials",
    "neutral_trials",
    "fixation_cross", 
    "time_limit",
    "github",
    "mean_dataset_rt",
    "mean_dataset_acc",
    "number_within_conditions",
    "group_description",
    "mean_age",
    "percentage_female",
    "n_members"
  )
  
  dataset_table_mandatory <- c(0, 0, 0, 0,
                               0, 0, 0, 0,
                               0, 0, 0, 0,
                               0, 0, 0, 0,
                               0, 0, 0)
  
  within_table_columns <- c(
    "within_id",
    "dataset_id",
    "within_description",
    "percentage_congruent",
    "percentage_neutral",
    "n_obs",
    "mean_obs_per_participant",
    "mean_condition_rt", 
    "mean_condition_acc"
  )
  
  within_table_mandatory <- c(0, 0, 1, 0,
                              0, 0, 0, 0, 
                              0)
  task_table_columns <- c(
    "task_id",
    "task_name",
    "task_description"
  )
  
  task_table_mandatory <- c(0, 1, 0)
  
  observation_table_columns <- c(
    "observation_id",
    "dataset_id",
    "subject",
    "block", 
    "trial",
    "within_id",
    "congruency",
    "accuracy",
    "rt"
  )
  
  observation_table_mandatory <- c(0, 0, 1, 1,
                                   1, 0, 
                                   1, 1, 1)
  
  measures_table_columns <- c(
    "measures_id",
    "study_id",
    "measure_name"
  )
  
  measures_table_mandatory <- c(1, 1, 1)
  
  table_info_db <- list(
    publication_table = data.frame(column = publication_table_columns, 
                                   mandatory = publication_table_mandatory),
    study_table = data.frame(column = study_table_columns,
                             mandatory = study_table_mandatory),
    dataset_table = data.frame(column = dataset_table_columns,
                               mandatory = dataset_table_mandatory),
    within_table = data.frame(column = within_table_columns,
                              mandatory = within_table_mandatory),
    observation_table = data.frame(column = observation_table_columns,
                                   mandatory = observation_table_mandatory),
    task_table = data.frame(column = task_table_columns,
                            mandatory = task_table_mandatory),
    measures_table = data.frame(column = measures_table_columns,
                                mandatory = measures_table_mandatory)
  )
  
  return(table_info_db)
}
