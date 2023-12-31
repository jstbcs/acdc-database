#' Get Overview Dataset Information
#'
#' This function retrieves an overview of dataset information from a database connection.
# It returns a data frame with summary information for each dataset, including dataset ID, publication code,
# author information, date conducted, task name, number of participants, number of trials, and indications of
# the presence of within and between manipulations.
#
#' @param conn A database connection object.
#' @param arguments A list of arguments to filter the dataset.
#' @param argument_relation A logical operator ('and' or 'or') for combining multiple filter conditions.
#
#' @return A data frame containing summary information for each dataset, indicating the presence of within
#' and between manipulations.
#' @export
#'
get_overview_information <- function(conn, arguments, argument_relation){
  # This function should output the dataframe ultimately displayed in
  # the shiny app under the "overview" tab
  query_results = query_db(
    conn = conn,
    arguments = arguments,
    argument_relation = argument_relation,
    target_table = "dataset_table",
    target_vars = c(
      # "study_id",
      # "publication_id",
      "dataset_id",
      "publication_code",
      "authors",
      "conducted",
      "task_name",
      "n_participants",
      "n_blocks",
      "n_trials",
      # "n_groups",
      # "n_tasks",
      # "task_id",
      # "task_description",
      # "keywords",
      # "added",
      "neutral_trials",
      "within_id",
      "number_within_conditions",
      "group_description"
      # "within_description",
      # "condition_id",
      # "group_description"
      # "mean_age",
      # "percentage_female"
    )
  )
  
  if(nrow(query_results) == 0) stop("There are no datasets that match your criteria.")


  query_results = dplyr::distinct(query_results, dataset_id, .keep_all = TRUE)
  
  query_results$within_id = c()

  query_results$short_author = NA
  for (i in 1:nrow(query_results)){
    query_results$short_author[i] = shorten_author_field(query_results$authors[i])
  }

  return(query_results)
}
