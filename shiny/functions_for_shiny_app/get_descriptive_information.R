#' Get Descriptive Information
#'
#' This function retrieves detailed information about datasets from a database connection.
# It returns a data frame with a range of information related to the datasets, such as dataset IDs,
# within and between manipulations, task name, number of participants, number of trials,
# mean observations per participant, percentage congruent trials, percentage neutral trials,
# time limit, mean reaction time (RT), and mean accuracy (acc).
#
#' @param conn A database connection object.
#' @param arguments A list of arguments to filter the results.
#' @param argument_relation A logical operator ('AND' or 'OR') for combining multiple filter conditions.
#
#' @return A data frame containing detailed information about datasets, including the specified parameters.

#' @export
get_descriptive_information <- function(conn, arguments, argument_relation){
  # This function should return the dataframe displayed in the
  # tab displaying more detailed information

  query_results = query_db(
    conn = conn,
    arguments = arguments,
    argument_relation = argument_relation,
    target_table = "dataset_table",
    target_vars = c(
      "dataset_id",
      "publication_id",
      "study_id",
      "within_id",
      "within_description",
      "group_description",
      "task_name",
      "n_participants",
      "n_trials",
      "mean_obs_per_participant",
      "percentage_congruent",
      "percentage_neutral",
      "time_limit",
      "mean_dataset_rt",
      "mean_dataset_acc",
      "number_within_conditions"
    )
  )

  # Turn duplications of within/between_id into one row per dataset
  unique_dataset_ids = unique(query_results$dataset_id)

  query_results$within_manipulation = NA

  for (id in unique_dataset_ids){
    if (length(unique(query_results$within_id)) > 1){
      within_manipulations = paste(
        unique(query_results[which(query_results$dataset_id == id), "within_description"]),
        collapse = "; "
      )
    } else {
      within_manipulations = "none"
    }

    query_results[which(query_results$dataset_id == id), "within_manipulation"] = within_manipulations
  }

  query_results = dplyr::distinct(query_results, dataset_id, .keep_all = TRUE)
  query_results$within_id = c()
  query_results$within_description = c()
  return(query_results)
}
