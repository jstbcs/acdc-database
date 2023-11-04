#' Get Detailed Dataset Information
#'
#' This function retrieves detailed information about a single dataset from a database connection.
# It returns a list containing dataset-specific information, including dataset ID, within and between manipulations,
# task name, number of participants, number of trials, mean observations per participant, percentage congruent trials,
# percentage neutral trials, time limit, mean dataset-level reaction time (RT), and mean dataset-level accuracy (acc).
# It also includes histograms for RT distributions within different conditions and the raw trial-level data.
#
#' @param conn A database connection object.
#' @param arguments A list of arguments to filter the dataset.
#' @param argument_relation A logical operator ('AND' or 'OR') for combining multiple filter conditions.
#
#' @return A list containing detailed information specific to the dataset, including information about the dataset itself,
#' histograms of RT distributions within different conditions, and the raw trial-level data.
#' @export
get_detailed_information <- function(conn, arguments, argument_relation){
  # This function should get the data displayed when inspecting single dataset-ids.
  query_results_dataset = query_db(
    conn = conn,
    arguments = arguments,
    argument_relation = argument_relation,
    target_table = "dataset_table",
    target_vars = c(
      "dataset_id",
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
      "mean_condition_rt",
      "mean_condition_acc",
      "within_id",
      "between_id",
      "condition_id"
    )
  )

  query_results_trial = query_db(
    conn = conn,
    arguments = arguments,
    argument_relation = argument_relation,
    target_table = "observation_table",
    target_vars = c(
      "rt",
      "accuracy",
      "dataset_id",
      "condition_id",
      "subject"
    )
  )

  # condition_ids = unique(query_results_dataset$condition_id)
  # 
  # n_conditions = length(condition_ids)
  # plots = vector(mode = "list", length = n_conditions)
  # for (i in 1:n_conditions){
  #   plots[[i]] = hist(query_results_trial$rt[query_results_trial$rt < 2 & query_results_trial$condition_id == condition_ids[i]],
  #                     breaks = 40,
  #                     plot = FALSE)
  # }

  query_results = list()
  query_results$data = query_results_dataset
  # query_results$plots = plots
  query_results$trial_data = query_results_trial

  return(query_results)
}
