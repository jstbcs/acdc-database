#' Add Overview Table
#'
#' This function adds the overview table to the database, provided that the essential variables such as 'study_id' and 'task_id' are present in the overview data. The function checks if these variables exist and then injects the relevant columns into the 'data_overview' table.
#'
#' @param conn The connection object or database connection string.
#' @param object The object containing overview data.
#'
#' @export
add_overview_table <- function(conn, object){
  stop_if_not_data_level(object)
  if (do_elements_exist(c("study_id", "task_id"), object$overview) == FALSE)
  {
    stop("Variables 'study_id' and 'task_id' need to be present in overview")
  }
  overview_columns = c("study_id", "task_id", "keywords", "data_exclusions", "codebook", 
                       "n_participants", "n_blocks", "n_trials", 
                       "mean_effect", "sd_effect", "neutral_trials",
                       "percentage_incongruent", "feedback",
                       "fixation_cross", "time_limit",
                       "mean_age", "percentage_female", "percentage_male"
  )
  overview_inject = object$overview[which_elements_exist(overview_columns, object$overview)]
  dbWriteTable(
    conn = conn,
    name = "data_overview",
    value = overview_inject,
    append = TRUE
  )
}