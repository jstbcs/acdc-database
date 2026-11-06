#' Check for duplicate observations
#'
#' Verifies that each combination of subject, block, trial, and within occurs
#' at most once in the observation table.
#'
#' @param observation_table A data frame containing the columns subject, block,
#'   trial, and within.
#'
#' @details Missing values are treated as equal when identifying duplicate
#'   combinations. An error is raised if the input is not a data frame, required
#'   columns are missing, or duplicate combinations are found.
#'
#' @return Invisibly returns NULL if validation succeeds.
#'
#' @export
check_duplicates_in_observation_table <- function(observation_table) {
  if (!is.data.frame(observation_table)) {
    stop("`observation_table` must be a data frame.", call. = FALSE)
  }
  
  required_columns <- c("subject", "block", "trial", "within")
  missing_columns <- setdiff(required_columns, names(observation_table))
  
  if (length(missing_columns) > 0L) {
    stop(
      "Missing required columns in `observation_table`: ",
      paste(missing_columns, collapse = ", "),
      ".",
      call. = FALSE
    )
  }
  
  duplicates <- observation_table |>
    dplyr::count(
      subject, block, trial, within,
      name = "occurrences",
      .drop = TRUE
    ) |>
    dplyr::filter(occurrences > 1L)
  
  if (nrow(duplicates) > 0L) {
    preview <- utils::head(as.data.frame(duplicates), 10L)
    preview_text <- paste(
      utils::capture.output(print(preview, row.names = FALSE)),
      collapse = "\n"
    )
    
    stop(
      "Each combination of `subject`, `block`, `trial`, and `within` ",
      "must occur at most once in `observation_table`.\n",
      "Found ", nrow(duplicates), " duplicate combination(s).\n",
      "Duplicate combinations and their occurrence counts",
      if (nrow(duplicates) > 10L) " (first 10 shown)" else "",
      ":\n",
      preview_text,
      call. = FALSE
    )
  }
  
  invisible(NULL)
}