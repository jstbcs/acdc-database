#' Check Details for Appending Data to Database
#'
#' This function validates the details provided for appending data to the database.
#'
#' @param details A character string specifying the details for appending data. It can be either a publication code or a study ID.
#'
#' @return This function ensures that the provided details are either a publication code (matching a specific pattern) or a numeric study ID. If the details do not match these criteria, it raises an error with an appropriate message.
#'
#' @export
check_append_details <- function(details){
  regex_matches_publication_code = get_appropriate_regex_pattern("publication_code")
  if (
    stringr::str_detect(details, regex_matches_publication_code) == FALSE &
    stringr::str_detect(details, "^\\d+$") == FALSE
  ){
    stop("Details need to be either a publication code or a study-id that a dataset should be added to.")
  }
}