check_append_details <- function(details){
  if (
    stringr::str_detect(details, regex_matches_publication_code) == FALSE &
    stringr::str_detect(details, "^\\d+$") == FALSE
  ){
    stop("Details need to be either a publication code or a study-id that a dataset should be added to.")
  }
}