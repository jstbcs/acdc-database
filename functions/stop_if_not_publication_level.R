stop_if_not_publication_level <- function(object){
  if (!all(str_detect(colnames(object), regex_matches_study_names))){
    stop("This function takes a publication-level object")
  }
}