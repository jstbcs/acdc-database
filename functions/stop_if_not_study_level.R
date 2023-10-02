stop_if_not_study_level <- function(object){
  if(do_elements_exist(c("study_table", "between_table", "data1"), object) == FALSE)
  {
    stop("This function takes a study-level object")
  }
}