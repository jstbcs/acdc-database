inspect_study_data <- function(submission_obj){
  n_studies = sum(grepl(names(submission_obj$publication_1), pattern = "study_"))
  
  study_data = vector(mode = "list", length = n_studies)
  
  for (istudy in 1:n_studies){
    study_data[[istudy]] = submission_obj$publication_1[[paste0("study_", istudy)]]$study_table
  }
  
  return(study_data)
}
