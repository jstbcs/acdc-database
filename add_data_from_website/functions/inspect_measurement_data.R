inspect_measurement_data <- function(submission_obj){
  n_studies = sum(grepl(names(submission_obj$publication_1), pattern = "study_"))
  
  measurement_data = vector(mode = "list", length = n_studies)
  
  for (istudy in 1:n_studies){
    measurement_data[[istudy]] = submission_obj$publication_1[[paste0("study_", istudy)]]$measurement_table
  }

  return(measurement_data)
}
