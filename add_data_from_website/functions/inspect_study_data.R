inspect_study_data <- function(submission_obj){
  n_studies = length(submission_obj$study_info)
  
  study_data = vector(mode = "list", length = n_studies)
  
  for (istudy in 1:n_studies){
    study_data[[istudy]] = submission_obj$study_info[[istudy]]$study_data
  }
  
  return(study_data)
}