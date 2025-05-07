inspect_raw_data <- function(submission_obj){
  n_studies = length(submission_obj$study_info)
  
  raw_data = vector(mode = "list", length = n_studies)
  
  for (istudy in 1:n_studies){
    raw_data[[istudy]] = submission_obj$study_info[[istudy]]$raw_data
  }
  
  return(raw_data)
}