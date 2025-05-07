inspect_condition_data <- function(submission_obj){
  n_studies = length(submission_obj$study_info)
  
  condition_data = vector(mode = "list", length = n_studies)
  
  for (istudy in 1:n_studies){
    condition_data[[istudy]]$between_data = ifelse(
      is.null(submission_obj$study_info[[istudy]]$between_data), 
      "none", 
      submission_obj$study_info[[istudy]]$between_data
    )
    condition_data[[istudy]]$within_data = ifelse(
      is.null(submission_obj$study_info[[istudy]]$within_data), 
      "none", 
      submission_obj$study_info[[istudy]]$within_data
      )
    condition_data[[istudy]]$repetition_data = submission_obj$study_info[[istudy]]$repetition_data
  }
  
  return(condition_data)
}