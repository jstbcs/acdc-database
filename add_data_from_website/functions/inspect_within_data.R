inspect_within_data <- function(submission_obj) {
  n_studies = sum(grepl(names(submission_obj$publication_1), pattern = "study_"))
  
  within_data = vector(mode = "list", length = n_studies)
  
  for (istudy in 1:n_studies){
    n_datasets = sum(grepl(names(submission_obj$publication_1[[paste0("study_", istudy)]]), pattern = "data_"))
    
    within_data[[istudy]] = vector(mode = "list", length = n_datasets)
    for (idata in 1:n_datasets){
      within_data[[istudy]][[idata]] = submission_obj$publication_1[[paste0("study_", istudy)]][[paste0("data_", idata)]]$within_table
    }
  }
  return(within_data)
}
