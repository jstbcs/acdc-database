inspect_raw_data <- function(submission_obj){
  n_studies = sum(grepl(names(submission_obj$publication_1), pattern = "study_"))
  
  raw_data = vector(mode = "list", length = n_studies)
  
  for (istudy in 1:n_studies){
    n_datasets = sum(grepl(names(submission_obj$publication_1[[paste0("study_", istudy)]]), pattern = "data_"))
    
    raw_data[[istudy]] = vector(mode = "list", length = n_datasets)
    for (idata in 1:n_datasets){
      raw_data[[istudy]][[idata]] = submission_obj$publication_1[[paste0("study_", istudy)]][[paste0("data_", idata)]]$observation_table
    }
  }

  return(raw_data)
}
