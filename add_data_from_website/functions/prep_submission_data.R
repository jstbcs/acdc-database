prep_submission_data <- function(object){
  submission_obj = object$publication_1

  overview_table = get_database_info()
  
  submission_obj$publication_table = prep_publication_data(submission_obj$publication_table, overview_table)
  
  
  n_studies = sum(grepl(names(submission_obj), pattern = "study_"))
  
  for (istudy in 1:n_studies){
    submission_obj[[paste0("study_", istudy)]]$study_table = prep_study_data(submission_obj[[paste0("study_", istudy)]]$study_table, overview_table)
    
    if ("measurement_table" %in% names(submission_obj[[paste0("study_", istudy)]])){
      submission_obj[[paste0("study_", istudy)]]$measurement_table = prep_measurement_data(submission_obj[[paste0("study_", istudy)]]$measurement_table, overview_table)
    }

    n_datasets = sum(grepl(names(submission_obj[[paste0("study_", istudy)]]), pattern = "data_"))

    for (idata in 1:n_datasets){
      submission_obj[[paste0("study_", istudy)]][[paste0("data_", idata)]]$task_table = prep_task_data(submission_obj[[paste0("study_", istudy)]][[paste0("data_", idata)]]$task_table, overview_table)
      submission_obj[[paste0("study_", istudy)]][[paste0("data_", idata)]]$dataset_table = prep_dataset_data(submission_obj[[paste0("study_", istudy)]][[paste0("data_", idata)]]$dataset_table, overview_table)
      if ("within_table" %in% names(submission_obj[[paste0("study_", istudy)]][[paste0("data_", idata)]])){
        submission_obj[[paste0("study_", istudy)]][[paste0("data_", idata)]]$within_table = prep_within_data(submission_obj[[paste0("study_", istudy)]][[paste0("data_", idata)]]$within_table, overview_table)
      }
      submission_obj[[paste0("study_", istudy)]][[paste0("data_", idata)]]$observation_table = prep_raw_data(submission_obj[[paste0("study_", istudy)]][[paste0("data_", idata)]]$observation_table, overview_table)
    }
  }
  
  object$publication_1 = submission_obj
  return(object)
}
