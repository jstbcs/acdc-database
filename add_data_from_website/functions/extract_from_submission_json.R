extract_from_submission_json <- function(json_path){
  json_obj = jsonlite::read_json(json_path)
  
  # json_obj = replace_999_with_na(json_obj)
  
  submission_list = list()
  
  submission_list$publication_table = as.data.frame(json_obj$publication_data)
  
  n_tasks = length(json_obj$task_info)
  
  if (n_tasks > 0) {
    if (length(json_obj$task_info[[1]]) > 0){
      submission_list$task_info = vector(mode = "list", n_tasks)
      for (itask in 1:n_tasks){
        submission_list$task_info[[itask]]$task_data = as.data.frame(json_obj$task_info[[itask]]$task_info$task_data)
      }
    }
  } else {
    n_tasks = 0
  }
  
  n_studies = length(json_obj$study_info)
  
  for (istudy in 1:n_studies){
    study_info = list()
  
    study_info$study_table = as.data.frame(json_obj$study_info[[istudy]]$study_data)
   
    # Deal with submitted additional measures
    if (json_obj$study_info[[istudy]]$measurement_data$additional_measures == "1"){
      study_info$measures_table= as.data.frame(data.table::rbindlist(json_obj$study_info[[istudy]]$measurement_data$measures))
    } else {
      study_info$measures_table = data.frame(
        name = "no additional measures",
        identifier = 1
      )
    }
    
    n_datasets = length(json_obj$study_info[[istudy]]$dataset_info)
    
    for (idata in 1:n_datasets){
      data_info = list()
      
      # Extract the task name
      task_name_used = json_obj$study_info[[istudy]]$dataset_info[[idata]]$dataset_data$task_name
      
      # Retrieve ID number
      task_id_used = as.numeric(grep("\\d+$", task_name_used))
      
      data_info$task_table = submission_list$task_info[[task_id_used]]$task_data
      
      data_info$dataset_table = as.data.frame(json_obj$study_info[[istudy]]$dataset_info[[idata]]$dataset_data)
      
      if (json_obj$study_info[[istudy]]$dataset_info[[idata]]$within_data$has_within_conditions == "1"){
        data_info$within_table = as.data.frame(data.table::rbindlist(json_obj$study_info[[istudy]]$dataset_info[[idata]]$within_data$within_condition_details))
      } else {
        data_info$within_table = data.frame(
          name = "no within manipulation",
          identifier = 1
        )
      }
      
      data_info$observation_table = as.data.frame(data.table::rbindlist(json_obj$study_info[[istudy]]$dataset_info[[idata]]$raw_data))

      if (json_obj$study_info[[istudy]]$dataset_info[[idata]]$within_data$has_within_conditions == "0"){
        data_info$observation_table$within = 1
      }
      
      study_info[[paste0("data_", idata)]] = data_info
    }
    
    submission_list[[paste0("study_", istudy)]] = study_info
  }
  
  object = list()
  object$publication_1= submission_list
  
  return(object)
}
