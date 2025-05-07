extract_from_submission_json <- function(json_path){
  json_obj = jsonlite::read_json(json_path)
  
  json_obj = replace_999_with_na(json_obj)
  
  submission_list = list()
  
  submission_list$publication_data = as.data.frame(json_obj$publication_data)
  
  n_statementsets = length(json_obj$statementset_info)
  
  if (n_statementsets > 0) {
    if (length(json_obj$statementset_info[[1]]) > 0){
      submission_list$statementset_info = vector(mode = "list", n_statementsets)
      for (istatementset in 1:n_statementsets){
        submission_list$statementset_info[[istatementset]]$publication = json_obj$statementset_info[[istatementset]]$statementset_publication
        
        if (trimws(submission_list$statementset_info[[istatementset]]$publication) == ""){
          submission_list$statementset_info[[istatementset]]$publication = submission_list$publication_data$apa_reference
        }
        
        submission_list$statementset_info[[istatementset]]$statementset_data = as.data.frame(data.table::rbindlist(json_obj$statementset_info[[istatementset]]$statementset_data))
        
        columns_to_extract = c("statement_identifier", "statement_text", "statement_accuracy", "statement_category", "proportion_true")
        submission_list$statementset_info[[istatementset]]$statementset_data = dplyr::select(submission_list$statementset_info[[istatementset]]$statementset_data, dplyr::any_of(columns_to_extract))
      }
    }
  } else {
    n_statementsets = 0
  }
  
  n_studies = length(json_obj$study_info)
  
  submission_list$study_info = vector(mode = "list", length = n_studies)
  for (istudy in 1:n_studies){
    submission_list$study_info[[istudy]]$study_data = as.data.frame(json_obj$study_info[[istudy]]$study_data)
    submission_list$study_info[[istudy]]$repetition_data = as.data.frame(data.table::rbindlist(json_obj$study_info[[istudy]]$repetition_data))

    columns_to_extract = c("subject", "presentation_identifier", "trial", "response", "repeated")
    
    # Here check if has conditions
    if (json_obj$study_info[[istudy]]$condition_data$has_within_conditions == "1"){
      submission_list$study_info[[istudy]]$within_data = as.data.frame(data.table::rbindlist(json_obj$study_info[[istudy]]$condition_data$within_condition_details))
      columns_to_extract = c(columns_to_extract, "within_identifier")
    }
    
    if (json_obj$study_info[[istudy]]$condition_data$has_between_conditions == "1"){
      submission_list$study_info[[istudy]]$between_data = as.data.frame(data.table::rbindlist(json_obj$study_info[[istudy]]$condition_data$between_condition_details))
      columns_to_extract = c(columns_to_extract, "between_identifier")
    }
    
    # Here extract only necessary columns from the raw data
    submission_list$study_info[[istudy]]$raw_data = as.data.frame(data.table::rbindlist(json_obj$study_info[[istudy]]$raw_data))
    
    # Add columns to extract
    if (json_obj$study_info[[istudy]]$study_data$statementset_name != "no information"){
      columns_to_extract = c(columns_to_extract, "statement_identifier")
    }
    
    if (json_obj$study_info[[istudy]]$study_data$rt_measured == "1"){
      columns_to_extract = c(columns_to_extract, "rt")
    }
    
    if (json_obj$study_info[[istudy]]$study_data$subjective_certainty == "1"){
      columns_to_extract = c(columns_to_extract, "certainty")
    }
    
    submission_list$study_info[[istudy]]$raw_data = as.data.frame(submission_list$study_info[[istudy]]$raw_data)[, columns_to_extract]
    
    # Deal with submitted additional measures
    if (json_obj$study_info[[istudy]]$measurement_data$additional_measures == "1"){
      submission_list$study_info[[istudy]]$measurement_data = as.data.frame(data.table::rbindlist(json_obj$study_info[[istudy]]$measurement_data$measures))
    }
  }
  
  return(submission_list)
}
