prep_submission_data <- function(conn, submission_obj){
  overview_table = generate_db_overview_table(conn)
  
  submission_obj$publication_data = prep_publication_data(submission_obj$publication_data, overview_table)
  
  n_statementsets = length(submission_obj$statementset_info)
  if (n_statementsets > 0){
    publication_refs = character(n_statementsets)
    for (istatementset in 1:n_statementsets){
      publication_refs[istatementset] = submission_obj$statementset_info[[istatementset]]$publication
      
      submission_obj$statementset_info[[istatementset]]$statementset_data = prep_statementset_data(submission_obj$statementset_info[[istatementset]]$statementset_data, overview_table)
    }
    
    # If there are statementsets linked to the same publication but multiple entries
    # Suffix the study number to them so they don't mess up the anti-duplication system
    if (any(duplicated(tolower(trimws(publication_refs))))){
      duplicated_refs = unique(publication_refs[which(duplicated(tolower(trimws(publication_refs))))])
      publication_refs[which(publication_refs %in% duplicated_refs)] = paste0(publication_refs[which(publication_refs %in% duplicated_refs)], " - Study ", which(publication_refs %in% duplicated_refs))
    }
    
    for (istatementset in 1:n_statementsets){
      submission_obj$statementset_info[[istatementset]]$publication = publication_refs[istatementset]
    }
  }
  
  n_studies = length(submission_obj$study_info)
  for (istudy in 1:n_studies){
    submission_obj$study_info[[istudy]]$study_data = prep_study_data(submission_obj$study_info[[istudy]]$study_data, overview_table)
    
    if ("within_data" %in% names(submission_obj$study_info[[istudy]])){
      submission_obj$study_info[[istudy]]$within_data = prep_within_data(submission_obj$study_info[[istudy]]$within_data, overview_table)
    }
    if ("between_data" %in% names(submission_obj$study_info[[istudy]])){
      submission_obj$study_info[[istudy]]$between_data = prep_between_data(submission_obj$study_info[[istudy]]$between_data, overview_table)
    }
    submission_obj$study_info[[istudy]]$procedure_data = prep_repetition_data(submission_obj$study_info[[istudy]]$repetition_data, overview_table)
    submission_obj$study_info[[istudy]]$raw_data = prep_raw_data(submission_obj$study_info[[istudy]]$raw_data, overview_table)
    
    if ("measurement_data" %in% names(submission_obj$study_info[[istudy]])){
      submission_obj$study_info[[istudy]]$measurement_data = prep_measurement_data(submission_obj$study_info[[istudy]]$measurement_data, overview_table)
    }
  }
  
  return(submission_obj)
}
