#' Find a Suitable Name for a Data Frame
#'
#' This function determines a suitable name for a data frame based on the provided download links.
#'
#' @param download_links A data frame containing download links.
#' @param col The column index specifying the download link.
#'
#' @return A character string representing a suitable name for the data frame.
#'
#' @details This function generates a name for a data frame based on the provided download links.
#' The naming pattern depends on the structure of the download links.
#'
#' @export
find_dataname <- function(download_links, col){
  # pattern type: only 1 task in fist study 
  if(colnames(entry[which(entry == download_links[2, col])]) == "Upload.data" |
     colnames(entry[which(entry == download_links[2, col])]) == "Link.to.data"){
    data_name <- "processed_data_study1"
    
  } else if(grepl("in", colnames(download_links)[col])){
    # pattern type: includes both study number and task number 
    split_name <- unlist(strsplit(colnames(download_links)[col], "[.]"))
    task_number <- split_name[10]
    study_number <- split_name[length(split_name)]
    data_name <- paste("processed_data_study",study_number,"_task",task_number, sep="")
    
  } else if(grepl("Upload.data.for.task.", colnames(entry[which(entry == download_links[2, col])])) |
            grepl("Link.to.data.for.task.", colnames(entry[which(entry == download_links[2, col])]))) {
    # name pattern if only one study submitted
    split_name <- unlist(strsplit(colnames(entry[which(entry == download_links[2, col])]), "[.]"))
    task_number <- split_name[length(split_name)]
    data_name <- paste("processed_data_study1_task", task_number, sep="")
    
  } else if(grepl("How.would.you.like.to.submit.data.for.study.", colnames(download_links)[col])){
    # name pattern if only one task in study 2-5
    split_name <- unlist(strsplit(colnames(download_links)[col], "[.]"))
    study_number <- split_name[length(split_name)]
    data_name <- paste("processed_data_study", study_number, sep="")
    
  } else {
    data_name <- "NAME MANUALLY"
    warning(paste("Assigning data frame names failed for data listed in column ", 
                  colnames(entry[which(entry == download_links[2, col])]), 
                  ". Please name the data set manually following the naming conventions"))
  }
  return(data_name)
}