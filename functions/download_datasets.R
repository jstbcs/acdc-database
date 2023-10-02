#' Download Datasets and Organize into a List
#'
#' This function downloads datasets from various sources and organizes them into a list of data frames. 
# The data sources can include uploaded data, links to GitHub, or links to OSF, among others.
# 
#' @param entry A data frame containing information about how to access the data, including choices like "Upload data to this form" and "Submit link to OSF/Github/other."
#' @param download_folder The folder where downloaded data files will be stored.
# 
#' @return A list of data frames, where each data frame contains a downloaded dataset. The list is organized based on the information provided in the 'entry' data frame.
#' 
#' @export
# main function: download datasets, assign correct name and add to list --------
download_datasets <- function(entry, download_folder){
  # step 1: Get non-entry choices of how to provide data access
  datachoice_cols <- which(grepl("How", colnames(entry)))
  download_links <- entry %>%
    select(datachoice_cols[which(!is.na(entry[1, datachoice_cols]))])
  
  # step 2: append respective download link to each column of download_links df
  for(col in 1:ncol(download_links)){
    if(download_links[1,col] == "Upload data to this form"){
      # note: link to uploaded link is in column directly after choice column
      colnumber <- which(colnames(entry) == colnames(download_links)[col]) # find choice col index
      download_links[2,col] <- entry[1, colnumber + 1] 
    } else if(download_links[1,col] == "Submit link to OSF/ Github/ other"){
      # note: link to data is 2 columns next to choice column
      colnumber <- which(colnames(entry) == colnames(download_links)[col]) 
      download_links[2, col] <- entry[1, colnumber + 2] 
    }
  }
  
  # list will store all datasets
  dataframe_list <- list()
  
  # download each dataset 
  for(i in 1:ncol(download_links)){
    # define correct file name for saving 
    filepath <- paste(download_folder, "/data",i,".csv", sep="")
    
    # download data to local machine 
    # if data was uploaded to our website
    if(download_links[1, i] == "Upload data to this form"){
      download.file(download_links[2, i], filepath)
      # read data into list object, delete first row 
      dataframe_list[[i]] <- read.csv(filepath)
      dataframe_list[[i]] <- dataframe_list[[i]][, 2:ncol(dataframe_list[[i]])]
      # find correct name and assign to dataframe 
      data_name <- find_dataname(download_links, col = i)
      names(dataframe_list)[i] <- data_name 
      
      # if link to raw data on github  
    } else if(grepl("raw.githubusercontent.com", download_links[2, i])){
      dataframe_list[[i]] <- read_csv(url(download_links[2, i]))
      # find correct name and assign to dataframe 
      data_name <- find_dataname(download_links, col = i)
      names(dataframe_list)[i] <- data_name 
      
      # if download link to osf data
    } else if(grepl("osf.io", download_links[2, i]) & grepl("/download", download_links[2,i])){
      data <- read.csv(download_links[2,i], header=TRUE, na.strings="NA")
      if(ncol(data) == 1) {  # check if csv2 required instead
        data <- read.csv2(download_links[2,i], header=TRUE, na.strings="NA")
      }
      dataframe_list[[i]] <- data
      # find correct name and assign to dataframe 
      data_name <- find_dataname(download_links, col = i)
      names(dataframe_list)[i] <- data_name 
      
    } else {
      # if provided link cannot be downloaded automatically into R, give warning
      warning(paste("The following link to data for", names(download_links)[i], "can not be downloaded automatically.
                    Please check the following link:", download_links[2, i], "and download the data manually.", sep=" "))
    }
  }
  
  return(dataframe_list)
}