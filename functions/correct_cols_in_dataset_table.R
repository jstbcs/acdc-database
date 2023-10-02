
# This function checks whether all optional columns of the dataset_table table 
# are provided; since none are mandatory, no error occurs but warning is given
# and input from user is required
correct_cols_in_dataset_table <- function(dataset_table){
  colnames = colnames(dataset_table)
  
  # check if object is data frame 
  if(!is.data.frame(dataset_table)){
    stop("dataset_table must be a dataframe")
  }
  # check if object contains more than 1 entry
  if(nrow(dataset_table) > 1){
    stop("The dataset_table data frame can only contain 1 row")
  }
  
  confirm_object_names(dataset_table, entry_list_info$dataset_table)
}