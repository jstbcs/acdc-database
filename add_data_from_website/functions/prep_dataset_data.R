prep_dataset_data <- function(dataset_data, raw_data){  
  dataset_data = clean_char_columns(dataset_data)

  raw_data_exp = remove_practice(raw_data)

  dataset_data$mean_dataset_rt = get_mean_rt(raw_data_exp)
  dataset_data$mean_dataset_acc = get_mean_acc(raw_data_exp)
  dataset_data$number_within_conditions = length(unique(raw_data_exp$within))

  return(dataset_data)
}