prep_raw_data <- function(raw_data){
  raw_data[grepl("^true$", raw_data, ignore.case = TRUE)] = 1
  raw_data[grepl("^false$", raw_data, ignore.case = TRUE)] = 0
  raw_data[grepl("^yes$", raw_data, ignore.case = TRUE)] = 1
  raw_data[grepl("^no$", raw_data, ignore.case = TRUE)] = 0
  
  raw_data[raw_data == TRUE] = 1
  raw_data[raw_data == FALSE] = 0
  
  raw_data = dplyr::select(raw_data, dplyr::all_of(
    c("subject","block", "trial",	"within_identifier",
      "congruency",	"accuracy",	"rt")))
  
  clean_raw_data = clean_char_columns(raw_data)

  clean_raw_data = raw_data |>
    dplyr::mutate(
      dplyr::across(dplyr::any_of(c("rt", "accuracy")), ~as.numeric(sub(",", ".", ., fixed = TRUE)))
    )
  
  if ("rt" %in% colnames(clean_raw_data)){
    clean_raw_data$rt = ifelse(clean_raw_data$rt > 30, clean_raw_data$rt / 1000, clean_raw_data$rt)
    
    clean_raw_data$rt = ifelse(clean_raw_data$rt > 30, NA, clean_raw_data$rt)
  }
  
  # To make sure it is also character, like within_data might sometimes contain
  clean_raw_data$within_identifier = as.character(clean_raw_data$within_identifier)
  
  # Some functions need one, some the other
  clean_raw_data$within = as.character(clean_raw_data$within_identifier)
  
  return(clean_raw_data)
}
