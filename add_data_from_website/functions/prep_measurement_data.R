prep_measurement_data <- function(measurement_data){
  measurement_data = clean_char_columns(measurement_data)

  measurement_data$measurement_name = measurement_data$name
  measurement_data$measurement_identifier = measurement_data$identifier

  return(measurement_data)
}
