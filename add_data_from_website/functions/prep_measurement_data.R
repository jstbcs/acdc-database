prep_measurement_data <- function(measurement_data){
  measurement_data = clean_char_columns(measurement_data)
  return(measurement_data)
}
