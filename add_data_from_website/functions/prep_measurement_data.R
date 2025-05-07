prep_measurement_data <- function(measurement_data, db_overview){
  measurement_data <- clean_char_columns(measurement_data, db_overview, "measure_table")
  return(measurement_data)
}
