prep_within_data <- function(within_data, db_overview){
  within_data <- clean_char_columns(within_data, db_overview, "within_table")
  
  return(within_data)
}