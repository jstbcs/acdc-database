prep_between_data <- function(between_data, db_overview){
  between_data = clean_char_columns(between_data, db_overview, "between_table")
  
  return(between_data)
}