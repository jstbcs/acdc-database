prep_publication_data <- function(publication_data, db_overview){
  # Generate the publication code
  publication_data$publication_code = tolower(paste0(
    publication_data$first_author, "_",
    publication_data$conducted, "_", 
    strsplit(publication_data$title, " ")[[1]][1]
  ))
  
  clean_publication_data = clean_char_columns(publication_data, db_overview, "publication_table")
  
  return (clean_publication_data)
}
