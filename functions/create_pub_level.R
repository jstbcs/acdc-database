#' Create a publication level entry for inserting data into the database.
#'
#' This function creates a publication level entry for inserting data into the database, based on the provided information. It generates a publication table with the required columns, including authors, year conducted, date added, country, contact information, keywords, and APA reference.
# 
#' @param entry A nested list containing the information required for the publication level.
# 
#' @return A list containing the publication level entry for database insertion.
# 
#' @details The 'create_pub_level' function generates a publication table entry for insertion into the database. It extracts information from the provided nested list and creates a publication table with the required columns. The resulting publication entry can be appended to the database using relevant functions.
# 
#' @export
#' 


create_pub_level <- function(entry){
  pub <- list()
  # Function to create publication level when inserting entries to the online form 
  # into the database (see Manual_Onlineform_2_Nested_list.Rmd)
  
  pub$publication_table <- data.frame(
    authors = ifelse("Authors" %in% names(entry), entry$Authors, NA),
    conducted = ifelse("Year" %in% names(entry), entry$Year, NA), 
    added = Sys.Date(), 
    country = ifelse("Country" %in% names(entry), entry$Country, NA), 
    contact = ifelse("Email.for.contact" %in% names(entry), entry$Email.for.contact, NA),
    keywords = ifelse("Keywords" %in% names(entry), entry$Keywords, NA),
    APA_reference = entry$APA.reference, 
    publication_code = pub_code
  )
  
  return(pub)
}
