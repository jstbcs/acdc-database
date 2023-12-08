get_entry_information <- function(conn){
  pub_table = DBI::dbGetQuery(
    conn,
    "SELECT publication_code, publication_id FROM publication_table"
  )
  
  return(pub_table)
}
