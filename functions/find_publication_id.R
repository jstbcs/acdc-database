find_publication_id <- function(conn, code){
  pub_table = tbl(conn, "publication_table")
  pub_id = pub_table %>% 
    filter(publication_code == code) %>% 
    pull(publication_id)
  return(pub_id)
}
