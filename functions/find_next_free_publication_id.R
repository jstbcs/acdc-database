# PROBably outdated
find_next_free_publication_id <- function(conn){
  warning("Using bad function: 'find_next_free_publication_id")
  pub_table = tbl(conn, "publication_table")
  next_free = find_next_free(pub_table, "publication_id")
  return(next_free)
}