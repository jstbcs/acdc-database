# Returns the next free elements in overview table as data_ids that are assigned
return_next_free_data_id <- function(conn){
  overview = tbl(conn, "data_overview")
  last_id = overview %>% 
    summarize(max = max(data_id)) %>% 
    pull(max)
  if (is.na(last_id)){
    last_id = 0
  }
  next_free = last_id + 1
  return(next_free)
}