find_next_free_id <- function(conn, type){
  data = as.data.frame(tbl(conn, type))
  column = return_id_name_from_table(type)
  max = max(data[column], na.rm = TRUE)
  if (is.na(max) | max <= 0){
    max = 0
  }
  next_free = max + 1
  return(next_free)
}