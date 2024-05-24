library(acdcquery)
library(dplyr)

conn <- connect_to_db("acdc.db")

arguments <- list() %>% 
  add_argument(conn, "study_id", "equal", 6)

result <- query_db(conn, arguments, c("default"), target_table = "study_table")

dist <- result %>% distinct()
