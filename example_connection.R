library(acdcquery)
library(dplyr)

conn <- connect_to_db("acdc.db")

arguments <- list() %>% 
  add_argument(conn, "publication_id", "greater", -1)

result <- query_db(conn, arguments, c("publication_code", "apa_reference"), target_table = "study_table") %>% 
  distinct()
