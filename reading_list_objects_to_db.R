# Library
library(dplyr)
library(RSQLite)
library(DBI)

# Getting info on lists -------
# NOTE: adjust this code to select only those list objects you want to newly add to the db
data_folder_path = "./Create_db/add_data/"

list_files = list.files(data_folder_path, "(list).*(RData)", full.names = TRUE)

lists = vector(mode = "list", length = length(list_files))

for (i in seq_along(list_files)){
  lists[[i]] = readRDS(list_files[i])
}

names(lists) = paste0("publication", seq_along(list_files))

# Need to join the row from between_table to the appropriate datasets "dataset" table 
# also within each dataset, join the columns from condition_table to the within_table
for (ipub in seq_along(lists)){
  for (istudy in 2:length(lists[[ipub]])){
    between_table = lists[[ipub]][[istudy]]$between_table
    for (idata in 3:length(lists[[ipub]][[istudy]])){
      data_num = idata - 2
      between_info = between_table[data_num, -1]
      lists[[ipub]][[istudy]][[idata]]$dataset_table = cbind(lists[[ipub]][[istudy]][[idata]]$dataset_table, between_info)
      
      condition_info = lists[[ipub]][[istudy]][[idata]]$condition_table
      lists[[ipub]][[istudy]][[idata]]$within_table = cbind(lists[[ipub]][[istudy]][[idata]]$within_table, condition_info)
    }
  }
}

# Adding lists ------

files.sources = list.files("./functions", pattern = "\\.R$", full.names = TRUE, include.dirs = FALSE)
sapply(files.sources, source)

check_overall_structure(lists)

create_empty_db("acdc.db")

db_conn = DBI::dbConnect(RSQLite::SQLite(), "acdc.db")

add_object(db_conn, lists)
