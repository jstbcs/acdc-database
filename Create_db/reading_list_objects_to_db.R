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

additional_vars = data.table::fread(paste0(data_folder_path, "research_additional_tasks.csv")) %>% 
  tidyr::separate_rows(tasks, sep = ", ?") %>% 
  mutate(tasks = tolower(tasks))

# Need to join the row from between_table to the appropriate datasets "dataset" table 
# also within each dataset, join the columns from condition_table to the within_table

# What for datasets with less between conditions than datasets

for (ipub in seq_along(lists)){
  for (istudy in 2:length(lists[[ipub]])){
    between_table = lists[[ipub]][[istudy]]$between_table
    if (nrow(between_table) == 1) between_table$between_name = 1
    
    # Get the measures table
    ipub_code = lists[[ipub]]$publication_table$publication_code
    vars_table = additional_vars %>% 
      filter(publication_code == ipub_code) %>% 
      # Here, introduce an optional filter for a study within a publication
      # filter(study_no == istudy) %>% 
      select(study_id = study_no, measure_name = tasks)
    
    if(nrow(vars_table) == 0) warning(paste0("No additional tasks for pub code: ", ipub_code))
    
    
    lists[[ipub]][[istudy]]$measures_table = vars_table
    
    # First two tables are study and between table. Last table is measures table
    # Therefor we loop from 3 to n-1
    for (idata in 3:(length(lists[[ipub]][[istudy]])-1)){
      between_num = ifelse(is.na(lists[[ipub]][[istudy]][[idata]]$dataset_table$between), 1, lists[[ipub]][[istudy]][[idata]]$dataset_table$between)
      between_info = between_table[which(between_table$between_name == between_num), -1]
      lists[[ipub]][[istudy]][[idata]]$dataset_table = cbind(lists[[ipub]][[istudy]][[idata]]$dataset_table, between_info)
      
      condition_info = lists[[ipub]][[istudy]][[idata]]$condition_table
      lists[[ipub]][[istudy]][[idata]]$within_table = cbind(lists[[ipub]][[istudy]][[idata]]$within_table, condition_info)
      
      # Recode condition variable
      lists[[ipub]][[istudy]][[idata]]$observation_table$congruency = factor(lists[[ipub]][[istudy]][[idata]]$observation_table$congruency, levels = 1:3, labels = c("congruent", "incongruent", "neutral"))
    }
  }
}

# Adding lists ------

files.sources = list.files("./functions", pattern = "\\.R$", full.names = TRUE, include.dirs = FALSE)
sapply(files.sources, source)

check_overall_structure(lists)

create_empty_db("./acdc_measures.db")

db_conn = DBI::dbConnect(RSQLite::SQLite(), "./acdc_measures.db")

add_object(db_conn, lists)

DBI::dbDisconnect(db_conn)
