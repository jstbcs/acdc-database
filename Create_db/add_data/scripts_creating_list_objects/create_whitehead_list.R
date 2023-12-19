# CREATE NESTED LIST OBJECT TO READ IN WHITEHEAD ET AL DATA INTO DB ############################

# NOTE: when running all scripts in "\add_data" to replicate or reconstruct the 
# first 40 datasets added to the db, we recommend first running "reformat_datasets.R"
# to have all raw datasets loaded when constructing the nested list objects 

files.sources = list.files("./functions", pattern = "\\.R$", full.names = TRUE, include.dirs = FALSE)
sapply(files.sources, source) # load all helper functions
source("./Create_db/add_data/scripts_creating_list_objects/00_create_publication_study_level.R")

# Load required info from excel file -------------------------------------------------------
publication_df <- readxl::read_excel("./Create_db/add_data/Book.xlsx", "publication_table", range = "A6:I6", 
                                     col_names = c("study", "authors", "conducted",
                                                   "added", "country","contact", 
                                                   "keywords", "APA-reference",
                                                   "publication_code")) 
study_df <- readxl::read_excel("./Create_db/add_data/Book.xlsx", "study_table", range = "A8:D10",
                               col_names = c("study", "n_groups",	"n_tasks", "comment"))
study_df$n_data <- c(3,3,3) # encode number of data sets per study (by hand)
group_df <- readxl::read_excel("./Create_db/add_data/Book.xlsx", "group_table", range = "A9:G11",
                               col_names = c("study_in_publication", "study_description",
                                             "between_id",	"mean_age",	"percentage_female",
                                             "n_members",	"group_description"))
task_df <- readxl::read_excel("./Create_db/add_data/Book.xlsx", "task", range = "A17:D25", 
                              col_names = c("study_within_pub",	"Dataset", "task",
                                            "task_description"))
dataset_df <- readxl::read_excel("./Create_db/add_data/Book.xlsx", "dataset_overview_table", range = "A17:K25",
                                 col_names = c("study_within_publication", "data",	
                                               "data_excl", "n_participants",
                                               "n_blocks", "n_trials", "neutral_trials",
                                               "fixaction_cross",	"time_limit",
                                               "github",	"dataset in R"))
within_df <- readxl::read_excel("./Create_db/add_data/Book.xlsx", "within_table", range = "A21:D29",
                                col_names = c("study_within_publication",	"data set",
                                              "within_id",	"within_desciption"))
condition_df <- readxl::read_excel("./Create_db/add_data/Book.xlsx", "condition_descriptives", range = "A74:F81",
                                   col_names = c("study_in_publication",
                                                 "dataset & condition",	"percentage_congr",
                                                 "percentage_neutral",	"mean_obs_pp",	"n_obs"))

# NOTE: read in datasets 35-40

