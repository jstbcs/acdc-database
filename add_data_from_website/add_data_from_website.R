# Overview
# This script is meant to be used to add data from the website to the database.
# It is not meant to be used to add data from other sources, such as the raw
# data files, or to add data that is not in the format of the submission JSON.

# The script is structured as follows:  
# 1. Source the functions that are needed to prepare the data and add it to the database.
# 2. Prepare the data by reading it from the JSON file and transforming it into the usual object thing.
# 3. Check the structure of the data with the functions that are provided.
# 4. Submit the data to the database.
# 5. Check that the data has been added to the database by querying it with the acdcquery package.


# 1. Source Scripts
## Scripts necessary to prepare the data for submission
functions_path_submission <- list.files("./add_data_from_website/functions", 
                                        pattern = "*\\.R",
                                        full.names = TRUE)
sapply(functions_path_submission, source)

## Scripts necessary to check the structure of the data and add it to the database
functions_path_integration <- list.files("./functions", 
                                         pattern = "*\\.R",
                                         full.names = TRUE)
sapply(functions_path_integration, source)

## Library
library(dplyr)
library(RSQLite)
library(DBI)


# Prepare Submission
# Read from JSON
# Loop over this if you want to submit multiple files
# Make sure to change the path to the JSON file that you want to submit
json_path <- "./add_data_from_website/test/submission_Rey-Mermet_2018.json"

unprepped_obj <- extract_from_submission_json(json_path)

# 2. Transform into the usual R-list object 
prepped_obj <- prep_submission_data(unprepped_obj)

# # Allow inspection
# inspect_publication_data(prepped_obj)
# inspect_study_data(prepped_obj)
# inspect_task_data(prepped_obj)
# inspect_measurement_data(prepped_obj)
# inspect_dataset_data(prepped_obj)
# inspect_within_data(prepped_obj)
# inspect_raw_data(prepped_obj)

# 3. Check structure with functions
check_overall_structure(prepped_obj)
# MAKE SURE THIS PASSES WITHOUT ERRORS

# 4. Submit to db
path = "./acdc.db"
# create_empty_db(path) # for testing in a clean database

db_conn = DBI::dbConnect(RSQLite::SQLite(), path)

add_object(db_conn, prepped_obj)

DBI::dbDisconnect(db_conn)

# 5. Check that the data has been added to the database by querying it with the acdcquery package.
library(acdcquery)
con <- connect_to_db(path)
arguments <- list() %>% 
  add_argument(
    con,
    "publication",
    "equal",
    "1" # change this to the publication ID that you want to check
  )

result <- query_db(con, arguments, "default", "publication_table")

result <- query_db(con, arguments, "default", "observation_table")
