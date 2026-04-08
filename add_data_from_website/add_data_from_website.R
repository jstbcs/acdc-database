# Source Scripts
functions_path_submission <- list.files("./add_data_from_website/functions", 
                                        pattern = "*\\.R",
                                        full.names = TRUE)
sapply(functions_path_submission, source)

functions_path_integration <- list.files("./functions", 
                                         pattern = "*\\.R",
                                         full.names = TRUE)
sapply(functions_path_integration, source)

# Library
library(dplyr)
library(RSQLite)
library(DBI)


# Prepare Submission
# Read from JSON
# Loop over this if you want to submit multiple files
json_path <- "./add_data_from_website/test/submission_Rey-Mermet_2018.json"

unprepped_obj <- extract_from_submission_json(json_path)
# Transform into the usual object thing
prepped_obj <- prep_submission_data(unprepped_obj)

# # Allow inspection
# inspect_publication_data(prepped_obj)
# inspect_study_data(prepped_obj)
# inspect_task_data(prepped_obj)
# inspect_measurement_data(prepped_obj)
# inspect_dataset_data(prepped_obj)
# inspect_within_data(prepped_obj)
# inspect_raw_data(prepped_obj)

# Check structure with functions
check_overall_structure(prepped_obj)
# MAKE SURE THIS PASSES WITHOUT ERRORS

# Submit to db
path = "./acdc.db"
create_empty_db(path)

db_conn = DBI::dbConnect(RSQLite::SQLite(), path)

add_object(db_conn, prepped_obj)

DBI::dbDisconnect(db_conn)

# # Check
# library(acdcquery)
# con <- connect_to_db(path)
# arguments <- list() %>% 
#   add_argument(
#     con,
#     "study_id",
#     "equal",
#     "1"
#   )
# 
# result <- query_db(con, arguments, "default", "within_table")
