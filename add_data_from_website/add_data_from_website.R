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

# Prepare Submission
# Read from JSON
json_path <- "./add_data_from_website/test/submission_Rey-Mermet_2018.json"


unprepped_obj <- extract_from_submission_json(json_path)

prepped_obj <- prep_submission_data(unprepped_obj)

inspect_publication_data(prepped_obj)
inspect_study_data(prepped_obj)
inspect_task_data(prepped_obj)
inspect_measurement_data(prepped_obj)
inspect_dataset_data(prepped_obj)
inspect_within_data(prepped_obj)
inspect_raw_data(prepped_obj)

# Transform into the usual object thing

# Add to database
