# Library
library(dplyr)
library(RSQLite)
library(DBI)
library(acdcquery)
source("./shiny/helper_file_shiny.R")

# Getting info on lists -------
files.sources = list.files("./functions", pattern = "\\.R$", full.names = TRUE, include.dirs = FALSE)
sapply(files.sources, source)

conn <- connect_to_db("acdc.db")

arguments <- list() %>% 
  add_argument(
    conn,
    "publication_id",
    "greater",
    -1
  ) 

overview <- get_overview_information(conn, arguments, "and")
descriptive <- get_descriptive_information(conn, arguments, "and")
detailed <- get_detailed_information(conn, arguments, "and")
colnames(descriptive) <- colnames_descriptives 

input = "Dataset ID"
# setting the order_by arguments to column name returns error
plot_dataset_histograms(descriptive, order_by = {{input}})
plot_trial_rtdist(detailed, 4, 2)

detailed %>% plot_trial_rtdist(., 51)
