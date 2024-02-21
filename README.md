# Attentional Control Data Collection

This repo contains all code required for creating and maintaining the SQLite data base storing open access data from attentional control tasks.

You can contribute data to the data base through [this](https://www.ampl-psych.com/attentional_control_data_collection/) form:

## Create_db
This folder contains scripts to create the initial database in R (using the DBI package) in `create_tables.Rmd`, as well as an add_data subfolder. The add_data folder stores all list objects required to read the datasets into the data base and the R script `adding_list_to_db.R` which inserts them into the data base. 
The datasets themselves were formatted in the `reformat_datasets.R` script. Note that we created nested a nested list object for each publication in the `scripts_creating_list_objects` subfolder.
It allows us to systematically read in data, making sure that all primary and foreign keys are correctly assigned. 
The nested list objects themselves can be found in `add_data` as well. 
To add data to the database which have been submitted to the online form, follow instructions in the script `Manual_Onlineform_2_nested_list.Rmd`.

## data
Holds all raw data sets included in the data base. The maintainer of this project should make sure that datasets newly added to the db are also strored in this folder on github. 


## functions
This folder stores all functions used for reading in, formatting, and retrieving data in this project. 

## shiny
The repo cntains code to the ACDC shiny app (see https://mfhcgn.shinyapps.io/acdc-database/), namely a server.R and a ui.R script, a shiny_functions folder and a rsconnect folder. The ui and server logic of the ACDC shiny app which gives users an overview over all datasets in the database and gives the opportunity to filter datasets. 
