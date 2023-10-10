create_testing_db <- function(file_path){
  conn = DBI::dbConnect(RSQLite::SQLite(), file_path)
  
  # Publication table
  DBI::dbExecute(
    conn,
    "CREATE TABLE publication_table (
    publication_id INTEGER PRIMARY KEY AUTOINCREMENT,
    authors VARCHAR(10000),
    conducted DATE,
    added DATE,
    country VARCHAR(255),
    contact VARCHAR(10000),
    apa_reference VARCHAR(10000),
    keywords VARCHAR(10000),
    publication_code VARCHAR(255) UNIQUE
    );"
  )
  
  DBI::dbExecute(
    conn,
    "CREATE TABLE study_table (
    study_id INTEGER PRIMARY KEY AUTOINCREMENT,
    publication_id INTEGER,
    n_groups INTEGER,
    n_tasks INTEGER,
    study_comment VARCHAR(10000),
    FOREIGN KEY (publication_id) REFERENCES publication_table(publication_id)
    );"
  )
  
  DBI::dbExecute(
    conn,
    "CREATE TABLE between_table (
    between_id INTEGER PRIMARY KEY AUTOINCREMENT,
    study_id INTEGER,
    mean_age FLOAT,
    percentage_female FLOAT,
    n_members INTEGER,
    group_description VARCHAR(10000),
    FOREIGN KEY (study_id) REFERENCES study_table(study_id)
    );"
  )
  
  DBI::dbExecute(
    conn,
    "CREATE TABLE task_table (
    task_id INTEGER PRIMARY KEY AUTOINCREMENT,
    task_name VARCHAR(255),
    task_description VARCHAR(10000)
    );"
  )
  
  DBI::dbExecute(
    conn,
    "CREATE TABLE dataset_table (
    dataset_id INTEGER PRIMARY KEY AUTOINCREMENT,
    study_id INTEGER,
    task_id INTEGER,
    data_excl VARCHAR(10000),
    n_participants INTEGER,
    n_blocks INTEGER,
    n_trials INTEGER,
    neutral_trials BOOLEAN,
    fixation_cross VARCHAR(255),
    time_limit INTEGER,
    mean_dataset_rt INTEGER,
    mean_dataset_acc INTEGER,
    github VARCHAR(1000),
    dataset_comment VARCHAR(10000),
    FOREIGN KEY (study_id) REFERENCES study_table(study_id),
    FOREIGN KEY (task_id) REFERENCES task_table(task_id)
    );"
  )
  
  DBI::dbExecute(
    conn,
    "CREATE TABLE within_table (
    within_id INTEGER PRIMARY KEY AUTOINCREMENT,
    dataset_id INTEGER,
    within_description VARCHAR(10000),
    FOREIGN KEY (dataset_id) REFERENCES dataset_table(dataset_id)
    );"
  )
  
  DBI::dbExecute(
    conn,
    "CREATE TABLE condition_table (
    condition_id INTEGER PRIMARY KEY AUTOINCREMENT,
    dataset_id INTEGER,
    between_id INTEGER,
    within_id INTEGER, 
    percentage_congruent FLOAT,
    percentage_neutral FLOAT,
    n_obs INTEGER,
    mean_obs_per_participant INTEGER,
    mean_condition_rt INTEGER, 
    mean_condition_acc INTEGER,
    FOREIGN KEY (dataset_id) REFERENCES dataset_table(dataset_id),
    FOREIGN KEY (between_id) REFERENCES between_table(between_id),
    FOREIGN KEY (within_id) REFERENCES within_table(within_id)
    );"
  )
  
  DBI::dbExecute(
    conn,
    "CREATE TABLE observation_table (
    observation_id INTEGER PRIMARY KEY AUTOINCREMENT,
    dataset_id INTEGER,
    subject INTEGER,
    block INTEGER,
    trial INTEGER,
    condition_id INTEGER,
    congruency INTEGER,
    accuracy BOOLEAN,
    rt FLOAT,
    FOREIGN KEY (dataset_id) REFERENCES dataset_table(dataset_id),
    FOREIGN KEY (condition_id) REFERENCES condition_table(condition_id),
    UNIQUE (dataset_id, subject, block, trial, condition_id)
    );"
  )
}

