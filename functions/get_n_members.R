# for between table -------------------
get_n_members <- function(pub, study, task, between_value){
  # input: nested list object with publication, study, and data level;
  # indeces of study, task, and between condition
  
  # filter observation_table by between value
  data <- pub[[study+1]][[task+2]][[4]]
  data <- data %>%
    filter(between == between_value)
  
  # count unique between IDs in observation_table of study i, task j
  n_members <- length(unique(data$subject))
  
  return(n_members)
}