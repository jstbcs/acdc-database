# code condition column based on combination of within and between column
code_condition <- function(df){
  # create overview of conditions
  conditions <- df %>%
    count(within, between) %>%
    select(within, between) %>%
    mutate(condition = 1:nrow(.))
  
  # add respective condition value to observation
  data <- df %>% 
    left_join(conditions, by = join_by(between, within))
  
  return(data)
}