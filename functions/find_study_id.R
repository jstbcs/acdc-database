# Find study id of table for a given code
find_study_id <- function(conn, object){
  stop_if_not_top_level(object)
  code = get_study_code(object)
  study_table = tbl(conn, "study")
  study_id = study_table %>% 
    filter(study_code == code) %>% 
    pull(study_id)
  return(study_id)
}