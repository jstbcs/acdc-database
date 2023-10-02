add_object_to_database <- function(conn, object){
  stop_if_not_top_level(object)
  
  n_data = length(object) - 1
  
  data_names = paste0("data_", 1:n_data)
  
  check_object_structure(object)
  
  prep = prepare_object_ids(conn, object)
  
  for (i in data_names)
  {
    free_data_id = return_next_free_data_id(conn)
    add_overview_table(conn, prep[[i]])
    
    if (free_data_id != return_next_free_data_id(conn) - 1)
    {
      stop("Looks like there was no overview row added. Check the SQL Database")
    }
    prep[[i]] = add_data_id(conn, prep[[i]])
    add_raw_data_table(conn, prep[[i]])
  }
}