append_db <- function(conn, object, level, details, keys = NULL){
  # Find connection
  # Write level-appropriate functions returning the correct publication/study/group/within ids
  if (!level %in% c("study", "data")){
    stop("Can only append the database on study or data level")
  }
  
  check_append_details(details)
  
  if (level == "study"){
    append_db_study(conn, object, details)
  }
  
  if (level == "data"){
    if (is.null(keys)){
      stop("Adding a dataset requires specifying a key-dataframe for group keys")
    }
    append_db_data(conn, object, details, keys)
  }
  
}