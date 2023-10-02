#' Append Data to Database
#'
#' This function appends either study-related or data-related information to a specific database based on the provided level. It automatically handles the identification of publication, group, and other relevant IDs.
#'
#' @param conn The database connection object.
#' @param object A list containing the data to append to the database. The structure should correspond to the specified level (study or data).
#' @param level A character indicating the level of the data to append, either "study" or "data."
#' @param details A list of details containing the identifiers required for the specific level. For "study" level, this should be a publication code or study ID. For "data" level, this should be a study ID.
#' @param keys A dataframe containing the key data for grouping at the data level. This parameter is only required when appending data.
#'
#' @return This function appends the provided data to the specified database.
#'
#' @export
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