#' Return the ID Column Name for a Given Table
#'
#' This function generates the name of the ID column for a specified table name. 
# For example, if you provide "publication_table" as the table name, it will return "publication_id".
# 
#' @param table_name The name of the table for which you want to generate the ID column name.
# 
#' @return A character string representing the ID column name for the specified table.
# 
#' @examples
#' # Example usage:
#' id_name <- return_id_name_from_table("publication_table")
#' # id_name will contain "publication_id"
# 
#' @export
return_id_name_from_table <- function(table_name){
  name = stringr::str_replace(table_name, "table$", "id")
  return(name)
}