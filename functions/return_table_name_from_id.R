#' Return the table name based on an ID column name.
#'
#' Given an ID column name, this function constructs the associated table name by
#' replacing "id" with "table" in the name. For example, "publication_id" would be
#' transformed into "publication_table".
#'
#' @param id_name The ID column name.
#' @return The table name associated with the provided ID column name.
#' @export
return_table_name_from_id <- function(id_name){
  name = stringr::str_replace(id_name, "id$", "table")
  return(name)
}