#' Get Appropriate Regular Expression Pattern
#'
#' This function returns a regular expression pattern based on the specified type.
# The pattern can be used to match and validate strings according to the provided type.
# 
#' @param type A character string specifying the type for which a regular expression pattern is needed.
# 
#' @return A regular expression pattern for the specified type.
#'
#' @export
get_appropriate_regex_pattern <- function(type){
  switch(
    type,
    publication_code = {"^[a-zA-Z]+_[12][0-9][0-9][0-9]_[a-zA-Z]+$"},
    publication_names = {"^publication([1-9][0-9]?)$"},
    study_names = {"^study([1-9][0-9]?)$"},
    data_names = {"^data([1-9][0-9]?)$"}
  )
}
