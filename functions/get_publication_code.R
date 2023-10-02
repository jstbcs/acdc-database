# Returns vector of study code specified in object
get_publication_code <- function(object){
  stop_if_not_publication_level(object) # defined below
  publication_code = c()
  publication_code = object$publication_code
  return(publication_code)
}