# Return only elements that match a code
which_elements_match <- function(vector, regex){
  clean = vector[stringr::str_detect(vector, regex)]
  return(clean)
}