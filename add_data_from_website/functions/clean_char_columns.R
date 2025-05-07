clean_char_columns <- function(data){
  data = dplyr::mutate(
    data, 
    dplyr::across(dplyr::everything(), ~tolower(trimws(.)))
  )
  
  return(data)
}
