#' Return Elements that Match a Regular Expression Pattern
#'
#' This function returns elements from a character vector that match a specified regular expression pattern.
#'
#' @param vector A character vector from which you want to extract matching elements.
#' @param regex A regular expression pattern to match against the elements in the input vector.
#'
#' @return A character vector containing elements that match the specified regular expression pattern.
#'
#' @examples
#' # Create a sample character vector
#' my_vector <- c("apple123", "banana456", "cherry789", "date123")
#'
#' # Extract elements matching a specific pattern
#' matching_elements <- which_elements_match(my_vector, ".*123")
#' print(matching_elements)  # Output: [1] "apple123" "date123"
#'
#' @export
which_elements_match <- function(vector, regex){
  clean = vector[stringr::str_detect(vector, regex)]
  return(clean)
}