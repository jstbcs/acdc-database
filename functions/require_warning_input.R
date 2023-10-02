#' Require User Input to Confirm Something
#'
#' This function displays a warning message and requires the user to make a choice to confirm or cancel.
# 
#' @param message The warning message to display.
# 
#' @return A character representing the choice made by the user. It can be "Yes, I want to continue anyways" or "No. That is not what I want."
# 
#' @export
require_warning_input <- function(message){
  utils::menu(
    choices = c("Yes, I want to continue anyways", "No. That is not what I want"),
    title = paste0(message)
  )
}