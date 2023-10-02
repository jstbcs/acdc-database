#' Prompt the user to continue or stop after a warning message.
#'
#' This function displays a warning message and prompts the user to decide whether to continue or stop the process.
# The user can input '1' to continue or '2' to stop the process.
# If '2' is entered, the function raises a stop message to halt the process.
# This is useful when there is a warning condition, and the user needs to decide how to proceed.
# 
#' @param message The warning message to display to the user.
# 
#' @return If the user enters '1', the function continues. If the user enters '2', the function raises a stop message to halt the process.
# 
#' @details This function is used to handle situations where a warning message is generated, and the user needs to decide whether to continue
# with the operation despite the warning. The function displays the 'message' and waits for user input. If '1' is entered, it continues;
# if '2' is entered, it raises a stop message, canceling the process.
#'
#' @export
#' 
continue_after_warning <- function(message){
  answer = require_warning_input(message)
  if (answer == 2)
  {
    stop("Process cancelled")
  }
}