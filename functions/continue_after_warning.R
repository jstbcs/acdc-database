# Stop function if warning input is 'STOP'
continue_after_warning <- function(message){
  answer = require_warning_input(message)
  if (answer == 2)
  {
    stop("Process cancelled")
  }
}