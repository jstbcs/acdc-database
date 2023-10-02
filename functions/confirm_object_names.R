#' Confirm that an object has the correct specified names based on a data frame that defines the expected names.
#'
#' This function checks whether an object contains the correctly specified names based on a provided data frame
#' that defines the expected names and whether they are mandatory or not. 
# It handles both mandatory and optional columns and raises warnings if mandatory columns are missing.
# For optional columns, it informs the user about any missing names.
# 
#' @param object The object or dataframe to check for the specified names.
#' @param data_frame_names A data frame that specifies the expected column names and their mandatory status (1 for mandatory, 0 for optional).
#'
#' @details This function checks the column names in the 'object' against the expected names specified in 'data_frame_names'.
# If a column is marked as mandatory (with '1' in the 'mandatory' column of 'data_frame_names'), and it is missing from 'object',
# a warning will be raised to inform the user.
# If a column is marked as optional, a warning will be raised to inform the user if it's missing from 'object.
# This function helps ensure that 'object' follows the expected structure.
#'
#' @export
confirm_object_names <- function(object, data_frame_names){
  # This function checks whether an object has the correctly specified names
  # The data_frame_names gives the info on what names should be there and 
  # whether they are mandatory or not.
  
  for (i in 1:nrow(data_frame_names)){
    if (data_frame_names$mandatory[i] == 1){
      are_mandatory_elements_present(object, data_frame_names$column[i])
    } else {
      confirm_columns_not_specified(data_frame_names$column[i], object)
    }
  }
}