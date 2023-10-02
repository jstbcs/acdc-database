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