#' Check the structure of a data list element.
#'
#' This function validates the structure of a data list element, ensuring that it adheres to the expected format.
#'
#' @param object A list element representing data to be validated. The structure should contain data entries with "data" and "overview" sub-elements.
#'
#' @details This function checks the provided 'object' structure to ensure it adheres to the expected format. It validates the presence of required elements and verifies the structure of sub-elements, including "data" and "overview".
#'
#' @return This function does not return a value. It raises an error if the 'object' structure does not meet the expected criteria.
#'
#' @seealso \code{\link{do_elements_exist}}, \code{\link{confirm_columns_not_specified}}
#'
#' @export
check_data_structure <- function(object){
  # Check if all other (not study) objects are lists containing two 1 items
  overview_names = c("task_name", "keywords", "data_exclusions", "codebook", 
                     "n_participants", "n_blocks", "n_trials", 
                     "mean_effect", "sd_effect", "neutral_trials",
                     "percentage_incongruent", "feedback",
                     "fixation_cross", "time_limit",
                     "mean_age", "percentage_female", "percentage_male"
  )
  for (i in 2:length(object)){
    if(inherits(object[[i]], "list") == FALSE)
    {
      stop(paste0("Error in structure of data_", i - 1, ":", " Data entry in object must be a list of two elements (data, overview)"))
    }
    if(length(object[[i]]) != 2)
    {
      stop(paste0("Error in structure of data_", i - 1, ":", " Data entry in object must be a list of two elements (data, overview)"))
    }
    if (do_elements_exist(c("data", "overview"), object[[i]]) == FALSE)
    {
      stop(paste0("Error in structure of data_", i - 1, ":", " Data entry in object must be a list of two elements (data, overview)"))
    } 
    if(is.data.frame(object[[i]]$data) == FALSE)
    {
      stop(paste0("Error in structure of data_", i - 1, ":", " Raw data must be a data frame"))
    } 
    if(is.data.frame(object[[i]]$overview) == FALSE)
    {
      stop(paste0("Error in structure of data_", i - 1, ":", " Overview must be a data frame"))
    } 
    if (do_elements_exist(c("subject", "trial", "accuracy", "rt", "block", "congruency", "age_group"), object[[i]]$data) == FALSE)
    {
      stop(paste0("Error in structure of data_", i - 1, ":", " Raw data must have correct columns specified"))
    } 
    if (do_elements_exist(c("task_name", "keywords"), object[[i]]$overview) == FALSE)
    {
      stop(paste0("Error in structure of data_", i - 1, ":", " Overview must have correct columns specified"))
    } 
    confirm_columns_not_specified(overview_names, object[[i]]$overview)
  } # TODO: Talk about which elemens have to exist in overview, and which are optional
  # Right now all columns in $data are required, but only task_name and keywords for overview
}