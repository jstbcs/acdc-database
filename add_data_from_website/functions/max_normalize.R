# Function to maximum normalize a vector to range 0-1
max_normalize <- function(vec) {
  # Ensure the vector has more than one unique value to avoid division by zero
  if (length(unique(vec)) == 1) {
    warning("Vector has a single unique value; returning a vector of 0s")
    return(rep(0, length(vec)))
  }
  # Perform maximum normalization
  return((vec - min(vec, na.rm = TRUE)) / (max(vec, na.rm = TRUE) - min(vec, na.rm = TRUE)))
}
