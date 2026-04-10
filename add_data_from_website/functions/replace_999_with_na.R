# Recursive function to replace 999 with NA
replace_999_with_na <- function(x) {
  if (is.list(x)) {
    lapply(x, replace_999_with_na)
  } else if ((is.numeric(x) || is.character(x)) && length(x) < 20) {
    ifelse(x == 999 | x == "NA" | x == "NaN" | x == "na" | x == "Na", NA, x)
  } else {
    x
  }
}
