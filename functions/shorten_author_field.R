#' Shorten Author Field
#'
#' This function shortens a string containing author names by replacing all but the first author with "et al.".
# It assumes that the author names are separated by commas, and the last name of the first author is extracted
# based on spaces.
#
#' @param authors A character vector containing author names, separated by commas.
#
#' @return A character vector with the first author's last name followed by "et al." if there are multiple authors,
# or the same input if there's only one author.
#
#' @examples
#' # Shorten an author field with multiple authors
#' authors <- "John Doe, Jane Smith, Mary Johnson"
#' shortened_authors <- shorten_author_field(authors)
#
#' # Print the shortened authors
#' print(shortened_authors)  # Output: "Doe et al."
#
#' # Shorten an author field with a single author
#' single_author <- "Alice Brown"
#' shortened_single_author <- shorten_author_field(single_author)
#
#' # Print the shortened single author
#' print(shortened_single_author)  # Output: "Alice Brown"
#
#' @export
shorten_author_field <- function(authors){
  # assumes all authors are separated by commas and the letters
  # following the last space is the last name of the author

  n_commas = stringr::str_count(authors, ",")

  if (n_commas < 2){
    return(authors)
  } else {
    first_author = strsplit(authors, ",")[[1]][1]
    last_name = strsplit(first_author, " ")[[1]][length(strsplit(first_author, " ")[[1]])]

    authors = paste(last_name, "et al.")
    return(authors)
  }
}
