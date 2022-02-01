
`%||%` <- function(a, b) {
  if(is.null(a)) {
    b
  } else {
    a
  }
}

#' Split word into individual letters
#'
#' @param word word to split
parse_word <- function(word) {

  l <- stringr::str_count(word)
  stringr::str_sub(word, 1:l, 1:l)

}

clean_entry <- function(word) {
  tolower(stringr::str_replace_all(word, " ", ""))
}
