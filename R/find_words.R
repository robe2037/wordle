
#' Filter dictionary using regular expression
#'
#' @param dictionary Character vector through which to search for pattern
#' @param pattern Regular expression to match with entries in `dict`
#' @param negate If `TRUE`, include strings in `dict` that match with `pattern`. Otherwise, exclude matches.
#'
#' @return Character vector
#' @export
#'
#' @examples
#' dictionary <- words::words$word
#' find_words(dictionary, pattern = "^d")
find_words <- function(dictionary, pattern, negate = FALSE) {

  if(negate) {
    dictionary[!stringr::str_detect(dictionary, pattern)]
  } else {
    dictionary[stringr::str_detect(dictionary, pattern)]
  }

}
