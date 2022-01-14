
#' Filter dictionary using regular expression
#'
#' @param dict Character vector through which to search for pattern
#' @param pattern Regular expression to match with entries in `dict`
#' @param negate If `TRUE`, include strings in `dict` that match with `pattern`. Otherwise, exclude matches.
#'
#' @return Character vector
#' @export
#'
#' @examples
#' dictionary <- c("this", "is", "a", "dictionary")
#' find_words(dictionary, pattern = "^d")
find_words <- function(dict, pattern, negate = FALSE) {

  if(negate) {
    dict[!stringr::str_detect(dict, pattern)]
  } else {
    dict[stringr::str_detect(dict, pattern)]
  }

}
