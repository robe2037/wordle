
#' Filter dictionary using regular expression
#'
#' @param pattern Regular expression to match with entries in `dict`
#' @param dict Vector of words through which to search for pattern
#' @param neg If `TRUE`, include strings in `dict` that match with `pattern`. Otherwise, exclude matches.
#'
#' @return Character vector
find_words <- function(pattern, dict = NULL, neg = FALSE) {

  if(is.null(dict)) {
    dict <- dictionary
  }

  if(neg) {
    dict[!stringr::str_detect(dict, pattern)]
  } else {
    dict[stringr::str_detect(dict, pattern)]
  }

}
