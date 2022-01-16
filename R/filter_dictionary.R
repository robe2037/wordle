
#' Filter dictionary of target words based on letters that are misplaced.
#'
#' @param dictionary Character vector of possible words to match
#' @param letters List of single-length character strings, indicating the letters
#' that are misplaced.
#' @param positions List of integers between 1 and 5 that indicates the index within
#' the word where each associated (at same list index) misplaced letter was guessed.
#'
#' @return Character vector of possible target words remaining after applying misplaced letter logic
#' @export
#'
#' @examples
#' dictionary <- c("cumin", "salad", "touch", "urges")
#' filter_misplaced(dictionary, letters = list("a", "l"), positions = list(1, 2))
filter_misplaced <- function(dictionary, letters, positions) {

  if(!is.list(letters)) {
    letters <- list(letters)
  }

  if(!is.list(positions)) {
    positions <- list(positions)
  }

  patterns <- unlist(
    purrr::map2(
      positions,
      letters,
      ~glue::glue("^.{<.x-1>}[^<.y>](.?)+$", .open = "<", .close = ">")
    )
  )

  w1 <- purrr::reduce(
    purrr::map(
      patterns,
      ~find_words(dictionary, .x)
    ),
    intersect
  )

  q <- paste0(glue::glue("(?=.*{letters})"), collapse = "")

  find_words(w1, q)

}

#' Filter dictionary of target words based on letters that are incorrect
#'
#' @param dictionary Character vector of possible words to match
#' @param letters vector of single-length strings indicating letters not present in target word.
#'
#' @return Character vector of possible target words remaining after applying incorrect letter logic
#' @export
#'
#' @examples
#' dictionary <- c("cumin", "salad", "touch", "urges")
#' filter_incorrect(dictionary, letters = list("t", "e"))
filter_incorrect <- function(dictionary, letters) {

  if(is.null(letters)) {
    return(dictionary)
  }

  letters <- paste0(letters, collapse = "")
  pattern <- glue::glue("^[^{letters}]+$")

  find_words(dictionary, pattern)

}

#' Filter dictionary of target words based on letters that are correct
#'
#' @param dictionary Character vector of possible words to match
#' @param letters List of single-length character strings, indicating the letters
#' that are present in the word.
#' @param positions List of integers between 1 and 5 that indicates the index within
#' the word where each associated (at same list index) correct letter was guessed.
#'
#' @return Character vector of possible target words remaining after applying correct letter logic
#' @export
#'
#' @examples
#' dictionary <- c("cumin", "salad", "touch", "urges")
#' filter_correct(dictionary, letters = list("o", "h"), positions = list(2, 5))
filter_correct <- function(dictionary, letters, positions) {

  if(!is.list(letters)) {
    letters <- list(letters)
  }

  if(!is.list(positions)) {
    positions <- list(positions)
  }

  patterns <- unlist(
    purrr::map2(
      positions,
      letters,
      ~glue::glue("^.{<.x-1>}<.y>(.?)+$", .open = "<", .close = ">")
    )
  )

  purrr::reduce(
    purrr::map(patterns, ~find_words(dictionary, .x)),
    intersect
  )

}
