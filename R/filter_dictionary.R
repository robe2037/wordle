
#' Filter dictionary of target words using Wordle logic
#'
#' Restrict original dictionary of possible words to those that meet criteria
#' based on information about letters that are correct, incorrect, or misplaced
#' in the unknown target word.
#'
#' @param dictionary Vector of strings of possible words to match
#' @param letters Vector of single-length character strings, indicating the letters
#' that are correct, incorrect, or misplaced.
#' @param positions For \code{filter_correct} and \code{filter_misplaced},
#' vector of integers between 1 and 5 that indicates the index within
#' the word where each associated misplaced letter was guessed.
#'
#' @name filter_wordle
#'
#' @return Character vector of words in `dictionary` that meet specified criteria
#'
#' @examples
#' dictionary <- c("cumin", "salad", "touch", "urges")
#'
#' filter_misplaced(dictionary, letters = c("a", "l"), positions = c(1, 2))
#' filter_incorrect(dictionary, letters = c("t", "e"))
#' filter_correct(dictionary, letters = c("o", "h"), positions = c(2, 5))
NULL

#' @rdname filter_wordle
#' @export
filter_correct <- function(dictionary, letters, positions) {

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

#' @rdname filter_wordle
#' @export
filter_incorrect <- function(dictionary, letters) {

  if(!all(stringr::str_detect(unlist(letters), "[A-Za-z]"))) {
    warning(
      "Nonalphabetical characters passed to `filter_incorrect` are ignored.",
      call. = FALSE
    )
  }

  if(is.null(letters)) {
    return(dictionary)
  }

  letters <- paste0(letters, collapse = "")
  pattern <- glue::glue("^[^{letters}]+$")

  find_words(dictionary, pattern)

}

#' @rdname filter_wordle
#' @export
filter_misplaced <- function(dictionary, letters, positions) {

  patterns <- unlist(
    purrr::map2(
      positions,
      letters,
      ~glue::glue("^.{<.x-1>}[^<.y>](.?)+$", .open = "<", .close = ">")
    )
  )

  not_in_position_words <- purrr::reduce(
    purrr::map(
      patterns,
      ~find_words(dictionary, .x)
    ),
    intersect
  )

  letters_in_word <- paste0(glue::glue("(?=.*{letters})"), collapse = "")

  find_words(not_in_position_words, letters_in_word)

}
