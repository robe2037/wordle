
#' Determine letter categories for a Wordle guess
#'
#' Compare a guess word to a known target word and determine the index locations
#' for all correct, incorrect, and misplaced letters in the guess. Guess word
#' and target word must have the same number of letters.
#'
#' @param target target of the match
#' @param guess word to be compared to the target word
#'
#' @name find_letters
#'
#' @return integer vector
NULL

#' @rdname find_letters
find_correct <- function(target, guess) {

  target <- clean_entry(target)
  guess <- clean_entry(guess)

  stopifnot(stringr::str_count(target) == stringr::str_count(guess))

  i <- which(parse_word(target) == parse_word(guess))

  if(length(i) == 0) {
    return(NULL)
  }

  i

}

#' @rdname find_letters
find_incorrect <- function(target, guess) {

  target <- clean_entry(target)
  guess <- clean_entry(guess)

  stopifnot(stringr::str_count(target) == stringr::str_count(guess))

  guess <- parse_word(guess)
  target <- parse_word(target)

  i <- which(!purrr::map_lgl(guess, ~.x %in% target))

  if(length(i) == 0) {
    return(NULL)
  }

  i

}

#' @rdname find_letters
find_misplaced <- function(target, guess) {

  target <- clean_entry(target)
  guess <- clean_entry(guess)

  stopifnot(stringr::str_count(target) == stringr::str_count(guess))

  correct <- find_correct(target, guess)

  in_word <- which(
    purrr::map_lgl(
      parse_word(guess),
      ~stringr::str_detect(target, .x)
    )
  )

  i <- setdiff(in_word, correct)

  if(length(i) == 0) {
    return(NULL)
  }

  i

}
