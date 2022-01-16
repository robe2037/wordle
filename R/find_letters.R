
#' Identify correct, incorrect, and misplaced letters for a given target and guess word
#'
#' @param target target of the match
#' @param guess word to be compared to the target word
#' @param idx If `TRUE` return integer index of position of correct, incorrect, or misplaced letters.
#' If `FALSE` return letter characters themselves.
#'
#' @return integer or character vector
#' @export
#'
#' @examples
#' find_misplaced("torch", "trick")
#' find_correct("torch", "trick")
find_misplaced <- function(target, guess, idx = FALSE) {

  stopifnot(stringr::str_count(target) == stringr::str_count(guess))

  correct <- find_correct(target, guess, idx = TRUE)

  in_word <- which(
    purrr::map_lgl(
      parse_word(guess),
      ~stringr::str_detect(target, .x)
    )
  )

  i <- setdiff(in_word, correct)

  if(length(i) == 0) {
    NULL
  } else {
    if(idx) {
      i
    } else {
      purrr::map2(
        parse_word(guess)[i],
        i,
        ~list(.x, .y)
      )
    }
  }

}

#' @rdname find_misplaced
#' @export
find_incorrect <- function(target, guess, idx = FALSE) {

  stopifnot(stringr::str_count(target) == stringr::str_count(guess))

  correct <- find_correct(target, guess, idx = TRUE)
  misplaced <- find_misplaced(target, guess, idx = TRUE)

  i <- setdiff(1:stringr::str_count(target), c(correct, misplaced))

  if(length(i) == 0) {
    NULL
  } else {
    if(idx) {
      i
    } else {
      parse_word(guess)[i]
    }
  }

}

#' @rdname find_misplaced
#' @export
find_correct <- function(target, guess, idx = FALSE) {

  stopifnot(stringr::str_count(target) == stringr::str_count(guess))

  i <- which(parse_word(target) == parse_word(guess))

  if(length(i) == 0) {
    NULL
  } else {
    if(idx) {
      i
    } else {
      purrr::map2(
        parse_word(guess)[i],
        i,
        ~list(.x, .y)
      )
    }
  }

}


#' Split word into individual letters
#'
#' @param word single character string
#'
#' @return vector of length-1 characters
#' @export
#'
#' @examples
#' parse_word("word")
parse_word <- function(word) {

  l <- stringr::str_count(word)
  stringr::str_sub(word, 1:l, 1:l)

}


#' Make a single Wordle guess
#'
#' @description Compare a guess word to a target word and return correct,
#'incorrect, and misplaced letters with associated index positions.
#'
#' @param target target of the match
#' @param guess word to be compared to the target word
#'
#' @return List of length 3.
#' The first element is a list of lists, each of which contains a letter in the
#' guess word that was present in the target word and its associated position in the word.
#' The second element is a character vector of letters in the guess not present in the target word.
#' The third element is a list of lists, each of which contains a misplaced letter and its associated
#' position in the guess
#' @export
#'
#' @examples
#' guess <- make_guess("torch", "trick")
#' str(guess)
make_guess <- function(target, guess) {

  list(
    correct = find_correct(target, guess),
    incorrect = find_incorrect(target, guess),
    misplaced = find_misplaced(target, guess)
  )

}
