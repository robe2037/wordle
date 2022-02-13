
#' Filter dictionary of target words using Wordle logic
#'
#' Restrict original dictionary of possible words to those that meet criteria
#' based on information about letters that are correct, incorrect, or misplaced
#' in the unknown target word.
#'
#' @param guess String of guessed word
#' @param target If provided, word to use as comparison to `guess` to determine
#' which letters are correct, incorrect, misplaced. If not specified, at least
#' one of correct, incorrect, or misplaced must be specified.
#' @param correct Vector of integers indicating the index position of all correct
#' letters in `guess`
#' @param incorrect Vector of integers indicating the index position of all incorrect
#' letters in `guess`
#' @param misplaced Vector of integers indicating the index position of all misplaced
#' letters in `guess`
#' @param dict Vector of strings of possible words to match. If NULL, uses `dictionary` as default.
#' @param hide If `TRUE`, return number of words remaining after filtering. If
#' `FALSE`, return vector of remaining words themselves.
#'
#' @return Either a vector of words remaining after filtering or integer
#' indicating the number of words remaining.
#' @export
#'
#' @examples
#' target <- "tangy"
#' guess <- "tonal"
#'
#' filter_guess(
#'   guess = guess,
#'   target = target
#' )
#'
#' filter_guess(
#'   guess = guess,
#'   correct = c(1, 3),
#'   incorrect = c(2, 5),
#'   misplaced = 4
#' )
filter_guess <- function(dict = NULL,
                         guess = NULL,
                         target = NULL,
                         correct = NULL,
                         incorrect = NULL,
                         misplaced = NULL,
                         hide = TRUE) {

  dict <- dict %||% dictionary

  guess_letters <- parse_word(guess)

  if(!is.null(target)) {

    if(guess == target) {
      return(NULL)
    }

    correct <- find_correct(target, guess)
    incorrect <- find_incorrect(target, guess)
    misplaced <- find_misplaced(target, guess)

  } else {

    if(is.null(c(correct, incorrect, misplaced))) {
      stop(
        "Either `target` or at least one of `correct`, `incorrect`, or `misplaced`",
        "must be provided",
        call. = FALSE
      )
    }

    if(is.character(correct)) {

      if(!all(correct %in% guess_letters)) {
        stop(
          "All correct letters must be present in guess word.",
          call. = FALSE
        )
      }

      correct <- purrr::map_int(correct, ~which(guess_letters == .x))
    }

    if(is.character(incorrect)) {

      if(!all(incorrect %in% guess_letters)) {
        stop(
          "All incorrect letters must be present in guess word.",
          call. = FALSE
        )
      }

      incorrect <- purrr::map_int(incorrect, ~which(guess_letters == .x))
    }

    if(is.character(misplaced)) {

      if(!all(misplaced %in% guess_letters)) {
        stop(
          "All misplaced letters must be present in guess word.",
          call. = FALSE
        )
      }

      misplaced <- purrr::map_int(misplaced, ~which(guess_letters == .x))
    }
  }

  if(is.null(correct)) {
    correct_words <- dict
  } else {
    correct_words <- filter_correct(guess_letters[correct], correct, dict = dict)
  }

  if(is.null(incorrect)) {
    incorrect_words <- dict
  } else {
    incorrect_words <- filter_incorrect(guess_letters[incorrect], dict = dict)
  }

  if(is.null(misplaced)) {
    misplaced_words <- dict
  } else {
    misplaced_words <- filter_misplaced(guess_letters[misplaced], misplaced, dict = dict)
  }

  remaining <- purrr::reduce(
    list(
      correct_words,
      incorrect_words,
      misplaced_words
    ),
    intersect
  )

  if(hide) {
    length(remaining)
  } else {
    remaining
  }
}

#' Filter dictionary of target words using Wordle logic
#'
#' Restrict original dictionary of possible words to those that meet criteria
#' based on information about letters that are correct, incorrect, or misplaced
#' in the unknown target word.
#'
#' @param guesses vector of character indicating sequential guesses
#' @param target Word to use as comparison to `guess` to determine
#' which letters are correct, incorrect, or misplaced.
#' @param dict Vector of strings of possible words to match. If NULL, uses `dictionary` as default.
#' @param hide If `TRUE`, return number of words remaining after filtering. If
#' `FALSE`, return vector of remaining words themselves.
#'
#' @return List of either a vector of words remaining after filtering or integer
#' indicating the number of words remaining.
#' @export
#'
#' @examples
#' target <- "panic"
#' guess <- c("burst", "canoe")
#'
#' filter_game(dict = dictionary, guess, target)
filter_game <- function(guesses, target, dict = NULL, hide = TRUE) {

  dict <- dict %||% dictionary

  remaining <- list()

  for(guess in guesses) {
    dict <- filter_guess(dict = dict, guess, target, hide = FALSE)

    if(hide) {
      remaining <- append(remaining, length(dict))
    } else {
      remaining <- append(remaining, list(dict))
    }
  }

  remaining

}

#' Filter dictionary of target words using Wordle logic
#'
#' Restrict original dictionary of possible words to those that meet criteria
#' based on information about letters that are correct, incorrect, or misplaced
#' in the unknown target word.
#'
#' @param letters Vector of single-length character strings, indicating the letters
#' that are correct, incorrect, or misplaced.
#' @param positions For \code{filter_correct} and \code{filter_misplaced},
#' vector of integers between 1 and 5 that indicates the index within
#' the word where each associated misplaced letter was guessed.
#' @param dict Vector of strings of possible words to match
#'
#' @name filter_wordle
#'
#' @return Character vector of words in `dictionary` that meet specified criteria
NULL

#' @rdname filter_wordle
filter_correct <- function(letters, positions, dict = NULL) {

  dict <- dict %||% dictionary

  patterns <- unlist(
    purrr::map2(
      positions,
      letters,
      ~glue::glue("^.{<.x-1>}<.y>(.?)+$", .open = "<", .close = ">")
    )
  )

  purrr::reduce(
    purrr::map(patterns, ~find_words(.x, dict = dict)),
    intersect
  )

}

#' @rdname filter_wordle
filter_incorrect <- function(letters, dict = NULL) {

  dict <- dict %||% dictionary

  if(!all(stringr::str_detect(unlist(letters), "[A-Za-z]"))) {
    warning(
      "Nonalphabetical characters passed to `filter_incorrect` are ignored.",
      call. = FALSE
    )
  }

  if(is.null(letters)) {
    return(dict)
  }

  letters <- paste0(letters, collapse = "")
  pattern <- glue::glue("^[^{letters}]+$")

  find_words(pattern, dict = dict)

}

#' @rdname filter_wordle
filter_misplaced <- function(letters, positions, dict = NULL) {

  dict <- dict %||% dictionary

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
      ~find_words(.x, dict = dict)
    ),
    intersect
  )

  letters_in_word <- paste0(glue::glue("(?=.*{letters})"), collapse = "")

  find_words(letters_in_word, not_in_position_words)

}

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
