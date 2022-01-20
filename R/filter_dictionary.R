
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

#' Filter dictionary of target words using Wordle logic
#'
#' Restrict original dictionary of possible words to those that meet criteria
#' based on information about letters that are correct, incorrect, or misplaced
#' in the unknown target word.
#'
#' @param dictionary Vector of strings of possible words to match
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
#' @param hide If `TRUE`, return number of words remaining after filtering. If
#' `FALSE`, return vector of remaining words themselves.
#'
#' @return Either a vector of words remaining after filtering or integer
#' indicating the number of words remaining.
#' @export
#'
#' @examples
#' all_words <- words::words$word
#' dictionary <- all_words[stringr::str_count(all_words) == 5]
#'
#' target <- "tangy"
#' guess <- "tonal"
#'
#' filter_guess(dictionary, guess, target)
#'
#' filter_guess(
#'   dictionary,
#'   guess,
#'   correct = c(1, 3),
#'   incorrect = c(2, 5),
#'   misplaced = 4
#' )
filter_guess <- function(dictionary,
                         guess,
                         target = NULL,
                         correct = NULL,
                         incorrect = NULL,
                         misplaced = NULL,
                         hide = TRUE) {

  guess_letters <- parse_word(guess)

  if(!is.null(target)) {

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
    correct_words <- dictionary
  } else {
    correct_words <- filter_correct(dictionary, guess_letters[correct], correct)
  }

  if(is.null(incorrect)) {
    incorrect_words <- dictionary
  } else {
    incorrect_words <- filter_incorrect(dictionary, guess_letters[incorrect])
  }

  if(is.null(misplaced)) {
    misplaced_words <- dictionary
  } else {
    misplaced_words <- filter_misplaced(dictionary, guess_letters[misplaced], misplaced)
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
#' @param dictionary Vector of strings of possible words to match
#' @param guesses vector of character indicating sequential guesses
#' @param target Word to use as comparison to `guess` to determine
#' which letters are correct, incorrect, or misplaced.
#' @param hide If `TRUE`, return number of words remaining after filtering. If
#' `FALSE`, return vector of remaining words themselves.
#'
#' @return List of either a vector of words remaining after filtering or integer
#' indicating the number of words remaining.
#' @export
#'
#' @examples
#' all_words <- words::words$word
#' dictionary <- all_words[stringr::str_count(all_words) == 5]
#'
#' target <- "panic"
#' guess <- c("burst", "canoe")
#'
#' filter_game(dictionary, guess, target)
filter_game <- function(dictionary, guesses, target, hide = TRUE) {

  remaining <- list()

  for(guess in guesses) {
    dictionary <- filter_guess(dictionary, guess, target, hide = FALSE)

    if(hide) {
      remaining <- append(remaining, length(dictionary))
    } else {
      remaining <- append(remaining, list(dictionary))
    }
  }

  remaining

}
