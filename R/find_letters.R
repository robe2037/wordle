
#' Determine letter categories for a Wordle guess
#'
#' Compare a guess word to a known target word and determine the index locations
#' for all correct, incorrect, and misplaced letteres in the guess. Guess word
#' and target word must have the same number of letters.
#'
#' @param target target of the match
#' @param guess word to be compared to the target word
#'
#' @name find_letters
#'
#' @return integer vector
#'
#' @examples
#' find_misplaced("torch", "trick")
#' find_correct("torch", "trick")
#' find_incorrect("torch", "trick")
NULL

#' @rdname find_letters
#' @export
find_correct <- function(target, guess) {

  stopifnot(stringr::str_count(target) == stringr::str_count(guess))

  i <- which(parse_word(target) == parse_word(guess))

  if(length(i) == 0) {
    return(NULL)
  }

  i

}

#' @rdname find_letters
#' @export
find_incorrect <- function(target, guess) {

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
#' @export
find_misplaced <- function(target, guess) {

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
#' Compare a guess word to a target word and return index positions
#' of correct, incorrect, and misplaced letters. Optionally print guessed
#' letters to console with colors indicating whether letters were correct,
#' incorrect, or misplaced.
#'
#' @param target Target of the match
#' @param guess Word to be compared to the target word
#' @param quiet Should guess results be printed to console in the form of color-
#' coded guess letters?
#'
#' @return List of length 3. Each list element is a vector of integers indicating
#' index positions for the letters in the guess word in the associated category.
#' @export
#'
#' @examples
#' result <- make_guess("torch", "trick")
#' str(result)
make_guess <- function(target, guess, quiet = FALSE) {

  result <- list(
    correct = find_correct(target, guess),
    incorrect = find_incorrect(target, guess),
    misplaced = find_misplaced(target, guess)
  )

  guess_letters <- toupper(parse_word(guess))

  if(!quiet) {

    guess_letters[result$correct] <- crayon::green$bold(guess_letters[result$correct])
    guess_letters[result$incorrect] <- crayon::red$bold(guess_letters[result$incorrect])
    guess_letters[result$misplaced] <- crayon::yellow$bold(guess_letters[result$misplaced])

    cat(paste0(guess_letters, collapse = " "))

  }

  invisible(result)

}
