
#' Request word guess input from user
#'
#' @param n_letters Required number of letters in guess word
#' @param init Whether this is the initial prompt of the Wordle game.
#' Determines which output to display to user when requesting guess.
#'
#' @return input word
prompt_for_guess <- function(n_letters = 5, init = TRUE) {

  if(init) {
    guess <- readline("Make a guess! > ")
  } else {
    guess <- readline("> ")
  }

  is_valid <- validate_guess(guess, n_letters)

  if(!is_valid$correct_length) {
    message(glue::glue("Your entry was not {n_letters} letters long!"))
    guess <- prompt_for_guess(n_letters, init = FALSE)
  } else if(!is_valid$in_dict) {
    message(glue::glue("Your entry is not recognized as a valid word."))
    guess <- prompt_for_guess(n_letters, init = FALSE)
  }

  invisible(guess)

}

#' Compare user input to target word
#'
#' Prompt user for input and provide feedback. Continue to do so until
#' game is won or exited.
#'
#' @param target Target word of the game
#' @param n_letters Number of letters of the words being used in this game
#' @param init Whether this is the initial guess of the game
check_guess <- function(target, n_letters, init = TRUE) {

  guess <- prompt_for_guess(n_letters, init = init)

  if(target == guess) {

    make_guess(target, guess)
    message("\n\nVICTORY!")
    return(invisible())

  } else {

    make_guess(target, guess)

    guess <- check_guess(target, n_letters, init = FALSE)

  }

}

#' Play a game of Wordle
#'
#' Selects a random target word of specified length and prompts user
#' for input words. Input words are compared to target word and feedback
#' is given to user indicating which words are correct, incorrect, or
#' misplaced.
#'
#' @param n_letters Length of target word
#'
#' @export
#'
#' @importFrom dplyr %>%
play_wordle <- function(n_letters = 5) {

  message(
    glue::glue(
      "You\'re playing a game of Wordle with {n_letters} letter words."
    )
  )

  if(n_letters != 5) {

    dictionary <- words::words %>%
      dplyr::filter(stringr::str_count(word) == n_letters) %>%
      dplyr::pull(word)

  } else {

    dictionary <- target_dictionary

  }

  target <- sample(dictionary, 1)
  # cat(target)

  check_guess(target, n_letters)

}

#' Validate a Wordle guess
#'
#' @param guess Guessed word
#' @param n_letters Required number of letters for the guess
#'
#' @return List of logical values for each word check
validate_guess <- function(guess, n_letters = 5) {

  guess <- clean_entry(guess)

  correct_length <- stringr::str_count(guess) == n_letters
  in_dict <- guess %in% c(target_dictionary, guess_dictionary)

  list(
    correct_length = correct_length,
    in_dict = in_dict
  )

}
