
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
      "You\'re playing a game of Wordle with {n_letters} letter words!\n\n",
      "Type your word and press [enter] to submit.\n",
      "Press [enter] without any letters to quit.\n"
    )
  )

  if(n_letters != 5) {

    dictionary <- words::words %>%
      dplyr::filter(stringr::str_count(word) == n_letters) %>%
      dplyr::pull(word)

    target <- sample(dictionary, 1)

  } else {

    target <- sample(target_dictionary, 1)

  }

  check_guess(target, n_letters)

}


#' Compare user input to target word
#'
#' Prompt user for input and provide feedback. Continue to do so until
#' game is won, lost, or exited.
#'
#' @param target Target word of the game
#' @param n_letters Number of letters of the words being used in this game
#' @param init Whether this is the initial guess of the game
#' @param remaining Number of tries remaining before game loss
check_guess <- function(target, n_letters = 5, init = TRUE, remaining = 6) {

  guess <- prompt_for_guess(n_letters, init = init)

  if(guess == "") {

    message("Quitting? :(\n")
    make_guess(target, target)
    return(invisible())

  }

  if(target == guess) {

    make_guess(target, guess)
    message("\n\nVictory!")
    return(invisible())

  } else {

    make_guess(target, guess)

    if(remaining != 1) {
      guess <- check_guess(target, n_letters, init = FALSE, remaining = remaining - 1)
    } else {
      message("\nToo bad...The word was:\n")
      make_guess(target, target)
    }

  }

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
make_guess <- function(target, guess, quiet = FALSE) {

  result <- list(
    correct = find_correct(target, guess),
    incorrect = find_incorrect(target, guess),
    misplaced = find_misplaced(target, guess)
  )

  guess_letters <- toupper(parse_word(clean_entry(guess)))

  if(!quiet) {

    guess_letters[result$correct] <- crayon::green$bold(
      guess_letters[result$correct]
    )

    guess_letters[result$incorrect] <- crayon::red$bold(
      guess_letters[result$incorrect]
    )

    guess_letters[result$misplaced] <- crayon::yellow$bold(
      guess_letters[result$misplaced]
    )

    cat(paste0(guess_letters, collapse = " "))

  }

  invisible(result)

}

#' Validate a Wordle guess
#'
#' @param guess Guessed word
#' @param n_letters Required number of letters for the guess
#'
#' @return List of logical values for each word check
validate_guess <- function(guess, n_letters = 5) {

  if(n_letters == 5) {
    dict <- dictionary
  } else {
    dict <- words::words %>%
      dplyr::filter(stringr::str_count(word) == n_letters) %>%
      dplyr::pull(word)
  }

  guess <- clean_entry(guess)

  correct_length <- stringr::str_count(guess) == n_letters
  in_dict <- guess %in% dict

  list(
    correct_length = correct_length,
    in_dict = in_dict
  )

}

#' Request word guess input from user
#'
#' @param n_letters Required number of letters in guess word
#' @param init Whether this is the initial prompt of the Wordle game.
#' Determines which output to display to user when requesting guess.
#'
#' @return input word
prompt_for_guess <- function(n_letters = 5, init = TRUE) {

  if(init) {
    guess <- readline("Make a guess! >> ")
  } else {
    guess <- readline(">> ")
  }

  if(guess == "") {
    return(invisible(guess))
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
