
#' Determine possible remaining words based on results of a Wordle guess
#'
#' @description Filter provided dictionary based on the logic resulting from
#' feedback after a single wordle guess.
#'
#' @param dictionary Character vector of words to filter based on result of comparing `guess` to `target`
#' @param target target of the match
#' @param guess word to be compared to the target word
#' @param hide If `TRUE`, return number of words remaining in dictionary after comparing
#' `target` to `guess`. Otherwise, return character vector of remaining words themselves
#'
#' @return If `hide = TRUE`, single integer. Otherwise, character vector
#' @export
#'
#' @examples
#' all_words <- words::words$word
#' dictionary <- all_words[stringr::str_count(all_words) == 5]
#'
#' check_remaining(dictionary, "torch", "trick", hide = TRUE)
check_remaining <- function(dictionary, target, guess, hide = TRUE) {

  if(target == guess) {
    return(NULL)
  }

  result <- make_guess(target, guess)
  guess <- parse_word(guess)

  correct <- guess[result$correct]
  incorrect <- guess[result$incorrect]
  misplaced <- guess[result$misplaced]

  if(!length(correct) == 0) {
    correct_words <- filter_correct(
      dictionary,
      letters = correct,
      positions = result$correct
    )
  } else {
    correct_words <- dictionary
  }

  incorrect_words <- filter_incorrect(
    dictionary,
    incorrect
  )

  if(!length(misplaced) == 0) {
    misplaced_words <- filter_misplaced(
      dictionary,
      letters = misplaced,
      positions = result$misplaced
    )
  } else {
    misplaced_words <- dictionary
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

#' Identify remaining words after multiple guesses for a single target word
#'
#' @description Given a provided dictionary, compare multiple guesses to a single
#' target word, returning the remaining available words in the dictionary at each
#' step based on the correct, incorrect, and misplaced letters identified after
#' comparing each guess to the target word.
#'
#' @param dictionary Character vector of words to filter based on result of comparing `guess` to `target`
#' @param target Target of the match
#' @param guesses Character vector of guesses (in order) to compare to target word
#' @param hide If `TRUE`, return list of integers indicating number of words
#' remaining in `dictionary` after each guess is compared to the target word. Otherwise,
#' return list of Character vectors of the words that remain.
#'
#' @return Either list of integers (if `hide = TRUE`) or character vectors (if `hide = FALSE`)
#' @export
#'
#' @examples
#' all_words <- words::words$word
#' dictionary <- all_words[stringr::str_count(all_words) == 5]
#'
#' target <- "tangy"
#' guesses <- c("plaid", "cream", "axons", "tangy")
#'
#' check_full_game(dictionary, target, guesses, hide = FALSE)
check_full_game <- function(dictionary, target, guesses, hide = TRUE) {

  remaining <- list()

  for(guess in guesses) {
    dictionary <- check_remaining(dictionary, target, guess, hide = FALSE)

    if(hide) {
      remaining <- append(remaining, length(dictionary))
    } else {
      remaining <- append(remaining, list(dictionary))
    }
  }

  remaining

}
