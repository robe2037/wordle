
<!-- README.md is generated from README.Rmd. Please edit that file -->

# wordle

<!-- badges: start -->
<!-- badges: end -->

Implementation of Wordle in R. Designed primarily for package
development practice and for personal Wordle data tracking.

## Installation

You can install the development version of wordle from
[GitHub](https://github.com/) with:

``` r
# install.packages("devtools")
devtools::install_github("robe2037/wordle")
```

## Example

To play a game of Wordle:

``` r
library(wordle)

play_wordle()
```

To explore how the universe of possible words changes with each guess,
use `filter_guess()`.

This will indicate the number of words remaining when provided with a
guessed word and the locations of the incorrect, correct, and misplaced
letters in that guess.

``` r
filter_guess(
  target_dictionary,
  guess = "flour", 
  correct = c(1, 5), 
  misplaced = 3,
  incorrect = c(2, 4), 
  hide = FALSE
)
```

`filter_guess()` can also be used with a known target word to
automatically determine the locations of the incorrect, correct, and
misplaced letters in the guess. Setting `hide = FALSE` will show the
words that remain rather than the number remaining.

``` r
filter_guess(
  target_dictionary,
  guess = "flock", 
  target = "quack",
  hide = FALSE
)
```

To get stats for an entire game, use `filter_game()`:

``` r
filter_game(
  target_dictionary,
  guesses = c("flock", "crack", "quack"), 
  target = "quack",
  hide = TRUE
)
```
