test_that("make_guess categorizes every letter of a word", {
  expect_equal(sort(unname(unlist(make_guess("hello", "world")))), 1:5)
  expect_equal(sort(unname(unlist(make_guess("hello", "hello")))), 1:5)
})

test_that("make_guess returns 3 elements", {
  expect_output(str(make_guess("hello", "world")), "List of 3")
  expect_output(str(make_guess("hello", "hello")), "List of 3")
})

test_that("make_guess returns NULL for missing categories", {
  expect_null(make_guess("hello", "cagey")$correct)
  expect_null(make_guess("hello", "gawks")$misplaced)
  expect_null(make_guess("hello", "hello")$incorrect)
})

test_that("make_guess returns integers", {
  expect_type(make_guess("hello", "world")$correct, "integer")
  expect_type(make_guess("hello", "world")$incorrect, "integer")
  expect_type(make_guess("hello", "world")$misplaced, "integer")
})

test_that("make_guess prints output when requested", {
  expect_output(make_guess("hello", "world"), "W O R L D")
  expect_silent(make_guess("hello", "world", quiet = TRUE))
})

test_that("make_guess ignores case", {
  expect_equal(make_guess("hello", "hAlLh", quiet = TRUE),
               make_guess("hello", "HALLH", quiet = TRUE))
})

test_that("make_guess ignores internal spaces", {
  expect_equal(make_guess("t a r   g e t", "te go at", quiet = TRUE),
               make_guess("target", "tegoat", quiet = TRUE))
})
