test_that("find_words uses proper regex", {
  expect_equal(find_words("^[ab]", c("a", "b", "c"), ), c("a", "b"))
})

test_that("find_words negate argument works", {
  expect_equal(find_words("^[ab]", c("a", "b", "c"), neg = TRUE), c("c"))
})
