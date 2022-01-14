test_that("find_words uses proper regex", {
  expect_equal(find_words(c("a", "b", "c"), "^[ab]"), c("a", "b"))
})

test_that("find_words negate argument works", {
  expect_equal(find_words(c("a", "b", "c"), "^[ab]", negate = TRUE), c("c"))
})
