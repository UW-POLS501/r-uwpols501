context("coalesce")

test_that("coalesce works as expected", {
  x <- c(NA, 1, NA, NA, 4, 6, NA, 10)
  y <- c(NA, NA, 2, NA, 5, NA, 8, 11)
  z <- c(NA, NA, NA, 3, NA, 7, 9, 12)
  expect_equal(coalesce(x, y, z), c(NA, 1, 2, 3, 4, 6, 8, 10))
})
