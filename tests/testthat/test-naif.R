context("naif")

test_that("naif works as expected", {
  x <- c(NA, 2, 3, 4)
  y <- c(1, 2, NA, 5)
  expect_equal(naif(x, y), c(NA, NA, NA, 4))
})
