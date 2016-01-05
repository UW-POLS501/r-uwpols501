context("lintr")

if (requireNamespace("lintr", quietly = TRUE)) {
  test_that("Code conforms to the package style", {
    lintr::expect_lint_free()
  })
}
