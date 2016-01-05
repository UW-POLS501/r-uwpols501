if (requireNamespace("lintr", quietly = TRUE)) {
  context("lintr")
  test_that("Code conforms to the package style", {
    lintr::expect_lint_free()
  })
}
