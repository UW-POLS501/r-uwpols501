context("filter_na")

library("dplyr")

test_that("filter_na works with no arguments", {
  df1 <- data_frame(alpha = c("a", "b", NA, NA, "d"),
                    bravo = c(1, NA, 2, NA, 3),
                    charlie = c(1, NA, NA, NA, NA)
                    )
  df2 <- data_frame(alpha = "a",
                    bravo = 1,
                    charlie = 1)
  df1_filtered <- filter_na(df1)
  expect_equivalent(df1_filtered, df2)
  expect_is(df1_filtered, c("tbl_df"))
})

test_that("filter_na works with selecting columns arguments", {
  df1 <- data_frame(alpha = c("a", "b", NA, NA, "d"),
                    bravo = c(1, NA, 2, NA, 3),
                    charlie = c(1, NA, NA, NA, NA)
  )
  df2 <- data_frame(alpha = c("a", "d"),
                    bravo = c(1, 3),
                    charlie = c(1, NA))
  df1_filtered <- filter_na(df1, -charlie)
  expect_equivalent(df1_filtered, df2)
  expect_is(df1_filtered, c("tbl_df"))
})

test_that("filter_na works with data.frame", {
  df1 <- data.frame(alpha = c("a", "b", NA, NA, "d"),
                    bravo = c(1, NA, 2, NA, 3),
                    charlie = c(1, NA, NA, NA, NA),
                    stringsAsFactors = FALSE)
  df2 <- data.frame(alpha = c("a"),
                    bravo = c(1),
                    charlie = c(1),
                    stringsAsFactors = FALSE)
  df1_filtered <- filter_na(df1)
  expect_equivalent(df1_filtered, df2)
  expect_is(df1_filtered, c("data.frame"))
})
