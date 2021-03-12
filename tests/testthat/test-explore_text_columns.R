library(tidyverse)

test_that("multiplication works", {

  df <- read_csv("data/spam.csv", locale(encoding = "latin1"),
                 col_names=c("target", "sms", "col3", "col4", "col5"),
                 col_types=NULL)

  df <- df %>% select("target", "sms")

  results <- explore_text_columns(df, text_cols="sms")

  expect_equal(results[[1]], 80.07)
  expect_equal(results[[2]], 61)

})
