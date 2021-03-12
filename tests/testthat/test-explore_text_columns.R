library(tidyverse)

test_that("multiplication works", {

  df <- read_csv("data/spam.csv", locale(encoding = "latin1"),
                 col_names=c("target", "sms", "col3", "col4", "col5"),
                 col_types=NULL)

  df <- df %>% select("target", "sms")

  results <- explore_text_columns(df, text_cols="sms")

  expect_equal(results[[1]], 80.07)
  expect_equal(results[[2]], 61)
  expect_equal(results[[3]], 910)
  expect_equal(results[[4]], 2)
  expect_equal(results[[5]], df$sms[1086])
  expect_equal(results[[6]], "v2")
  expect_equal(results[[7]], "Ok")
  expect_equal(results[[8]], "Ok")
  expect_equal(results[[9]], ":)")
  expect_equal(results[[10]], "Ok")
  expect_equal(results[[11]], "Ok")
  vdiffr::expect_doppelganger("Character_Length_Plot", results[[12]])
  expect_equal(results[[13]], 16.17)
  expect_equal(results[[14]], 12)
  expect_equal(results[[15]], 190)
  expect_equal(results[[16]], df$sms[1086])

})
