require(tidyverse)
require(vdiffr)
require(forcats)
require(purrr)
require(ggplot2)

test_that("explore_categorical_columns gives out a tibble as well a plot object
          and handles exceptions gracefully", {

  data <- read_csv("data/cat_cols_data.csv")
  cols <- as.vector(data %>% names())
  cat_op <- explore_categorical_columns(data,cols)
  test_df <- read_csv("data/test_df.csv")

  expect_error(explore_text_columns(data, 5))
  expect_error(explore_text_columns(data, 'col'))

  vdiffr::expect_doppelganger("test_12", results[[1]][[2]])
  vdiffr::expect_doppelganger("test_110", results[[1]][[10]])

  expect_equal(test_df %>% names(), cat_op[[1]] %>% names())
  expect_equal(test_df %>% dim(), cat_op[[1]] %>% dim())
  expect_equal(test_df[,3], op[[1]][,3])
  }
)
