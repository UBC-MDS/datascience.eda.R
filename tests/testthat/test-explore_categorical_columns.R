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

  expect_error(explore_text_columns(data, 5))
  expect_error(explore_text_columns(data, 'col'))

  vdiffr::expect_doppelganger("test_12", cat_op[[2]][[2]])
  vdiffr::expect_doppelganger("test_110", cat_op[[1]][[10]])

  expect_equal(c("column_name", "unique_items", "no_of_nulls", "percentage_missing"), as.vector(names(cat_op[[1]])))
  expect_equal(c(11,4), as.vector(dim(cat_op[[1]])))
  expect_equal(c('no_of_nulls'), as.vector(cat_op[[1]][,3] %>% names()))
  }
)
