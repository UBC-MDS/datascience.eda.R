require(tidyverse)

test_that('Failed',{

  df <- read_csv('data/menu_subset.csv')
  expect_error(explore_numeric_columns(1))
  expect_error(explore_numeric_columns(df, hist_cols = 1))
  expect_error(explore_numeric_columns(df, pairplot_cols = 1))
  expect_error(explore_numeric_columns(df, corr_method = 1))
  expect_error(explore_numeric_columns(df, hist_cols = 'abc'))
  expect_error(explore_numeric_columns(df, pairplot_cols = 'abc'))

  # Check if results are correct when no optional hyper-parameters are provided)
  results <- explore_numeric_columns(df)

  # Test histograms
  expect_equal(results$hist$labels$x, "Calories")
  expect_equal(results$hist$labels$y, "count")
  expect_equal(results$hist$labels$weight, "weight")
  expect_equal(results$hist$labels$title, "Histogram for column: Calories")
  expect_equal(typeof(results$hist$theme) , 'list')

  # Test GGally pairplot
  expect_equal(results$pair$byrow, TRUE)
  expect_equal(results$pair$ncol, 4)
  expect_equal(results$pair$nrow, 4)
  expect_equal(results$pair$xAxisLabels[1], "Calories")
  expect_equal(results$pair$xAxisLabels[4], "Carbohydrates")
  expect_equal(results$pair$yAxisLabels[1], "Calories")
  expect_equal(results$pair$yAxisLabels[4], "Carbohydrates")

  # Test correlation heat-map
  expect_equal(results$corr$labels$title, "Correlation between the different numeric features")
  expect_equal(results$corr$labels$y, "Feature 2")
  expect_equal(results$corr$labels$x, "Feature 1")
  expect_equal(results$corr$labels$fill, "value")
  expect_equal(typeof(results$corr$theme) , 'list')


  # Check if results are correct when optional hyper-parameters are provided)
  results <- explore_numeric_columns(df, hist_cols = c('Cholesterol'), pairplot_cols = c('Calories','Carbohydrates'), corr_method = 'spearman')

  # Test histograms
  expect_equal(results$hist$labels$x, "Cholesterol")
  expect_equal(results$hist$labels$y, "count")
  expect_equal(results$hist$labels$weight, "weight")
  expect_equal(results$hist$labels$title, "Histogram for column: Cholesterol")
  expect_equal(typeof(results$hist$theme) , 'list')

  # Test GGally pairplot
  expect_equal(results$pair$byrow, TRUE)
  expect_equal(results$pair$ncol, 2)
  expect_equal(results$pair$nrow, 2)
  expect_equal(results$pair$xAxisLabels[1], "Calories")
  expect_equal(results$pair$xAxisLabels[2], "Carbohydrates")
  expect_equal(results$pair$yAxisLabels[1], "Calories")
  expect_equal(results$pair$yAxisLabels[2], "Carbohydrates")

  # Test correlation heat-map
  expect_equal(results$corr$labels$title, "Correlation between the different numeric features")
  expect_equal(results$corr$labels$y, "Feature 2")
  expect_equal(results$corr$labels$x, "Feature 1")
  expect_equal(results$corr$labels$fill, "value")
  expect_equal(typeof(results$corr$theme) , 'list')

})
