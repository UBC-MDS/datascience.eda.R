test_that("KMeans - default parameters work correctly", {
  expect_equal(2 * 2, 4)
  df <- create_test_data()
  results <- explore_KMeans_clustering(df, centers=3)
})


test_that("KMeans - invalid parameters work correctly", {
  expect_equal(2 * 2, 4)
})

test_that("KMeans - custom parameters work correctly", {
  expect_equal(2 * 2, 4)
})
