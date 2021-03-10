test_that("explore_KMeans_clustering - default parameters work correctly", {
  df <- create_test_data()
  results <- explore_KMeans_clustering(df)
  expect_equal(length(results), 9)
  centers <- seq(2, 10)
  for (i in seq_along(centers)) {
    p <- results[[i]]
    verify_KMeans_plot(p, centers[i])
  }
})


test_that("explore_KMeans_clustering - invalid parameters work correctly", {
  df <- create_test_data()

  # there is no numeric columns
  char_cols <- df %>% dplyr::select_if(is.character)
  expect_error(explore_KMeans_clustering(char_cols))

  # invalid number of clusters
  expect_error(explore_KMeans_clustering(df,"a"))
  expect_error(explore_KMeans_clustering(df,0))
  expect_error(explore_KMeans_clustering(df,c(0, 2)))

  # invalid algorithm
  expect_error(explore_KMeans_clustering(df, algorithm = "test"))

  # invalid iter.max
  expect_error(explore_KMeans_clustering(df, iter.max = 3.2))
})

test_that("explore_KMeans_clustering - custom parameters work correctly", {
  df <- create_test_data()
  centers = seq(3, 5)
  results <- explore_KMeans_clustering(df, centers = centers, iter.max = 15,
                            algorithm = "Lloyd")
  expect_equal(length(results), length(centers))

  for (i in seq_along(centers)) {
    p <- results[[i]]
    verify_KMeans_plot(p, centers[i])
  }
})
