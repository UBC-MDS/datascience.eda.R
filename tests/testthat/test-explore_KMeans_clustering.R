test_that("KMeans - default parameters work correctly", {
  df <- create_test_data()
  results <- explore_KMeans_clustering(df)
  expect_equal(length(results), 9)
  centers <- seq(2, 10)
  for (i in seq_along(centers)) {
    p <- results[[i]]
    expect_equal(p$labels$title, paste0("PCA Plot - KMeans with", centers[i]," centers"))
    expect_equal(nlevels(p$data$groups), centers[i])
  }
})


test_that("KMeans - invalid parameters work correctly", {
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
})

test_that("KMeans - custom parameters work correctly", {
  df <- create_test_data()
  results <- explore_KMeans_clustering(df, seq(3, 5), iter.max = 15,
                            algorithm = "Lloyd")
})
