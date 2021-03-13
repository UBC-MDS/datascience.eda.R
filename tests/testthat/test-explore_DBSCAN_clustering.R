test_that("explore_DBSCAN_clustering - default parameters work correctly", {
  df <- create_test_data()
  results <- explore_DBSCAN_clustering(df)
  expect_equal(length(results), 1)
  eps <- c(1)
  minPts <- c(5)
  N = length(eps)*length(minPts)
  i <-  1

  for (e in eps){
    for (m in minPts){
      p <- results[[i]]
      verify_DBSCAN_plot(p, e, m)
      i <- i + 1
    }
  }
})


test_that("explore_DBSCAN_clustering - invalid parameters work correctly", {
  df <- create_test_data()

  # there is no numeric columns
  char_cols <- df %>% dplyr::select_if(is.character)
  expect_error(explore_DBSCAN_clustering(char_cols))

  # invalid eps
  expect_error(explore_DBSCAN_clustering(df,eps="a"))
  expect_error(explore_DBSCAN_clustering(df,eps=c("a", 1)))

  # invalid minPts
  expect_error(explore_DBSCAN_clustering(df,minPts ="a"))
  expect_error(explore_DBSCAN_clustering(df,minPts =c("a", 1)))
})

test_that("explore_DBSCAN_clustering - custom parameters work correctly", {
  df <- create_test_data()
  eps = c(3, 5)
  minPts <- c(5, 10)
  results <- explore_DBSCAN_clustering(df, eps=eps, minPts = minPts)

  N = length(eps)*length(minPts)
  expect_equal(length(results), N)

  i <-  1

  for (e in eps){
    for (m in minPts){
      p <- results[[i]]
      verify_DBSCAN_plot(p, e, m)
      i <- i + 1
    }
  }
})
