test_that("explore_clustering - default parameters work correctly", {
  df <- create_test_data()
  results <- explore_clustering(df)
  expect_equal(length(results), 2)
  expect_equal(names(results), c("KMeans", "DBSCAN"))
  kmeans_results <- results$KMeans
  dbscan_results <- results$DBSCAN
  expect_equal(length(kmeans_results), length(seq(from = 2, to = 10)))
  expect_equal(length(dbscan_results), 1)

})

test_that("explore_clustering - invalid parameters work correctly", {
  df <- create_test_data()

  expect_error(explore_clustering(df, c(1, 2)))

  # invalid key for KMeans
  hyperparams <- list(c(1, 2), c(2))
  names(hyperparams) <- c("K_Means", "DBSCAN")
  expect_error(explore_clustering(df, hyperparams))

  # invalid key for DBSCAN
  names(hyperparams) <- c("KMeans", "DB_SCAN")
  expect_error(explore_clustering(df, hyperparams))

  # invalid value for KMeans and DBSCAN
  hyperparams <- c(1, 2)
  names(hyperparams) <- c("KMeans", "DBSCAN")
  expect_error(explore_clustering(df, hyperparams))

  kmeans_keys <- c("centers", "iter.max", "algorithm")
  dbscan_keys <- c("eps", "minPts")

  # invalid keys inside KMeans
  kmeans_hparams <- list(seq(from = 2, to = 10), 10, c("Lloyd"))
  names(kmeans_hparams)  <-  c("a", "b", "c")
  dbscan_hparams <- list(c(1), c(5))
  names(dbscan_hparams) <- dbscan_keys
  hyperparams  <-  list(kmeans_hparams, dbscan_hparams)
  names(hyperparams) <- c("KMeans", "DBSCAN")
  expect_error(explore_clustering(df, hyperparams))

  kmeans_hparams <- list(seq(from = 2, to = 10), 10, c("Lloyd"))
  names(kmeans_hparams)  <-  c("centers", "iter.max", "c")
  dbscan_hparams <- list(c(1), c(5))
  names(dbscan_hparams) <- dbscan_keys
  hyperparams  <-  list(kmeans_hparams, dbscan_hparams)
  names(hyperparams) <- c("KMeans", "DBSCAN")
  expect_error(explore_clustering(df, hyperparams))

  kmeans_hparams <- list(seq(from = 2, to = 10), 10, c("Lloyd"))
  names(kmeans_hparams)  <-  c("centers", "b", "c")
  dbscan_hparams <- list(c(1), c(5))
  names(dbscan_hparams) <- dbscan_keys
  hyperparams  <-  list(kmeans_hparams, dbscan_hparams)
  names(hyperparams) <- c("KMeans", "DBSCAN")
  expect_error(explore_clustering(df, hyperparams))

  # invalid keys inside DBSCAN
  kmeans_hparams <- list(seq(from = 2, to = 10), 10, c("Lloyd"))
  names(kmeans_hparams)  <-  kmeans_keys
  dbscan_hparams <- list(c(1), c(5))
  names(dbscan_hparams) <- c("a", "b")
  hyperparams  <-  list(kmeans_hparams, dbscan_hparams)
  names(hyperparams) <- c("KMeans", "DBSCAN")
  expect_error(explore_clustering(df, hyperparams))

  kmeans_hparams <- list(seq(from = 2, to = 10), 10, c("Lloyd"))
  names(kmeans_hparams)  <-  kmeans_keys
  dbscan_hparams <- list(c(1), c(5))
  names(dbscan_hparams) <- c("eps", "b")
  hyperparams  <-  list(kmeans_hparams, dbscan_hparams)
  names(hyperparams) <- c("KMeans", "DBSCAN")
  expect_error(explore_clustering(df, hyperparams))

  # invalid value inside KMeans
  kmeans_hparams <- list(seq(from = 0, to = 10), 10, c("Lloyd"))
  names(kmeans_hparams)  <-  kmeans_keys
  dbscan_hparams <- list(c(1), c(5))
  names(dbscan_hparams) <- dbscan_keys
  hyperparams  <-  list(kmeans_hparams, dbscan_hparams)
  names(hyperparams) <- c("KMeans", "DBSCAN")
  expect_error(explore_clustering(df, hyperparams))

})

test_that("explore_clustering - custom parameters work correctly", {
  df <- create_test_data()

  kmeans_keys <- c("centers", "iter.max", "algorithm")
  dbscan_keys <- c("eps", "minPts")

  kmeans_hparams <- list(seq(from = 3, to = 10), 10, c("Hartigan-Wong"))
  names(kmeans_hparams)  <-  kmeans_keys
  dbscan_hparams <- list(c(1), c(6))
  names(dbscan_hparams) <- dbscan_keys
  hyperparams  <-  list(kmeans_hparams, dbscan_hparams)
  names(hyperparams) <- c("KMeans", "DBSCAN")
  results <- explore_clustering(df, hyperparams)
  expect_equal(length(results), 2)
  expect_equal(names(results), c("KMeans", "DBSCAN"))
  kmeans_results <- results$KMeans
  dbscan_results <- results$DBSCAN
  expect_equal(length(kmeans_results), length(seq(from = 3, to = 10)))
  expect_equal(length(dbscan_results), 1)

})
