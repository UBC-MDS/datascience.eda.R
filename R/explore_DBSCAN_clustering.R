#' fit and plot DBSCAN clustering on numeric columns of the dataset
#'
#' @param df dataframe: dataset
#' @param eps vector of the epsilon neighborhood size, optional, default to c(1)
#' @param minPts vector of number of minimum points in the eps region (for core points), default to c(5).
#'
#' @return a list of plots
#' @export
#'
#' @examples
#' library(palmerpenguins)
#' explore_DBSCAN_clustering(penguins)
explore_DBSCAN_clustering <- function(df, eps=c(1), minPts = c(5)){
  numeric_cols <-  dplyr::select_if(df, is.numeric)

  # verify inputs
  if (length(numeric_cols) == 0)
    stop("There is no numeric columns to be clustered.")

  if (!is.numeric(eps))
    stop("Invalid value for eps.")

  if (!is.numeric(minPts))
    stop("Invalid value for minPts.")

  # scale and impute the dataset before applying DBSCAN
  numeric_cols <- imputeR::guess(numeric_cols)
  numeric_cols <- scale(numeric_cols)

  N <- length(eps) * length(minPts)
  results <- vector("list", N)

  i = 1
  pca = stats::prcomp(numeric_cols, scale=FALSE)
  for (e in eps){
    for (m in minPts){
      dbo <- dbscan::dbscan(numeric_cols, eps=e, minPts = m)
      results[[i]] = (ggbiplot::ggbiplot(
        pca,
        groups = as.factor(dbo$cluster),
        choices = c(1, 2),
        ellipse = FALSE,
        var.axes = FALSE
      ) + ggplot2::ggtitle(paste0("PCA Plot - DBSCAN with eps=", e,", minPts=",m)))
      i <- i + 1
    }
  }
  results
}
