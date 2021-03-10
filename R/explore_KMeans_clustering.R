#' fits and plots KMeans clusterings on numeric columns of the dataset
#'
#' @param df dataframe
#' @param centers vector of number of clusters
#' @param iter.max integer: max number of iteration
#' @param algorithm character: may be abbreviated. Note that "Lloyd" and "Forgy" are alternative names for one algorithm
#'
#' @return a list of plots
#'
#' @examples
#' \dontrun{
#' library(palmerpenguins)
#' explore_KMeans_clustering(penguins)
#' }

explore_KMeans_clustering <-
  function(df,
           centers = seq(from = 2, to = 10),
           iter.max = 10,
           algorithm = c("Hartigan-Wong", "Lloyd", "Forgy",
                         "MacQueen"))
  {
    numeric_cols <-  dplyr::select_if(df, is.numeric)

    # verify inputs
    if (length(numeric_cols) == 0)
      stop("There is no numeric columns to be clustered.")

    if (!is.numeric(centers))
      stop("Centers must be a numeric vector.")

    for (c in centers)
      if (c<1)
        stop("Centers must be >= 1.")

    for (a in algorithm){
      if (!(a %in% c("Hartigan-Wong", "Lloyd", "Forgy", "MacQueen")))
        stop("Invalid value for algorithm.")
    }


    # if (!is.numeric(iter.max) | !all.equal(iter.max, as.integer(iter.max)) | iter.max<1)
    #   stop("Invalid value for iter.max.")

    # scale and impute the dataset before applying KMeans
    numeric_cols <- imputeR::guess(numeric_cols)
    numeric_cols <- scale(numeric_cols)

    N = length(centers)
    results <- vector("list", N)
    pca = stats::prcomp(numeric_cols, scale=FALSE)
    for (i in seq_along(centers)) {
      cl = stats::kmeans(numeric_cols, centers = centers[i], algorithm = algorithm, iter.max = iter.max)
      results[[i]] = (ggbiplot::ggbiplot(
        pca,
        groups = as.factor(cl$cluster),
        choices = c(1, 2),
        ellipse = TRUE,
        var.axes = FALSE
      ) + ggplot2::ggtitle(paste0("PCA Plot - KMeans with", centers[i]," centers")))
    }
    results
  }
