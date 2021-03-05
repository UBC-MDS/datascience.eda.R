#' fits and plots KMeans clusterings on numeric columns of the dataset
#'
#' @param df dataframe: dataset
#' @param num_cols vector: a vector of numeric column names
#' @param centers vector: a vector of number of clusters
#' @param iter.max integer: max number of iteration
#' @param algorithm character: may be abbreviated. Note that "Lloyd" and "Forgy" are alternative names for one algorithm
#'
#' @return
#' @export
#'
#' @examples
explire_KMeans_clustering <-
  function(df, num_cols = NULL,
           centers,
           iter.max = 10,
           algorithm = c("Hartigan-Wong", "Lloyd", "Forgy",
                         "MacQueen")
           ) {

  }
