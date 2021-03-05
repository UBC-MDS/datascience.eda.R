#' fits and plots KMeans clusterings on numeric columns of the dataset
#'
#' @param df dataframe
#' @param num_cols vector of numeric column names
#' @param centers vector of number of clusters
#' @param iter.max integer: max number of iteration
#' @param algorithm character: may be abbreviated. Note that "Lloyd" and "Forgy" are alternative names for one algorithm
#'
#' @return
#' @export
#'
#' @examples
explore_KMeans_clustering <-
  function(df, num_cols = NULL,
           centers,
           iter.max = 10,
           algorithm = c("Hartigan-Wong", "Lloyd", "Forgy",
                         "MacQueen")
           ) {

  }
