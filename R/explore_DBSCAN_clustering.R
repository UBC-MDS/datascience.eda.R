#' fit and plot DBSCAN clustering on numeric columns of the dataset
#'
#' @param df dataframe: dataset
#' @param num_cols vector: a vector of numeric column names
#' @param eps vector: a list of the epsilon neighborhood size
#' @param minPts vector: a list of number of minimum points in the eps region (for core points). Default is c(5).
#' @param weights numeric: weights for the data points. Only needed to perform weighted clustering.
#'
#' @return
#' @export
#'
#' @examples
explore_DBSCAN_clustering <- function(df, num_cols=NULL, eps=c(1), minPts = c(5), weights = NULL){

}
