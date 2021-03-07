#' This function creates common exploratory analysis visualizations on numeric columns in the dataset which is provided to it.
#'
#' @param df The dataframe for which exploratory analysis is to be carried out
#' @param hist_cols A list. If passed, it will limit histograms to a subset of columns
#' @param pairplot_cols A list. If passed, it will limit pairplots to a subset of columns
#' @param corr_method Chooses the metric for correlation. Default value is 'pearson'. Possible values are 'pearson', 'kendall', 'spearman'
#'
#' @return a list of plot objects generated created by the function
#'
#' @examples
explore_numeric_columns <- function(df, hist_cols=NULL, pairplot_cols=NULL, corr_method='pearson'){

}
