#' Performs EDA of categorical features.
#'
#' Creates a data frame containing column names and corresponding details about unique values, null values and most frequent category in every column
#' Plots count-plots for given categorical columns
#' Create a new factor from two existing factors, where the new factor's levels
#' are the union of the levels of the input factors.
#'
#' @param df input data as a data frame
#' @param categorical_col vector containing categorical columns
#'
#' @return A data frame with details about unique, null values and most frequent category in every column and a vector containing plots
#' @export
#' @examples
#' explore_categorical_columns(X, c('col1', 'col2'))

explore_categorical_columns <- function(df, categorical_col) {
  print('')
  # A data frame with details about unique, null values and most frequent category in every column

  # Plot objects
}
