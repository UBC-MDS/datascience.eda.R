#' Performs EDA of categorical features.
#'
#' Creates a data frame containing column names and corresponding details about unique values, null values and most frequent category in every column
#' Plots count-plots for given categorical columns
#'
#' @param df input data as a data frame
#' @param categorical_cols vector containing categorical columns
#'
#' @return A list object with first list element being a tibble with details about unique, null values and most frequent category in every column
#' and a second list element being count plots of user provided column names
#' @export
#'
#' @examples
#' library(dplyr)
#' library(MASS)
#' library(knitr)
#' df <- data.frame(lapply(survey[, c('Sex','Clap')], as.character), stringsAsFactors=FALSE) %>% tibble()
#' results <- explore_categorical_columns(df, c('Sex','Clap'))
#' results[[1]] %>% knitr::kable()
#' results[[2]][[1]]
#' results[[2]][[2]]

explore_categorical_columns <- function(df, categorical_cols){

  # exception if categorical_cols is not  passed as a char vector
  if (!is.character(categorical_cols)) {
    stop("Provided column(s) is not a character vector")
  }

  # exception if df passed is not a data.frame
  if (!is.data.frame(df)) {
    stop("Provided data is not of type data.frame")
  }

  for (col in categorical_cols){
    # exception if a column name passed is not a column in the dataframe
    if (!(col %in% colnames(df))){
      stop(paste0(col, " passed in categorical_cols is not available in the dataframe"))
    }
    # exception if a column name passed is not of character datatype
    if (!is.character(df %>% dplyr::pull(col))){
      stop(paste0(col, " passed in categorical_cols does not belong to character data type"))
    }
  }

  plot_list <- list()
  percentage_missing <- NULL

  # Creating tibble
  unq_tbl <- tibble::tibble(unique_items = purrr::map_chr(as.vector(sapply(categorical_cols,
                                                                   function(x) (df[, x] %>% unique()))), toString))

  cat_df <- tibble::tibble(
    column_name = categorical_cols,
    unique_items = unq_tbl$unique_items,
    no_of_nulls = apply(df[ ,categorical_cols], 2, function(col) sum(is.na(col))),
    percentage_missing = apply(df[ ,categorical_cols], 2, function(col) round(sum(is.na(col))/length(col)*100,3)),
  ) %>% dplyr::arrange(dplyr::desc(percentage_missing))

  #Creating plots
  for (col in seq_along(categorical_cols)){
    plot_list[[col]]<- ggplot2::ggplot(df) +
      ggplot2::aes(forcats::fct_infreq((!!dplyr::sym(df[,col] %>% names())))) +
      ggplot2::geom_bar(stat = 'count') +
      ggplot2::xlab(categorical_cols[col]) +
      ggplot2::theme(axis.text.x=ggplot2::element_text(angle=90))
  }

  # Returned list containing tibble and plots
  cat_op <- list(cat_df, plot_list)
}
