#' This function creates common exploratory analysis visualizations on numeric columns in the dataset which is provided to it.
#'
#' @param df The dataframe for which exploratory analysis is to be carried out
#' @param hist_cols A list. If passed, it will limit histograms to a subset of columns
#' @param pairplot_cols A list. If passed, it will limit pairplots to a subset of columns
#' @param corr_method Chooses the metric for correlation. Default value is 'pearson'. Possible values are 'pearson', 'kendall', 'spearman'
#'
#' @return a list of plot objects generated created by the function
#' @export
#'
#' @examples
#' library(palmerpenguins)
#' explore_numeric_columns(penguins)
explore_numeric_columns <- function(df, hist_cols=NULL, pairplot_cols=NULL, corr_method='pearson'){

  if (!is.data.frame(df)){
    stop('Value provided for df is not a data frame')
  }

  if (!is.null(hist_cols) & !is.vector(hist_cols, mode='character')){
    stop('Value provided for hist_cols is not a vector')
  }

  if (!is.null(pairplot_cols) & !is.vector(pairplot_cols, mode='character')){
    stop('Value provided for pairplot_cols is not a vector')
  }

  if (!is.null(corr_method) & !is.character(corr_method)){
    stop('Value provided for hist_cols is not a vector')
  }

  results <- list(hist = list() , pair = list() , corr = list())
  hist_temp <- c()
  df_numeric_all <- dplyr::select_if(df, is.numeric)
  if (is.null(hist_cols)){
    df_numeric_data <- dplyr::select_if(df, is.numeric)
    for (i in names(df_numeric_data)){
      hist_plot <- ggplot2::ggplot(df_numeric_data, ggplot2::aes_string(x=i))+ ggplot2::geom_histogram() + ggplot2::ggtitle(paste0('Histogram for column: ',i))
      print(hist_plot)
      results$hist <- append(results$hist, hist_plot)
    }
  }

  else{
    for (col in hist_cols){
      if (!col %in% names(df_numeric_all)){
        stop('Column specified in hist_cols is not present in the dataframe')
      }
    }
    df_numeric_data <- df %>% dplyr::select(hist_cols)
    for (i in names(df_numeric_data)){
      hist_plot <- ggplot2::ggplot(df_numeric_data, ggplot2::aes_string(x=i))+ ggplot2::geom_histogram() + ggplot2::ggtitle(paste0('Histogram for column: ',i))
      print(hist_plot)
      results$hist <- append(results$hist, hist_plot)

    }
  }


  if (is.null(pairplot_cols)){
    df_numeric_data <- dplyr::select_if(df, is.numeric)
    corr_pairplot <- GGally::ggpairs(df_numeric_data, title = 'Pairplot of all columns')
  }

  else{
    for (col in hist_cols){
      if (!col %in% names(df_numeric_all)){
        stop('Column specified in hist_cols is not present in the dataframe')
      }
    }
    df_numeric_data <- df %>% dplyr::select(pairplot_cols)
    corr_pairplot <- GGally::ggpairs(df_numeric_data, columns = pairplot_cols,title = 'Pairplots for the provided columns')
  }
  print(corr_pairplot)
  results$pair <- append(results$pair, corr_pairplot)

  df_numeric_all <- dplyr::select_if(df, is.numeric)
  cormat <- round(stats::cor(df_numeric_all),3)
  cormat[upper.tri(cormat)] <- NA
  melted_cormat <- reshape2::melt(cormat, use='na.or.complete')
  corr_plot <- ggplot2::ggplot(data = melted_cormat, ggplot2::aes_string(x='Var1', y='Var2', fill='value')) +
    ggplot2::geom_tile() + ggplot2::scale_fill_gradient2(low = "blue", high = "red", mid = "white",
                                                         midpoint = 0, limit = c(-1,1), space = "Lab") +
    ggplot2::xlab('Feature 1') + ggplot2::ylab('Feature 2') + ggplot2::ggtitle('Correlation between the different numeric features')
  print(corr_plot)
  results$corr <- append(results$corr, corr_plot)

  results
}
