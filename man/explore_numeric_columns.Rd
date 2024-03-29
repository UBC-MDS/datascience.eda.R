% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/explore_numeric_columns.R
\name{explore_numeric_columns}
\alias{explore_numeric_columns}
\title{Performs EDA of numeric features}
\usage{
explore_numeric_columns(
  df,
  hist_cols = NULL,
  pairplot_cols = NULL,
  corr_method = "pearson"
)
}
\arguments{
\item{df}{The dataframe for which exploratory analysis is to be carried out}

\item{hist_cols}{A list. If passed, it will limit histograms to a subset of columns}

\item{pairplot_cols}{A list. If passed, it will limit pairplots to a subset of columns}

\item{corr_method}{Chooses the metric for correlation. Default value is 'pearson'. Possible values are 'pearson', 'kendall', 'spearman'}
}
\value{
a list of plot objects generated created by the function
}
\description{
This function creates common exploratory analysis visualizations on numeric columns in the dataset which is provided to it.
}
\examples{
library(palmerpenguins)
results <- explore_numeric_columns(penguins)
}
