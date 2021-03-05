
<!-- README.md is generated from README.Rmd. Please edit that file -->

# datascience.eda

<!-- badges: start -->
<!-- badges: end -->

This package includes functions assisting data scientists with various
common tasks during the exploratory data analysis stage of a data
science project. Its functions will help the data scientist to do
preliminary analysis on common column types like numeric columns,
categorical columns and text columns; it will also conduct several
experimental clusterings on the dataset.

Our functions are tailored based on our own experience, there are also
similar packages published, a few good ones worth mentioning:

-   [EDA](https://cran.r-project.org/web/packages/dlookr/vignettes/EDA.html)
-   [SmartEDA](https://cran.r-project.org/web/packages/SmartEDA/vignettes/SmartEDA.html)
-   [DataExplorer](https://github.com/boxuancui/DataExplorer)

## Main functions

-   `explore_numeric_columns`: conducts common exploratory analysis on
    columns with numeric type: it generates a heatmap showing
    correlation coefficients (using `pearson`, `kendall` or `spearman`
    correlation on choice), histograms and SPLOM plots for all numeric
    columns or a list of columns specified by the user. This returns a
    list of plot objects so that the user can save and use them later
    on.

-   `explore_categorical_columns`: performs exploratory analysis on
    categorical features. It returns a dataframe containing column
    names, corresponding unique categories, counts of null values,
    percentages of null values and most frequent categories. It also
    generates and visualize countplots of a list of categorical columns
    of choice.

-   `explore_text_columns`: performs exploratory data analysis of text
    features. It prints the summary statistics of character length and
    word count. It also plots the word cloud, distributions of character
    lengths, word count and polarity and subjectivity scores. Bar charts
    of top n stopwords and top n words other than stopwords, top n
    bigrams, sentiments, name entities and part of speech tags will be
    visualized as well. This returns a list of plot objects.

-   `explore_clustering`: fits K-Means and DBSCAN clustering algorithms
    on the dataset and visualizes the clustering using scatterplots. It
    returns a dictionary with each key being name of the clustering
    algorithm and the value being a list of plots generated by the
    models.

-   `explore_KMeans_clustering`: fits K-Means clustering algorithms on
    the dataset and visualizes the clustering using scatterplots. It
    returns a list of plots.

-   `explore_DBSCAN_clustering`: fits K-DBSCAN clustering algorithms on
    the dataset and visualizes the clustering using scatterplots. It
    returns a list of plots.

## Installation

You can install the development version of datascience.eda with:

``` r
# install.packages("devtools")
devtools::install_github("UBC-MDS/datascience.eda.R")
```

## Example

``` r
library(datascience.eda)
## basic example code
```
