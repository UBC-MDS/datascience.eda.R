
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
    categorical features. It returns a list having a tibble with column
    names, corresponding unique categories, counts of null values,
    percentages of null values and most frequent categories along with 
    bar plots of user provided categorical columns.

-   `explore_text_columns`: performs exploratory data analysis of text
    features. If text feature columns are not specified, the function
    will try to identify the text features. The function prints the
    summary statistics of character length and word count, and plots a
    histogram of its distributions. It also plots the word cloud of
    words (after removing stopwords) and bi-grams. Bar charts of top 10
    words (after removing stopwords) and top 10 bi-grams will be plotted
    as well. This function returns a list of all the results.

-   `explore_clustering`: fits K-Means and DBSCAN clustering algorithms
    on the dataset and visualizes the clustering using scatterplots on
    PCA transformed data. It returns a dictionary (list) with each key
    being name of the clustering algorithm and the value being a list of
    plots generated by the models.

-   `explore_KMeans_clustering`: fits K-Means clustering algorithms on
    the dataset and visualizes the clustering using scatterplots on PCA
    transformed data. It returns a list of plots.

-   `explore_DBSCAN_clustering`: fits K-DBSCAN clustering algorithms on
    the dataset and visualizes the clustering using scatterplots on PCA
    transformed data. It returns a list of plots.

## Installation

You can install the development version of datascience.eda with:

``` r
# install.packages("devtools")
devtools::install_github("UBC-MDS/datascience.eda.R")
```

## Example

``` r
library(datascience.eda)
library(palmerpenguins)

explore_KMeans_clustering(penguins, centers = seq(3, 5))
#> Warning: did not converge in 10 iterations
#> [[1]]
```

<img src="man/figures/README-example-1.png" width="100%" />

    #> 
    #> [[2]]

<img src="man/figures/README-example-2.png" width="100%" />

    #> 
    #> [[3]]

<img src="man/figures/README-example-3.png" width="100%" />

``` r
explore_DBSCAN_clustering(penguins, eps = c(3, 5), minPts = c(5, 10))
#> [[1]]
```

<img src="man/figures/README-example-4.png" width="100%" />

    #> 
    #> [[2]]

<img src="man/figures/README-example-5.png" width="100%" />

    #> 
    #> [[3]]

<img src="man/figures/README-example-6.png" width="100%" />

    #> 
    #> [[4]]

<img src="man/figures/README-example-7.png" width="100%" />
