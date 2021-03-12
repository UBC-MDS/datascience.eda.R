#' Performs EDA of text features
#' - prints the summary statistics of character length
#' - plots the distribution of character length
#' - prints the summary statistics of word count
#' - plots the distribution of word count
#' - plots the word cloud
#'
#' @param df dataframe: dataset
#' @param text_cols vector of numeric column names
#'
#' @return a list of plots
#'
#' @examples
#' \dontrun{
#' explore_text_columns(df)
#' }
explore_text_columns <- function(df, text_cols=list()) {

  for (col in text_cols){

    results <- list()

    cat("## Exploratory Data Analysis of \"",col,"\" column:", sep = "")
    cat("\n")
    cat("### Character Length:")
    cat("\n")

    char_len <- df %>%
      dplyr::mutate(char_len = str_length(!!sym(col))) %>%
      dplyr::pull(char_len)

    mean_char_len <- round(mean(char_len), 2)
    median_char_len <- round(median(char_len), 2)

    cat("- The average character length of text is:",mean_char_len)
    cat("\n")
    cat("\n")
    cat("- The median character length of text is:",median_char_len)
    cat("\n")
    cat("\n")

    results <- append(results, mean_char_len)
    results <- append(results, median_char_len)

    max_char <- df %>%
      dplyr::mutate(char_len = str_length(!!sym(col))) %>%
      filter(char_len == max(char_len))

    min_char <- df %>%
      dplyr::mutate(char_len = str_length(!!sym(col))) %>%
      dplyr::filter(char_len == min(char_len))

    max_char_n <- max_char %>%
      dplyr::pull(char_len) %>%
      unique()

    min_char_n <- min_char %>%
      dplyr::pull(char_len) %>%
      unique()

    max_char_text <- max_char %>%
      dplyr::pull(!!sym(col))

    min_char_text <- min_char %>%
      dplyr::pull(!!sym(col))

    cat("- The longest text(s) has", max_char_n,"characters:\n")
    cat("\n")

    for (word in max_char_text){
      cat("\n")
      cat(word)
      cat("\n")
    }

    cat("\n")
    cat("\n")
    cat("- The shortest text(s) has(have)", min_char_n,"characters:\n")
    cat("\n")

    for (word in min_char_text){
      cat("\n")
      cat(word)
      cat("\n")
    }

    results <- append(results, max_char_n)
    results <- append(results, min_char_n)
    results <- append(results, max_char_text)
    results <- append(results, min_char_text)

    cat("\n")
    cat("#### Histogram of number of characters in \"",col,"\":", sep = "")
    cat("\n")

    results[[length(results)+1]] <- df %>%
      dplyr::mutate(char_len = str_length(!!sym(col))) %>%
      ggplot2::ggplot(aes(x=char_len)) +
      ggplot2::geom_histogram(bins = 30) +
      ggplot2::theme_bw() +
      ggplot2::xlab(paste0("Number of characters in \"",col,"\""))

    print(results[[length(results)]])

    cat("\n\n")
    cat("### Word Count:")
    cat("\n")
    word_counts <- df %>%
      dplyr::mutate(word_count = str_count(!!sym(col), "\\w+")) %>%
      dplyr::pull(word_count)

    mean_word_count <- round(mean(word_counts), 2)
    median_word_count <- round(median(word_counts), 2)

    cat("- The average  number of words in \"",col,"\" is: ", mean_word_count, sep = "")
    cat("\n")
    cat("\n")
    cat("- The median  number of words in \"",col,"\" is: ", median_word_count, sep = "")
    cat("\n")
    cat("\n")

    results <- append(results, mean_word_count)
    results <- append(results, median_word_count)

    max_words <- df %>%
      dplyr::mutate(word_count = str_count(!!sym(col), "\\w+")) %>%
      dplyr::filter(word_count == max(word_count))

    max_word_count <- max_words %>%
      dplyr::pull(word_count) %>%
      unique()

    max_word_text <- max_words %>%
      dplyr::pull(col)

    cat("- The text(s) in \"",col,"\" with most words(",max_word_count,"):\n", sep = "")

    for (word in max_word_text){
      cat("\n")
      cat(word)
      cat("\n")
    }

    results <- append(results, max_word_count)
    results <- append(results, max_word_text)

    cat("\n")
    cat("\n")
    cat("\n")
    cat("#### Histogram of number of words in \"",col,"\":", sep = "")
    cat("\n")
    results[[length(results)+1]] <- df %>%
      dplyr::mutate(word_count = str_count(!!sym(col), "\\w+")) %>%
      ggplot2::ggplot(aes(x=word_count)) +
      ggplot2::geom_histogram(bins = 30) +
      ggplot2::theme_bw() +
      ggplot2::xlab(paste0("Number of words in \"",col,"\""))

    print(results[[length(results)]])

  }

  results
}
