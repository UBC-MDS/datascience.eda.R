#' Performs EDA of text features. Works well with Rmarkdown.
#' Its highly recommended to use code-chunk option results='asis'
#' to make use of the markdown syntax
#' - prints the summary statistics of character length
#' - plots the distribution of character length
#' - prints the summary statistics of word count
#' - plots the distribution of word count
#' - plots the word cloud
#'
#' @importFrom dplyr %>%
#'
#' @param df dataframe: dataset
#' @param text_cols vector of text column names
#'
#' @return a list of results and plots
#' @export
#'
#' @examples
#' explore_text_columns(cars)
explore_text_columns <- function(df, text_cols=vector(mode='character')) {

  results <- list()
  word_count <- frequen <- NULL

  if (!is.character(text_cols)) {
    stop("text_cols is not passed as a character vector")
  }

  if (!is.data.frame(df)) {
    stop("The df passed is not a Data Frame")
  }

  if(length(text_cols) == 0) {
    for (col in colnames(df %>% dplyr::select_if(is.character))){
      if (df %>% dplyr::select(col) %>%
          dplyr::n_distinct(col)/nrow(df) > 0.75){
        if ((stats::median(df %>%
                    dplyr::mutate(char_len = stringr::str_length(!!dplyr::sym(col))) %>%
                    dplyr::pull(char_len))) > 5){
          text_cols <- append(text_cols, col)
        }
      }
    }
    if(length(text_cols) == 0) {
      cat("\n")
      cat("\n")
      cat("Could not identify any text column. Please pass the text column(s) when calling the function")
      cat("\n")
      cat("\n")
      return(results)
    }
    else {
      cat("\n")
      cat("\n")
      cat("Identified the following as text columns:", text_cols)
      cat("\n")
      cat("\n")
    }
  } else {
    for (col in text_cols){
      if (!(col %in% colnames(df))){
        stop(paste0(col, " passed in text_cols is not a column in the dataframe"))
      }
      if (!is.character(df)){
        stop(paste0(col, " passed in text_cols is not a column of character data type"))
      }
    }

  }

  results <- append(results, text_cols)

  for (col in text_cols){

    cat("## Exploratory Data Analysis of \"",col,"\" column:", sep = "")
    cat("\n")
    cat("### Character Length:")
    cat("\n")

    char_len <- df %>%
      dplyr::mutate(char_len = stringr::str_length(!!dplyr::sym(col))) %>%
      dplyr::pull(char_len)

    mean_char_len <- round(mean(char_len), 2)
    median_char_len <- round(stats::median(char_len), 2)

    cat("- The average character length of text is:",mean_char_len)
    cat("\n")
    cat("\n")
    cat("- The median character length of text is:",median_char_len)
    cat("\n")
    cat("\n")

    results <- append(results, mean_char_len)
    results <- append(results, median_char_len)

    max_char <- df %>%
      dplyr::mutate(char_len = stringr::str_length(!!dplyr::sym(col))) %>%
      dplyr::filter(char_len == max(char_len))

    min_char <- df %>%
      dplyr::mutate(char_len = stringr::str_length(!!dplyr::sym(col))) %>%
      dplyr::filter(char_len == min(char_len))

    max_char_n <- max_char %>%
      dplyr::pull(char_len) %>%
      unique()

    min_char_n <- min_char %>%
      dplyr::pull(char_len) %>%
      unique()

    max_char_text <- max_char %>%
      dplyr::pull(!!dplyr::sym(col))

    min_char_text <- min_char %>%
      dplyr::pull(!!dplyr::sym(col))

    cat("- The longest text(s) has", max_char_n,"characters:\n")
    cat("\n")

    for (word in max_char_text){
      cat("\n")
      cat(paste0("\"",word,"\""))
      cat("\n")
    }

    cat("\n")
    cat("\n")
    cat("- The shortest text(s) has", min_char_n,"characters:\n")
    cat("\n")

    for (word in min_char_text){
      cat("\n")
      cat(paste0("\"",word,"\""))
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
      dplyr::mutate(char_len = stringr::str_length(!!dplyr::sym(col))) %>%
      ggplot2::ggplot(ggplot2::aes(x=char_len)) +
      ggplot2::geom_histogram(bins = 30) +
      ggplot2::theme_bw() +
      ggplot2::xlab(paste0("Number of characters in \"",col,"\""))

    print(results[[length(results)]])

    cat("\n\n")
    cat("### Word Count:")
    cat("\n")
    word_counts <- df %>%
      dplyr::mutate(word_count = stringr::str_count(!!dplyr::sym(col), "\\w+")) %>%
      dplyr::pull(word_count)

    mean_word_count <- round(mean(word_counts), 2)
    median_word_count <- round(stats::median(word_counts), 2)

    cat("- The average  number of words in \"",col,"\" is: ", mean_word_count, sep = "")
    cat("\n")
    cat("\n")
    cat("- The median  number of words in \"",col,"\" is: ", median_word_count, sep = "")
    cat("\n")
    cat("\n")

    results <- append(results, mean_word_count)
    results <- append(results, median_word_count)

    max_words <- df %>%
      dplyr::mutate(word_count = stringr::str_count(!!dplyr::sym(col), "\\w+")) %>%
      dplyr::filter(word_count == max(word_count))

    max_word_count <- max_words %>%
      dplyr::pull(word_count) %>%
      unique()

    max_word_text <- max_words %>%
      dplyr::pull(col)

    cat("- The text(s) in \"",col,"\" with most words (",max_word_count," words):\n", sep = "")

    for (word in max_word_text){
      cat("\n")
      cat(paste0("\"",word,"\""))
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
      dplyr::mutate(word_count = stringr::str_count(!!dplyr::sym(col), "\\w+")) %>%
      ggplot2::ggplot(ggplot2::aes(x=word_count)) +
      ggplot2::geom_histogram(bins = 30) +
      ggplot2::theme_bw() +
      ggplot2::xlab(paste0("Number of words in \"",col,"\""))

    print(results[[length(results)]])

    cat("\n")
    cat("\n")
    cat("#### Word Cloud of text in \"",col,"\":", sep = "")
    cat("\n")

    text <- df %>%
      dplyr::pull(!!dplyr::sym(col))

    corpus <- tm::Corpus(tm::VectorSource(text))
    docs <- corpus %>%
      tm::tm_map(tm::removeNumbers) %>%
      tm::tm_map(tm::removePunctuation) %>%
      tm::tm_map(tm::stripWhitespace) %>%
      tm::tm_map(tm::content_transformer(tolower)) %>%
      tm::tm_map(tm::removeWords, tm::stopwords("english"))

    dtm <- tm::TermDocumentMatrix(docs)
    matrix <- as.matrix(dtm)
    words <- sort(rowSums(matrix),decreasing=TRUE)

    word_cloud_df <- data.frame(word = names(words), frequen=words) %>%
      dplyr::arrange(dplyr::desc(frequen))

    set.seed(1)
    wordcloud::wordcloud(words = word_cloud_df$word, freq = word_cloud_df$frequen,
                         min.freq = 1,max.words=200,
                         random.order=FALSE, rot.per=0.35,
                         colors=RColorBrewer::brewer.pal(8, "Dark2"))

    results[[length(results)+1]] <- word_cloud_df

    cat("\n")
    cat("\n")
    cat("#### Bar chart of top 10 words in \"",col,"\":", sep = "")
    cat("\n")

    results[[length(results)+1]] <- word_cloud_df %>%
      utils:: head(10) %>%
      ggplot2::ggplot(ggplot2::aes(x=stats::reorder(word, -frequen), y=frequen)) +
      ggplot2::geom_bar(stat="identity") +
      ggplot2::theme_bw() +
      ggplot2::xlab(paste0("Words in \"",col,"\""))

    print(results[[length(results)]])

    cat("\n")
    cat("\n")

    cat("#### Word Cloud of Bigrams in \"",col,"\":", sep = "")
    cat("\n")

    BigramTokenizer <- function(x) {
      unlist(lapply(NLP::ngrams(NLP::words(x), 2), paste, collapse = " "), use.names = FALSE)
    }

    corpus <- tm::VCorpus(tm::VectorSource(text))
    dtm_bigram <- tm::TermDocumentMatrix(corpus, control = list(tokenize = BigramTokenizer))
    matrix_bigram <- as.matrix(dtm_bigram)
    words_bigram <- sort(rowSums(matrix_bigram),decreasing=TRUE)

    bigram_word_cloud_df <- data.frame(word = names(words_bigram),frequen=words_bigram) %>%
      dplyr::arrange(dplyr::desc(frequen))

    set.seed(1)
    wordcloud::wordcloud(words = bigram_word_cloud_df$word, freq = bigram_word_cloud_df$frequen,
              min.freq = 1,max.words=200,
              random.order=FALSE, rot.per=0.35,
              colors=RColorBrewer::brewer.pal(8, "Dark2"))

    results[[length(results)+1]] <- bigram_word_cloud_df

    cat("\n")
    cat("\n")
    cat("#### Bar chart of top 10 Bigrams in \"",col,"\":", sep = "")
    cat("\n")

    results[[length(results)+1]] <- bigram_word_cloud_df %>%
      utils:: head(10) %>%
      ggplot2::ggplot(ggplot2::aes(x=stats::reorder(word, -frequen), y=frequen)) +
      ggplot2::geom_bar(stat="identity") +
      ggplot2::theme_bw() +
      ggplot2::xlab(paste0("Bigrams in \"",col,"\""))

    print(results[[length(results)]])

    cat("\n")
    cat("\n")
  }

  cat("**End of Exploratory Data Analysis of Text Columns**")
  cat("\n")
  cat("\n")

  results
}
