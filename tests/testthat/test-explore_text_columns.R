require(dplyr)
require(vdiffr)

test_that("explore_text_columns identifies the correct text features,
          calculates summary statistics and generates the plots related
          to text features accurately and handles exceptions gracefully", {

  df <- read_csv("data/spam.csv", locale(encoding = "latin1"),
                 col_names=c("target", "sms", "col3", "col4", "col5"),
                 col_types=NULL)

  df <- df %>% dplyr::select("target", "sms")
  df$num <- c(1:nrow(df))

  expect_error(explore_text_columns(df, 5))
  expect_error(explore_text_columns(df %>% dplyr::pull(sms)))
  expect_error(explore_text_columns(df, 'random_column'))
  expect_error(explore_text_columns(df, 'num'))
  empty_char <- vector(mode='character')
  expect_equal(length(explore_text_columns(df %>% dplyr::select("target"))), 0)
  expect_true(is.list(explore_text_columns(df %>% dplyr::select("target", "sms"))))
  results <- explore_text_columns(df)

  expect_equal(results[[1]], "sms")
  expect_equal(results[[2]], 80.07)
  expect_equal(results[[3]], 61)
  expect_equal(results[[4]], 910)
  expect_equal(results[[5]], 2)
  expect_equal(results[[6]], df$sms[1086])
  expect_equal(results[[7]], "v2")
  expect_equal(results[[8]], "Ok")
  expect_equal(results[[9]], "Ok")
  expect_equal(results[[10]], ":)")
  expect_equal(results[[11]], "Ok")
  expect_equal(results[[12]], "Ok")
  vdiffr::expect_doppelganger("Character_Length_Plot", results[[13]])
  expect_equal(results[[14]], 16.17)
  expect_equal(results[[15]], 12)
  expect_equal(results[[16]], 190)
  expect_equal(results[[17]], df$sms[1086])
  vdiffr::expect_doppelganger("Word_count_Plot", results[[18]])
  expect_equal(results[[19]]$word[1:5], c('call','now','can','get','will'))
  expect_equal(results[[19]]$frequen[1:5], c(578, 479, 405, 390, 378))
  vdiffr::expect_doppelganger("Word_count_bar", results[[20]])
  expect_equal(results[[21]]$word[1:5], c('are you','i am','have a','you are','you have'))
  expect_equal(results[[21]]$frequen[1:5], c(151, 133, 129, 123, 114))
  vdiffr::expect_doppelganger("Bigram_count_bar", results[[22]])

})
