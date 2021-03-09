#' return a data set used for testing
#'
#' @return data frame
#'
#' @examples
#' df <- create_test_data()
create_test_data <- function()
{
  library(palmerpenguins)
  palmerpenguins::penguins
}
