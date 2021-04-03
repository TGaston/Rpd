library(testthat)


test_that("fars reading", {
  fars_read <- function(filename) {
    if(!file.exists(filename))
      stop("file '", filename, "' does not exist")
    data <- suppressMessages({
      readr::read_csv(filename, progress = FALSE)
    })
    dplyr::tbl_df(data)
  }

  expect_that(fars_read, is_a("function"))

})
