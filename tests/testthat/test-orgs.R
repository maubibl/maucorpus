test_that("fetching diva organisations works (scraping)", {
  do <- diva_organisations()
  is_ok <- nrow(do) > 500 & ncol(do) >= 9
  expect_true(is_ok)
})
