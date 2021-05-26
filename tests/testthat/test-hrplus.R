test_that("hr_plus works with data from one week back", {
  skip_on_ci()
  d1 <- hr_plus(offset = 7)
  is_valid <- nrow(d1) > 1000 && all(names(d1) %in% hr_mapping$colname)
  expect_true(is_valid)
})

#library(daff)
#render_diff(diff_data(hr_plus(offset = 1), hr_plus()))
