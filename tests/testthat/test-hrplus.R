test_that("hr_latest works with data from one week back", {
  skip_on_ci()
  weekold <- format((today() - lubridate::days(7)), "%Y%m%d_abu.csv")
  d1 <- hr_read_csv(aws.s3::get_object(weekold, "hrplus"))
  is_valid <- nrow(d1) > 1000 && all(names(d1) %in% hr_mapping$colname)
  expect_true(is_valid)
})
