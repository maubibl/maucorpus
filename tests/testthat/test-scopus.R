test_that("scopus search works", {
  ss <- scopus_search_pubs_kth()
  s <- ss$publications
  is_valid <- nrow(s) >= 1
  expect_true(is_valid)
})
