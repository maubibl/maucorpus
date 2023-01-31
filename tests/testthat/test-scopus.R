test_that("scopus search works", {
  skip_on_ci()
  ss <- scopus_search_pubs_kth()
  s <- ss$publications
  is_valid <- nrow(s) >= 1
  expect_true(is_valid)
})

test_that("scopus remaining quota works", {
  skip_on_ci()
  rl <- scopus_ratelimit_quota()
  is_valid <- !any(is.null(c(rl$`X-RateLimit-Limit`, rl$`X-RateLimit-Remaining`, rl$`X-RateLimit-Reset`)))
  expect_true(is_valid)
})

test_that("scopus abstract extended info works", {
  skip_on_ci()
  mysid <- "SCOPUS_ID:85140456799"
# SCOPUS_ID:85140456799 - no org_sourcetext?
# SCOPUS_ID:85140456964 - no correspondence?
  ae <- scopus_abstract_extended(mysid)
  is_invalid <- ae$scopus_correspondence$ce_surname |> is.na() |> all()
  expect_true(!is_invalid)
})



