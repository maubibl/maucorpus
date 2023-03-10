is_longrunning <- TRUE

test_that("retrieving crossref funders for Sweden works", {
  skip_on_ci()
  skip_if(is_longrunning)
  crfs <- cr_funders("Sweden")
  is_valid <- nrow(crfs) > 500 & ncol(crfs) > 3
  expect_true(is_valid)
})

test_that("resolving funderstrings works", {
  skip_on_ci()
  orgs <- cr_funders_resolve("Kungliga")
  is_valid <- nrow(orgs) > 5 & ncol(orgs) > 3
  expect_true(is_valid)
})
