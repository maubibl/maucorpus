test_that("fetching curated authors data works", {
  skip_on_ci()
  res <- kth_issues_pubauth(jq = 'select(.kthid | test("^⚠"))')
  is_valid <- lengths(res) > 1
  expect_true(is_valid)
})
