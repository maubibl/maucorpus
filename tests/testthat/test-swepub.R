test_that("swepub check works", {
  d1 <- swepub_checks()
  expect_gt(nrow(d1), 0)
})
