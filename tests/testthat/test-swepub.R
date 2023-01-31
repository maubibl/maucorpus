test_that("swepub check works, default org", {
  d1 <- swepub_checks()
  expect_gt(nrow(d1), 0)
})

test_that("swepub check works, specific org", {
  cfg <- diva_config()
  cfg$org <- "his"
  d1 <- swepub_checks(config = cfg)
  expect_gt(nrow(d1), 0)
})
