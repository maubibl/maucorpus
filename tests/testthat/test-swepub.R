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

test_that("swepub classify api works", {

  abstract <- "This paper proposes a locally differentially private federated learning algorithm for strongly convex but possibly nonsmooth problems that protects the gradients of each worker against an honest but curious server. The proposed algorithm adds artificial noise to the shared information to ensure privacy and dynamically allocates the time-varying noise variance to minimize an upper bound of the optimization error subject to a predefined privacy budget constraint. This allows for an arbitrarily large but finite number of iterations to achieve both privacy protection and utility up to a neighborhood of the optimal solution, removing the need for tuning the number of iterations. Numerical results show the superiority of the proposed algorithm over state-of-the-art methods."
  keywords <- "dynamic allocation, Federated learning, local differential privacy"
  title <- "DYNAMIC PRIVACY ALLOCATION FOR LOCALLY DIFFERENTIALLY PRIVATE FEDERATED LEARNING WITH COMPOSITE OBJECTIVES"
  kwds <- gsub(", ", " ", keywords)

  hsv_call <- classify_swepub(title, abstract, kwds)

  is_valid <- nrow(hsv_call) == 2 & all(hsv_call$code == c(102, 202))
  expect_true(is_valid)
})
