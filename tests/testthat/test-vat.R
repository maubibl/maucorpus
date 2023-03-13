test_that("Parsing vatNumber formats works", {

  skip_on_ci()

  vat <- vat_parse("SE202100305401")

  is_valid <-
    vat$is_eu == TRUE & vat$is_swe == TRUE & vat$swe_org == "2021003054"

  expect_true(is_valid)

})

test_that("Resolving vatNumber from VIES EU REST API works", {
  skip_on_ci()
  vat <- vat_resolve_vies("SE202100305401")
  is_valid <- vat$isValid == TRUE
  expect_true(is_valid)
})

test_that("Searching EU service for pic (participant identification code) works", {
  skip_on_ci()
  search <- funders_eu_search(legal_name = "Kungliga Tekniska")
  is_valid <- c("999990946", "918528891") %in% search$pic |> all()
  expect_true(is_valid)
})

