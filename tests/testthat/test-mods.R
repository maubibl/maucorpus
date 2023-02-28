run_longrunning <- FALSE

test_that("generating mods for two specific scopus identifiers works", {

  skip_if_not(run_longrunning, "this test may become outdated soon...")

  sids <- c("SCOPUS_ID:85147970587", "SCOPUS_ID:85147928526")

  # generate MODS for each of these identifiers
  m1 <- sids[1] |> scopus_mods()
  m2 <- sids[2] |> scopus_mods()

  # generate for both
  m <- sids |> scopus_mods_crawl()

  # inspect results
  m$`SCOPUS_ID:85147970587` |> cat()
  m$`SCOPUS_ID:85147928526` |> cat()

  # confirm that results are identical
  is_valid <-
    identical(m$`SCOPUS_ID:85147970587`, m1) &
    identical(m$`SCOPUS_ID:85147928526`, m2)

  expect_true(is_valid)
})

test_that("generating mods for five more identifiers work", {

  skip_if_not(run_longrunning, "this test may not need to run each time")

  sids <-
     "SCOPUS_ID:85148093031
      SCOPUS_ID:85148091482
      SCOPUS_ID:85148079388
      SCOPUS_ID:85148108363
      SCOPUS_ID:85148053986" |>
    strsplit(split = "\\s+") |> unlist()

  scopus <- scopus_from_minio()
  ko <- kthid_orcid()

  # sid <- sids[1]
  #
  # abstract <- scopus_req_abstract(sid = sid) |> httr::content()
  # View(abstract)
  # abstract$`abstracts-retrieval-response`$item$`ait:process-info`
  #
  # ag <-
  #   abstract$`abstracts-retrieval-response`$item$bibrecord$head$`author-group`
  #
  # ag |> map_dfr(pluck_raw_org, .id = "id")

  # TODO: exame if this info is useful
  #abstract$`abstracts-retrieval-response`$item$`ait:process-info`

  mods <- scopus_mods_crawl(sids = sids, scopus = scopus, ko = ko)

  is_ok <- mods |> attr("debug") |> getElement("fails") |> length() == 0
  expect_true(is_ok)
})

test_that("generating mods for all articles works", {

  skip_if_not(run_longrunning, "this test may take too long to test every time...")

  scopus <- scopus_from_minio()
  ko <- kthid_orcid()
  articles <- scopus$publications |> subset(subtype == "ar") |> getElement("dc:identifier")
  mods <- scopus_mods_crawl(articles, scopus, ko)

  is_valid <- length(attr(mods, which = c("debug"))$fails) == 0
  expect_true(is_valid)
})



test_that("author groups gets wrapped when required in scopus_extended_abstract", {

  skip_if_not(run_longrunning, "this test may not need to run each time")

  raw_org <-
    "SCOPUS_ID:85148181190" |> scopus_abstract_extended() |>
    getElement("scopus_authorgroup") |>
    pull(raw_org) |> unique()

  is_valid <-
    "KTH-Royal Institute of Technology, Dept. of Energy Technology, Stockholm, SWEDEN" == raw_org

  expect_true(is_valid)

})
