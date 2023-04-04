run_longrunning <- FALSE

test_that("generating mods for two specific scopus identifiers works", {

  skip_if_not(run_longrunning, "this test may become outdated soon...")

  sids <- c("SCOPUS_ID:85150694011", "SCOPUS_ID:85150927217")

  # generate MODS for each of these identifiers
  m1 <- sids[1] |> scopus_mods()
  m2 <- sids[2] |> scopus_mods()

  # generate for both
  m <- sids |> scopus_mods_crawl()

  # inspect results
  m |> getElement(sids[1]) |> cat()
  m |> getElement(sids[2]) |> cat()

  # confirm that results are identical
  is_valid <-
    identical(m |> getElement(1), m1) &
    identical(m |> getElement(2), m2)

  expect_true(is_valid)
})


test_that("generating a mods collection for two identifiers works", {

  skip_if_not(run_longrunning, "this test may become outdated soon...")

  sids <- c("SCOPUS_ID:85150694011", "SCOPUS_ID:85150927217")

  # generate MODS for each of these identifiers
  m1 <- sids[1] |> scopus_mods()
  m2 <- sids[2] |> scopus_mods()

  # generate for both
  m <- sids |> scopus_mods_crawl()

  mx_a <- create_diva_modscollection(c(m1, m2)) |> xml2::read_xml() |> as.character()
  mx_b <- create_diva_modscollection(m) |> xml2::read_xml() |> as.character()

  is_valid <- mx_a == mx_b

  expect_true(is_valid)
})


test_that("generating mods for five identifiers not in the scopus search fails", {

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

  # abstract <- scopus_req_abstract(sid = sid) |> httr::content()
  # View(abstract)
  # abstract$`abstracts-retrieval-response`$item$`ait:process-info`
  #
  # ag <-
  #   abstract$`abstracts-retrieval-response`$item$bibrecord$head$`author-group`
  #
  # ag |> map_dfr(pluck_raw_org, .id = "id")
  #
  # TODO: exame if this info is useful
  # abstract$`abstracts-retrieval-response`$item$`ait:process-info`

  mods <- scopus_mods_crawl(sids = sids, scopus = scopus, ko = ko)

  are_fails <- mods |> attr("debug") |> getElement("fails") |> length() == 5
  expect_true(are_fails)
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
