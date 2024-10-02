test_that("connection through OAI-PMH can be made", {
  hello <- oai_hello()
  is_valid <- nrow(hello$sets) > 20 
  expect_true(is_valid)
})

test_that("database exists w harvest", {

  con <- oai_con()
  on.exit(DBI::dbDisconnect(con))
  is_valid <- "ids" %in% DBI::dbListTables(con)
  expect_true(is_valid)

})

test_that("changes since latest timestamp from database can be retrieved", {

  changes <- oai_db_refresh()
  n_rows <- changes |> map_int(nrow)
  
  is_ok <- length(unique(n_rows)) == 1
  expect_true(is_ok)

})
