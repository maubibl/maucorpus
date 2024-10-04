library(xml2)
library(xslt)

test_that("mods parsing works", {

  skip("Do not use python based json conversion...")

  mods_xml_from_pid <- function(pid) {
    kthcorpus::read_diva_mods(pid) |> 
      xml2::read_xml() |> xml2::xml_ns_strip() |> 
      as.character()
  } 

  x <- "1096938" |> mods_xml_from_pid() |> xml_to_r()

  is_valid <- 
    x |> mods_to_tbls() |> 
      map(\(x) x |> add_column(pid = "1096938")) |> 
      map_lgl(is.data.frame) |> all()
  
  expect_true(is_valid)

})

test_that("single mods xslt to jsonl parsing works", {

  x <- "1096938" |> read_diva_mods()

  is_valid <- (mods_to_xjsonld(x) |> length()) == 2

  expect_true(is_valid)

}

test_that("single mods xslt to json parsing works", {

  x <- "1096938" |> read_diva_mods()

  o <- 
    x |> mods_to_json() |> 
    mods_json_to_object() |> 
    parse_mods_object()

  is_valid <- 
    o |> 
      map_lgl(is.data.frame) |> all()  
  
  expect_true(is_valid)
})

test_that("multiple mods xslt to json to table parsing works", {

  my_reader <- function(pid) {
    read_diva_mods(pid) |> 
      mods_collection_file_to_tbl() |> 
      getElement("mods")
  }

  pids <- readr::read_lines("1786992
1792054
1812439
1836086
1779662
1779665
1782370
1781973
1763376
1785149
")

  x <- pids |> map(my_reader, .progress = TRUE)

  o <- 
    x |> parse_mods_objects() |> mods_records_to_tbls()

  is_valid <- nrow(o$recordInfo) == 10
  
  expect_true(is_valid)
})

test_that("long running conversion from MODS to tables succeeds", {

  skip("Enable only if needed")
  skip_if(!file.exists("~/diva-mods-2023.parquet"), message = "skipping long running operation")

  mods <- arrow::read_parquet("~/diva-mods-2023.parquet")
  my_records <- mods$mods
  
  my_tables <- 
    parse_mods_objects(my_records) |> mods_records_to_tbls()
  
  my_tables |> readr::write_rds(file = "~/diva-mods-2023-tables.rds")
  my_tables <- readr::read_rds(file = "~/diva-mods-2023-tables.rds")  

  # to check an individual record (conversion from JSON to Tables)

  # my_records[2659] |> mods_to_json() |> fromJSON() |> toJSON(pretty = TRUE) |> cat()

  # x <- my_records[2659] |> mods_to_json() |> mods_json_to_object() 
  # x |> parse_mods_object()

})

