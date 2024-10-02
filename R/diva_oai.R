oai_url <- function() {
  "https://kth.diva-portal.org/dice/oai"
}

oai_db_path <- function() {
  file.path(rappdirs::app_dir("kthcorpus")$config(), "oai.db") |>
  normalizePath()
}

oai_con <- function() {
  con <- duckdb::dbConnect(duckdb::duckdb(), dbdir = oai_db_path())
  DBI::dbExecute(con, "SET autoinstall_known_extensions=1;")
  DBI::dbExecute(con, "SET autoload_known_extensions=1;")
  return (con)
}

#' @importFrom oai list_metadataformats list_sets
oai_hello <- function(url = oai_url()) {
  service <- url |> oai::id() |> tibble::as_tibble()
  meta <- url |> oai::list_metadataformats() |> tibble::as_tibble()
  sets <- url |> oai::list_sets() |> tibble::as_tibble()
  list(service = service, meta = meta, sets = sets)
}

oai_db_rm <- function() {
  unlink(oai_db_path())
}

#' @importFrom oai list_identifiers
oai_identifiers_kth <- function(since) {
  message("... patience please ... 5 - 10 minutes ...")
  tictoc::tic()
  #ids <- oai_url() |> list_identifiers(from = "2010-01-01T", set = "all-kth")
  if (missing(since)) {
    ids <- oai_url() |> oai::list_identifiers(set = "all-kth")
  } else {
    ids <- oai_url() |> oai::list_identifiers(from = since, set = "all-kth")
  }
  tictoc::toc()

  return(ids)

}

oai_record <- function(ids, oai = oai_url(), 
  format = oai_hello(url = oai)$meta$metadataPrefix) {

  mf <- match.arg(format)

  ids |>
    oai::get_records(prefix = mf, url = oai, as = "raw") |>
    tibble::enframe(name = "identifier", value = "mods") |>
    tidyr::unnest("mods")
}

oai_crawl_records <- function(ids, batch_size = 100L) {

  pages <- split(ids, ceiling(seq_along(1:length(ids)) / batch_size))

  pb <- progress::progress_bar$new(
    format = "(:spin) harvesting :percent (:current) :elapsed [:bar] [:eta]",
    total = length(pages), width = 63
  )

  oai_crawl <- function(x) {
    pb$tick()
    oai_record(x, format = "swepub_mods")
  }

  crawl_or_proceed <- purrr::possibly(oai_crawl, otherwise = NULL)

  pages |>
    purrr::map_df(crawl_or_proceed, .progress = TRUE)

}

oai_db_lastmod <- function() {

  datestamp <- ts <- NULL

  con <- oai_con()
  on.exit(DBI::dbDisconnect(con, shutdown = TRUE))

  stopifnot(DBI::dbExistsTable(con, "ids"))

  ids <- 
    con |> dplyr::tbl("ids") |> collect() |> 
    readr::type_convert() |> suppressMessages()

  ids |> dplyr::summarise(ts = max(datestamp)) |> pull(ts) |> as.Date()
  
}

#' @importFrom dbplyr dbplyr_edition
oai_db_refresh <- function(append = FALSE) {

  edition <- dbplyr::dbplyr_edition()
  message("Using dbplyr edition ", edition)

  lastmod <- oai_db_lastmod()
  message("Fetching changes since last db datestamp: ", lastmod)
  ids <- oai_identifiers_kth(since = lastmod) |> 
    readr::type_convert() |> suppressMessages()

  con <- oai_con()
  stopifnot(DBI::dbExistsTable(con, "ids"))
  on.exit(DBI::dbDisconnect(con, shutdown = TRUE))

  ids_old <- 
    con |> dplyr::tbl("ids") |> dplyr::collect() |> 
    readr::type_convert() |> suppressMessages()

  new_ids <- dplyr::anti_join(ids, ids_old)

  print(new_ids)

  if (isTRUE(append)) {
    message("Appending new identifiers to ids table")
    res <- con |> DBI::dbWriteTable("ids", new_ids, append = TRUE)
  } else {
    message("No changes made to the database (ids table)")
  }
  
  message("Fetching MODS for new identifiers")

  tictoc::tic()
  new_mods <- new_ids$identifier |> oai_crawl_records()
  tictoc::toc()

  stopifnot(DBI::dbExistsTable(con, "mods"))

  if (isTRUE(append)) {
    message("Appending records to mods table")
    res <- con |> DBI::dbWriteTable("mods", new_mods, append = TRUE)
  } else {
    message("No changes made to the database (mods table)")
  }

  stopifnot(DBI::dbExistsTable(con, "mods_extra"))

  tictoc::tic()
  new_extra <- oai_db_enrich(new_mods)
  tictoc::toc()

  if (isTRUE(append)) {
    message("Appending mods in xml and json to mods_extra table")
    res <- con |> DBI::dbWriteTable("mods_extra", new_extra, append = TRUE)
  } else {
    message("No changes made to the database (mods_extra table)")
  }

  out <- list(
    ids = new_ids,
    mods = new_mods,
    extra = new_extra
  )

  return(out)
  
}

oai_record_status <- function(mods_xml_string, quiet = TRUE) {
  x <- mods_xml_string
  myxml <- xml2::read_xml(x)
  header <- xml2::xml_find_all(xml2::xml_ns_strip(myxml), "//GetRecord//header")
  res <- header |> xml2::xml_attr("status")
  if (length(res) > 1) {
    if (!quiet) message("Found more than one status value in xml: \n", as.character(header))
    return(res |> unique() |> na.omit() %>% paste(collapse = " "))
  }
  return(res)
}

xfind <- function(xml, xpath) {
  x <- read_xml(xml)
  res <- xml_find_all(xml_ns_strip(x), xpath) |> xml_text()
  if (length(res) < 1) return(NA_character_)
  if (length(res) > 1) return(paste(collapse = " ", res))
  return(res)
}

oai_mods_statuses <- function(records) {

  mods <- identifier <- status <- NULL

  if (missing(records)) {
    con <- oai_con()
    on.exit(DBI::dbDisconnect(con, shutdown = TRUE))
    records <- con |> tbl("mods") |> collect()  
  }
  
  records |> 
    mutate(status = purrr::map_chr(mods, oai_record_status, .progress = TRUE)) |>
    select(identifier, mods, status)
}

oai_mods_pids <- function(records) {

  mods <- NULL

  if (missing(records)) {
    con <- oai_con()
    on.exit(DBI::dbDisconnect(con, shutdown = TRUE))
    records <- con |> tbl("mods") |> collect()  
  }

  records |>
    mutate(PID = purrr::map_chr(mods, function(x) xfind(x, "//recordIdentifier"), .progress = TRUE)) |>
    filter(!is.na(PID)) |> 
    mutate(PID = gsub("diva2:", "", PID)) |>
    mutate(PID_many = grepl(" ", PID))   
}

oai_mods_jsons <- function(records) {

  mods <- NULL

  if (missing(records)) {
    con <- oai_con()
    on.exit(DBI::dbDisconnect(con, shutdown = TRUE))
    records <- con |> tbl("mods") |> collect()  
  }

  records |> 
    mutate(json = purrr::map_chr(mods, \(x) possibly(mods_to_json, NA_character_)(x), .progress = TRUE))
  
}

oai_db_enrich <- function(records) {

  if (missing(records)) {
    con <- oai_con()
    on.exit(DBI::dbDisconnect(con, shutdown = TRUE))
    records <- con |> tbl("mods") |> collect()  
  }

  t1 <- records |> oai_mods_jsons()
  t2 <- records |> oai_mods_statuses()
  t3 <- records |> oai_mods_pids()  
  t4 <- t1 |> left_join(t2) |> left_join(t3)

  records |> left_join(t4) |> 
    select(any_of(c("PID", "PID_many", "status")), everything())

}
