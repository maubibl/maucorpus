#' Retrieve DiVA publications for KTH from the KTH DiVA portal
#'
#' This function sends a request to KTH's DiVA portal for a CSV data export
#' covering KTH publications between 2013 and 2022.
#'
#' @param orgid the DiVA organisation id, by default "177" for KTH
#' @param year_beg the beginning of the period, by default "2013"
#' @param year_end the end of the period, by default "2022"
#' @param use_cache logical flag to indicate if cached data should be used, default: TRUE
#' @return data frame with results
#' @export
#' @importFrom jsonlite toJSON
#' @importFrom httr parse_url build_url
#' @importFrom curl curl_download
#' @importFrom rappdirs app_dir
#' @import readr
kth_diva_pubs <- function(orgid = "177", year_beg = "2013", year_end = "2022", use_cache = TRUE) {
  diva_tmp <- function(file) file.path(rappdirs::app_dir("kthcorpus")$config(), file)
  tmp <- diva_tmp("kth_diva_pubs.rds")
  if (!dir.exists(dirname(tmp))) dir.create(dirname(tmp), recursive = TRUE)
  if (file.exists(tmp)) {
    return(readr::read_rds(tmp))
  }

  message("Please be patient, the export takes a few minutes to complete...")

  pubtypes <- function() {
    c(
      "bookReview", "review", "article",
      "artisticOutput", "book", "chapter",
      "manuscript", "collection", "other",
      "conferencePaper", "patent", "conferenceProceedings",
      "report", "dataset"
    )
  }

  queryparam_aq2 <- function(.pubtypes = pubtypes()) {
    list(list(
      list(dateIssued = I(list(from = year_beg, to = year_end))),
      list(organisationId = orgid, `organisationId-Xtra` = TRUE),
      list(publicationTypeCode = .pubtypes)
    )) %>% jsonlite::toJSON(auto_unbox = TRUE)
  }

  smash_url <- httr::parse_url("https://kth.diva-portal.org/smash/export.jsf")

  # add any params? "language=en&searchType=RESEARCH&query=&af=[]&onlyFullText=false&sf=all"
  smash_url$query <- list(
    format = "csvall2", addFilename = "true",
    aq = I("[[]]"), aqe = I("[]"), aq2 = I(queryparam_aq2()),
    onlyFullText = "false", noOfRows = as.character(5e6L),
    sortOrder = "title_sort_asc", sortOrder2 = "title_sort_asc"
  )

  smash_file <- tempfile() # "~/repos/semanticscholar/data-raw/kth.csv"
  on.exit(unlink(smash_file))

  url <- httr::build_url(smash_url)
  cu <- curl::curl_download(url, destfile = smash_file, quiet = FALSE)
  # download.file(url_smash, destfile = smash_file)
  # res <- curl_fetch_memory(build_url(smash_url))

  ct <- readr::cols(
    .default = col_character(),
    PID = col_double(),
    Year = col_double(),
    PMID = col_double(),
    Opponents = col_logical(),
    Supervisors = col_logical(),
    Examiners = col_logical(),
    ThesisLevel = col_logical(),
    Credits = col_logical(),
    Programme = col_logical(),
    Subject = col_logical(),
    Uppsok = col_logical(),
    DefencePlace = col_logical(),
    DefenceLanguage = col_logical(),
    DefenceDate = col_logical(),
    CreatedDate = col_date(format = ""),
    PublicationDate = col_date(format = ""),
    LastUpdated = col_date(format = ""),
    NumberOfAuthors = col_double(),
    ExternalCooperation = col_logical(),
    FridaLevel = col_double(),
    Term = col_logical(),
    Reviewed = col_logical(),
    FreeFulltext = col_logical(),
    SustainableDevelopment = col_logical()
  )

  pubs <- readr::read_csv(cu, col_types = ct)

  if (use_cache) readr::write_rds(pubs, tmp)

  return(pubs)
}


#' Retrieve DiVA authors for KTH from the KTH DiVA portal
#'
#' This function returns parsed author information from DiVA data
#'
#' @param orgid the DiVA organisation id, by default "177" for KTH
#' @param year_beg the beginning of the period, by default "2013"
#' @param year_end the end of the period, by default "2020"
#' @param use_cache logical flag to indicate if cached data should be used,
#' default: TRUE
#' @param refresh_cache logical flag to indicate if data cache should be
#' refreshed, default: FALSE
#' @return data frame with results
#' @export
#' @importFrom readr write_rds read_rds
kth_diva_authors <- function(orgid = "177", year_beg = "2013", year_end = "2022",
                             use_cache = TRUE, refresh_cache = FALSE) {
  diva_tmp <- function(file) file.path(rappdirs::app_dir("kthcorpus")$config(), file)
  tmp <- diva_tmp("kth_diva_authors.rds")
  if (!dir.exists(dirname(tmp))) dir.create(dirname(tmp), recursive = TRUE)
  if (file.exists(tmp) && !refresh_cache) {
    return(readr::read_rds(tmp))
  }

  .pubs <- kth_diva_pubs(orgid, year_beg, year_end, use_cache)
  parsed_diva_names <- parse_diva_names(pubs = .pubs)

  if (use_cache) readr::write_rds(parsed_diva_names, tmp)

  return(parsed_diva_names)
}

#' Name aliases for DiVA authors in the KTH DiVA portal
#'
#' This function returns data for authors that for the same author identifier
#' have multiple names registered in composite DiVA Name strings
#' (bibliographic names for a publication).
#'
#' @param authors a tibble with authors data, default: kth_diva_authors()
#' @return data frame with results
#' @export
#' @import dplyr
kth_diva_aliases <- function(authors = kth_diva_authors()) {
  namez <- authors

  # aliases (kthids with more than one name variation)
  aliases_kthid <-
    namez %>%
    filter(!is.na(kthid) & !is.na(name)) %>%
    group_by(kthid) %>%
    mutate(n2 = n_distinct(name)) %>%
    filter(n2 > 1) %>%
    arrange(desc(n2)) %>%
    distinct(kthid, name, n2) %>%
    ungroup() %>%
    arrange(desc(n2), kthid, name) %>%
    select(kthid, name, n2)

  # aliases (orcids with more than one name variation)
  aliases_orcid <-
    namez %>%
    filter(!is.na(orcid) & !is.na(name)) %>%
    group_by(orcid) %>%
    mutate(n2 = n_distinct(name)) %>%
    filter(n2 > 1) %>%
    arrange(desc(n2)) %>%
    distinct(orcid, name, n2) %>%
    ungroup() %>%
    arrange(desc(n2), orcid, name) %>%
    select(orcid, name, n2)

  aliases_kthid %>%
    left_join(aliases_orcid) %>%
    select(kthid, orcid, n = n2, name) %>%
    collect()
}

diva_backup <- function(file) {

  ffr <- diva_tmp(file)
  fto <- diva_tmp(insert_ts(file))

  message("Backing up ", ffr, " to ", fto)
  file.rename(ffr, fto)

}

#' Refresh locally cached data files (and backup older data)
#' @return data frame with metadata including timestamp and age (in hours)
#' @export
diva_refresh <- function() {
  refreshed <- all(
    diva_backup("kth_diva_authors.rds"),
    diva_backup("kth_diva_pubs.rds")
  )
  if (!refreshed)
    warning("Not all files refreshed...")
  return (refreshed)
}

diva_tmp <- function(file)
  file.path(rappdirs::app_dir("kthcorpus")$config(), file)

insert_ts <- function(file) {
  file.path(paste0(tools::file_path_sans_ext(file), "_",
                   format(Sys.time(), "%y%m%d%H%M"), ".",
                   tools::file_ext(file))
  )
}

#' Metadata for cached data files
#' @return data frame with metadata including timestamp and age (in hours)
#' @export
diva_meta <- function() {

  sources <- c(
    "kth_diva_authors.rds",
    "kth_diva_pubs.rds"
  )

  timez <- file.mtime(diva_tmp(sources))

  age <- as.double(difftime(Sys.time(), timez, units = "h"))

  data.frame(source = sources, ts = timez, age = age)
}
