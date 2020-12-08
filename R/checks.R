#' Potential data quality issues for DiVA author data
#'
#' This function summarizes some potential data quality issues for DiVA
#' author data, for example records with ORCIDs that relates to multiple
#' KTH author identifiers and vice versa. Some of these records may be
#' candidates for merging author data at the source.
#'
#' @param authors a tibble with authors data, default: kth_diva_authors()
#' @return a list with slots for data frames (details, overview, and associated
#' publications)
#' @export
#' @import dplyr
kth_diva_issues <- function(authors = kth_diva_authors()) {
  namez <- authors

  # orcids which relates to more than one kthid
  # these could be data quality issues?
  details <-
    namez %>%
    filter(!is.na(orcid) & !is.na(kthid)) %>%
    group_by(orcid) %>%
    mutate(n1 = n_distinct(name), n2 = n_distinct(kthid)) %>%
    filter(n2 > 1) %>%
    distinct(orcid, kthid, n2, name, Name) %>%
    ungroup() %>%
    arrange(orcid, kthid, desc(n2), name) %>%
    select(orcid, kthid, n = n2) %>%
    inner_join(namez) %>%
    distinct(orcid, kthid, pids) %>%
    collect()

  overview <-
    details %>%
    group_by(orcid) %>%
    count(kthid) %>%
    arrange(orcid, desc(n)) %>%
    collect()

  # publications for kthids that have several different orcids
  pubs <-
    namez %>%
    left_join(namez %>%
                filter(!is.na(orcid) & !is.na(kthid)) %>%
                distinct(orcid, kthid), by = "kthid") %>%
    filter(orcid.x != orcid.y) %>%
    select(kthid, orcid.x, orcid.y, everything()) %>%
    arrange(desc(kthid)) %>%
    collect()

  list(overview = overview, details = details, pubs = pubs)
}

check_multiplettes_article_title <- function(pubs = kth_diva_pubs()) {

  article_title_multiplettes <-
    pubs %>%
    filter(grepl("^Artikel", PublicationType)) %>%
    group_by(Title) %>%
    count(Title) %>%
    arrange(desc(n)) %>%
    filter(n > 1)

  article_title_multiplettes %>%
    left_join(pubs %>% select(Title, PID), by = "Title") %>%
    group_by(Title, n) %>%
    summarize(pids = paste0(collapse = " ", PID)) %>%
    arrange(desc(n)) %>%
    collect()
}

check_invalid_submission_status <- function(pubs = kth_diva_pubs()) {
  pubs %>%
    filter(Status == "submitted" & (!is.na(DOI) | !is.na(ISI))) %>%
    collect()
}

check_missing_kthid <- function(authors = kth_diva_authors()) {
  authors %>%
    filter(!is.na(orgids) & is.na(kthid))  %>%
    collect()
}

check_missing_confpubdate <- function(pubs = kth_diva_pubs()) {
  pubs %>%
    filter(PublicationType == "Konferensbidrag") %>%
    filter(is.na(PublicationDate))  %>%
    collect()
}

check_missing_journals_identifiers <- function(pubs = kth_diva_pubs()) {
  pubs %>%
    filter(PublicationType == "Artikel i tidskrift") %>%
    filter(is.na(JournalISSN) & is.na(JournalEISSN))  %>%
    collect()
}

check_titles_book_chapters <- function(pubs = kth_diva_pubs()) {

  t1 <-
    pubs %>%
      filter(PublicationType == "Kapitel i bok, del av antologi")

  t1 %>%
    group_by(Title) %>%
    count(Title) %>%
    filter(n > 1, nchar(Title) < 20) %>%
    arrange(desc(n)) %>%
    inner_join(t1 %>% select(PID, Title), by = "Title") %>%
    group_by(Title, n) %>%
    summarize(pids = paste0(collapse = " ", PID))  %>%
    collect()

}

check_invalid_ISI <- function(pubs = kth_diva_pubs()) {

  pubs %>%
    filter(!is.na(ISI)) %>%
    filter(nchar(ISI) != 15, !grepl("^A1", ISI), !grepl("^000", ISI))  %>%
    collect()

}

check_invalid_DOI <- function(pubs = kth_diva_pubs()) {

  pubs %>%
    filter(!is.na(DOI)) %>%
    filter(!grepl("^10", DOI)) %>%
    select(DOI, PID)  %>%
    collect()

}

check_invalid_ISSN <- function(pubs = kth_diva_pubs()) {

  pubs %>%
    filter(!is.na(JournalISSN)) %>%
    filter(!grepl("\\d{4}-\\d{3}(X|\\d)", JournalISSN)) %>%
    select(JournalISSN, PID) %>%
    collect()

}

check_invalid_orgid <- function(pubs = kth_diva_pubs()) {
  # TODO: Gaël's code
  data.frame()
}

check_published <- function(pubs = kth_diva_pubs()) {

  t1 <-
    pubs %>%
    mutate(has_notes = grepl("^QP", Notes)) %>%
    mutate(is_not_published = !is.na(Status) &
             (Status %in% c("accepted", "aheadofprint", "inPress"))) %>%
    filter(has_notes, is_not_published)

  t2 <-
    pubs %>%
    mutate(is_QSorNQC = grepl("^QC|^NQC", Notes)) %>%
    filter(Status == "submitted",
           is_QSorNQC | PublicationType == "Manuskript (preprint)")

  bind_rows(t1, t2) %>%
    select(PID, PublicationType, has_notes,
           is_not_published, Notes, is_QSorNQC)  %>%
    collect()
}

#' Data quality checks for DiVA publication data
#'
#' This function makes a number of checks based on publications and author data
#'
#' @return a list with slots for data frames with check results
#' @export
kth_diva_checks <- function() {

  list(
    article_title_multiplettes = check_multiplettes_article_title(),
    submission_status_invalid = check_invalid_submission_status(),
    missing_kthid = check_missing_kthid(),
    missing_confpubdate = check_missing_confpubdate(),
    missing_journal_ids = check_missing_journals_identifiers(),
    odd_book_chapters = check_titles_book_chapters(),
    invalid_ISI = check_invalid_ISI(),
    invalid_DOI = check_invalid_DOI(),
    invalid_ISSN = check_invalid_ISSN(),
    invalid_orgid = check_invalid_orgid(),
    uncertain_published = check_published()
  )
}

#checks <- kth_diva_checks()

#' Issues with publication and author affiliations

#' @param pass passphrase required for accessing the data,
#'   Default: Sys.getenv("DIVA_PASS")
#' @param jq query for [filtering the data](https://docs.ropensci.org/jqr/)
#' @return json object
#' @examples
#' \dontrun{
#'  kth_issues_pubauth()
#' }
#' @seealso
#'  \code{\link[rappdirs]{app_dir}}
#'  \code{\link[rcrypt]{decrypt}}
#'  \code{\link[readr]{read_file}}
#' @export
#' @importFrom rappdirs app_dir
#' @importFrom rcrypt decrypt
#' @importFrom readr read_lines
#' @importFrom ndjson stream_in
#' @importFrom jqr jq
kth_issues_pubauth <- function(pass = Sys.getenv("DIVA_PASS"), jq = NULL) {

  stopifnot(nzchar(pass))

  diva_tmp <- function(file) {
    fp <- file.path(rappdirs::app_dir("diva")$config(), file)
    if (!dir.exists(dirname(fp)))
      dir.create(dirname(fp), recursive = TRUE)
    fp
  }

  if (!file.exists(diva_tmp("ap.json"))) {
    rcrypt::decrypt(
      system.file(package = "diva", "extdata", "ap.rcrypt"),
      diva_tmp("ap.json"),
      passphrase = pass
    )
  }

  if (!file.exists(diva_tmp("ap.json")))
    return(character(0))

  # if no jq query is specified, return all data in flat format
  if (is.null(jq))
    return(ndjson::stream_in(diva_tmp("ap.json"), cls = "tbl"))



  cb <- function(x, pos) {
    jqr::jq(x, jq) %>% #, flags = jqr::jq_flags(ascii = TRUE)) %>%
      textConnection() %>%
      jsonlite::stream_in(simplifyDataFrame = FALSE, verbose = FALSE)# %>%
      #jsonlite::toJSON(pretty = TRUE) %>%
      #as.character()
  }

  res <-
    readr::read_lines_chunked(diva_tmp("ap.json"),
      callback = ListCallback$new(cb), chunk_size = 9e4)
    #readLines(diva_tmp("ap.json"), encoding = "utf-8") %>%
    #gsub(., pattern = "⚑", replacement = "F", fixed = TRUE) %>%
    #gsub(., pattern = "⚠⚠", replacement = "WTF", fixed = TRUE) %>%
    #jqr::jq(jq) %>% #, flags = jqr::jq_flags(ascii = TRUE)) %>%
    #textConnection() %>%
    #jsonlite::stream_in(simplifyDataFrame = FALSE, verbose = FALSE) %>%
    #jsonlite::toJSON()

  return(res)
}

#jsonlite::stream_in(textConnection(readLines(diva_tmp("ap.json")))) %>%
#as_tibble() %>% toJSON()select(kthid) %>% unique() %>% filter(grepl("^⚠", kthid))

# readLines("data-raw/dontshare/test5.json", encoding = "utf-8") %>%
# #  gsub(., pattern = "⚑", replacement = "F", fixed = TRUE) %>%
# #  gsub(., pattern = "⚠⚠", replacement = "WTF", fixed = TRUE) %>%
#   jqr::jq("[.kthid, .orcid, .kth, .note] | @csv", flags = jqr::jq_flags(stream = TRUE)) %>%
#   #textConnection() %>%
#   read_csv(quote = "\"")
#   jsonlite::stream_in(simplifyDataFrame = FALSE ,verbose = FALSE) %>%
#   jsonlite::toJSON()

