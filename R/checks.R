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
    distinct(orcid, kthid, n2, name) %>%
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

link_diva <- function(href, text) {
  if (nzchar(href) || nzchar(text))
    return (NA_character_)
  paste0("<a href='https://kth.diva-portal.org/smash/record.jsf?dswid=-310&pid=diva2%3A",
         href, "' target='_blank' rel='noopener noreferrer'>", text, "</a>")
}

link_DOI <- function(href, text) {
  if (nzchar(href) || nzchar(text))
    return (NA_character_)
  paste0("<a href='https://doi.org/", href,
     "' target='_blank' rel='noopener noreferrer'>", text, "</a>")
}

shorten <- function(x) {
  if (nchar(x) > 20) return(substr(x, 1, 20))
  x
}

check_multiplettes_article_title <- function(pubs = kth_diva_pubs()) {

  Year <- NULL
  n_check <- NULL

  article_title_multiplettes <-
    pubs %>%
    filter(grepl("^Artikel", PublicationType)) %>%
    group_by(Title) %>%
    count(Title) %>%
    arrange(desc(n)) %>%
    filter(n > 1)

  atm <-
    article_title_multiplettes %>%
    left_join(pubs %>% filter(grepl("^Artikel", PublicationType)) %>% select(Title, PID), by = "Title") %>%
    group_by(Title, n) %>%
    summarize(pids = paste0(collapse = " ", PID)) %>%
    arrange(desc(n)) %>%
    collect()

  # FIXME

  atm <-
    atm %>% tidyr::separate_rows(pids) %>%
    mutate(PID = as.double(pids)) %>%
    inner_join(pubs) %>%
    #mutate(year = lubridate::year(PublicationDate)) %>%
    select(Title, n, PID, Year)

  a <-
    kth_diva_authors() %>%
    filter(PID %in% atm$PID) %>%
    mutate(last_name = gsub("(.+?)(,.*)", "\\1", name)) %>%
    group_by(PID) %>%
    summarise(initials = paste0(collapse = "", substr(last_name, 1, 1)))

  atm %>%
    inner_join(a) %>%
    mutate(check_key = paste0(initials, "_", Year)) %>%
    select(Title, n, PID, check_key) %>%
    mutate(Title = link_diva(PID, Title)) %>%
    group_by(check_key) %>%
    add_count(check_key, sort = TRUE, name = "n_check") %>%
    arrange(desc(n_check), Title, check_key) %>%
    ungroup() %>%
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

check_multiplettes_DOI <- function(pubs = kth_diva_pubs()) {
  ScopusId <- NULL
  pubs %>%
    select(PID, DOI, ScopusId) %>%
    filter(!is.na(DOI)) %>%
    count(DOI) %>%
    filter(n > 1) %>%
    arrange(desc(n)) %>%
    inner_join(kth_diva_pubs(), by = "DOI") %>%
    select(DOI, n_pids = n, PID, Title, ScopusId) %>%
    mutate(Title = link_diva(PID, shorten(Title))) %>%
    mutate(DOI_link = link_DOI(DOI, DOI)) %>%
    collect()
}

check_multiplettes_scopusid <- function(pubs = kth_diva_pubs()) {
  ScopusId <- n_pids <- DOI_link <- NULL
  pubs %>%
  select(PID, DOI, ScopusId) %>%
  filter(!is.na(ScopusId)) %>%
  count(ScopusId) %>%
  filter(n > 1) %>%
  arrange(desc(n)) %>%
  inner_join(kth_diva_pubs(), by = "ScopusId") %>%
  select(DOI, n_pids = n, PID, Title, ScopusId) %>%
  mutate(Title = link_diva(PID, shorten(Title))) %>%
  mutate(DOI_link = link_DOI(DOI, DOI)) %>%
  collect() %>%
  select(ScopusId, DOI_link, n_pids, PID, Title)
}

check_multiplettes_ISI <- function(pubs = kth_diva_pubs()) {
  ScopusId <- n_pids <- n_scopusid <- NULL
  isi <- pubs %>%
    select(PID, ISI, ScopusId) %>%
    filter(!is.na(ScopusId)) %>%
    count(ISI) %>%
    filter(n > 1, !is.na(ISI)) %>%
    arrange(desc(n)) %>%
    inner_join(kth_diva_pubs(), by = "ISI") %>%
    select(ISI, n_pids = n, PID, Title, ScopusId) %>%
    collect() %>%
    select(ISI, ScopusId, n_pids, PID, Title) %>%
    group_by(ISI) %>%
    mutate(n_scopusid = n_distinct(ScopusId)) %>%
    filter(n_scopusid > 1)

  if (nrow(isi) > 0) {
    isi %>%
    arrange(desc(ISI)) %>%
    rowwise() %>%
    mutate(link_d = link_diva(PID, shorten(Title)))
  }

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
    uncertain_published = check_published(),
    multiplettes_scopusid = check_multiplettes_scopusid(),
    multiplettes_DOI = check_multiplettes_DOI(),
    multiplettes_ISI = check_multiplettes_ISI()
  )
}

#checks <- kth_diva_checks()

