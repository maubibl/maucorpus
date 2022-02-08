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

paste_na <- function(x, y)
  tidyr::unite(tibble::tibble(x = x, y = y), "xy", x, y, na.rm = TRUE)$xy

blank <- function(x) ifelse(is.na(x), "", x)

link_general <- function(href, text) {

  if (missing(text)) text <- href

  if (!nzchar(href) || !nzchar(text))
    return (NA_character_)

  sprintf("<a href='%s' target='_blank' rel='noopener noreferrer' title='%s'>%s</a>",
          blank(href), blank(text), blank(shorten(text))
  )

}


#' Convert a string with a URL to a link
#'
#' @param href the link
#' @param text the text to show for the link, by default shortened href
#' @param title the tooltip, by default the href
#' @param target the type of link to make
#' @importFrom stringr str_c
#' @export
linkify <- function(href, text = shorten(href), title = href, target =
  c("href", "PID", "titlesearch", "freetextsearch", "DOI", "ScopusID", "ISSN",
    "ISBN", "ISI", "ORCID")) {

  if (missing(target)) target <- "href"

  pattern <-
    "<a href='%s' target='_blank' rel='noopener noreferrer' title='%s'>%s</a>"

  p_pattern <- function(x, y, z) {
    stringr::str_c(
      "<a href='",
      x,
      "' target='_blank' rel='noopener noreferrer' title='",
      y,
      "'>",
      z,
      "</a>"
      )
  }

  p_PID <- function(x) {
    stringr::str_c(
      "https://kth.diva-portal.org/smash/record.jsf?dswid=-310&pid=diva2%3A",
      x
    )
  }

  p_ISSN <- function(x) {
    stringr::str_c(
      "https://portal.issn.org/api/search?search[]=MUST=allissnbis=%22",
      x, "%22")
  }

  p_ScopusID <- function(x) {
    stringr::str_c(
      "https://www.scopus.com/record/display.url?origin=inward&partnerID=40&eid=",
      x
    )
  }

  p_DOI <- function(x) {
    stringr::str_c(
      "https://doi.org/",
      x
    )
  }

  search_title <- function(term)
    stringr::str_c(
      "https://kth.diva-portal.org/smash/resultList.jsf",
      "?aq2=%5B%5B%5D%5D&af=%5B%5D&searchType=RESEARCH",
      "&sortOrder2=title_sort_asc&language=en",
      "&aq=%5B%5B%7B%22titleAll%22%3A%22",
      utils::URLencode(term),
      "%22%7D%5D%5D",
      "&sf=all&aqe=%5B%5D&sortOrder=author_sort_asc",
      "&onlyFullText=false&noOfRows=50&dswid=-4347"
    )

  search_free <- function(term)
    stringr::str_c(
      "https://kth.diva-portal.org/smash/resultList.jsf",
      "?language=en&searchType=RESEARCH&",
      "query=&af=%5B%5D&aq=%5B%5B%7B%22freeText%22%3A%22",
      utils::URLencode(term),
      "%22%7D%5D%5D&aq2=%5B%5B%5D%5D&aqe=%5B%5D&noOfRows=50",
      "&sortOrder=author_sort_asc&sortOrder2=title_sort_asc",
      "&onlyFullText=false&sf=all"
    )

  p_ISBN <- function(x) {
    # WorldCat-sökning på ISBN:
    # https://www.worldcat.org/search?q=bn%3A978-91-7501-918-5&qt=advanced&dblist=638
    stringr::str_c(
      "https://www.worldcat.org/search?q=bn%3A",
      x,
      "&qt=advanced&dblist=638"
    )
  }

  p_ISI <- function(x) {
    # Web of Science, sökning på UT=WoS-id:
    # http://apps.webofknowledge.com/InboundService.do?product=WOS&action=retrieve&mode=FullRecord&UT=000722432000004
    stringr::str_c(
      "http://apps.webofknowledge.com/InboundService.do?product=WOS&action=retrieve&mode=FullRecord&UT=",
      x
    )
  }

  p_ORCID <- function(x) {
    #sprintf("https://orcid.org/%s", blank(orcid)), orcid))
    stringr::str_c("https://orcid.org/", x)
  }

  href_modified <- switch (target,
    href = href,
    PID = p_PID(href),
    titlesearch = search_title(href),
    freetextsearch = search_free(href),
    DOI = p_DOI(href),
    ScopusID = p_ScopusID(href),
    ISSN = p_ISSN(href),
    ISBN = p_ISBN(href),
    UT = p_ISI(href),
    ORCID = p_ORCID(href),
    href
  )

  p_pattern(
    href_modified, title, text
  )

}

link_diva <- function(href, text) {
  if (!nzchar(href) || !nzchar(text))
    return (NA_character_)
  paste0("<a href='https://kth.diva-portal.org/smash/record.jsf?dswid=-310&pid=diva2%3A",
         href, "' target='_blank' rel='noopener noreferrer' title='", text, "'>", shorten(text), "</a>")
}

link_DOI <- function(href, text) {
  if (!nzchar(href) || !nzchar(text))
    return (NA_character_)
  paste0("<a href='https://doi.org/", href,
     "' target='_blank' rel='noopener noreferrer' title='", text, "'>", shorten(text), "</a>")
}

link_ScopusID <- function(href, text) {
  if (!nzchar(href) || !nzchar(text))
    return (NA_character_)
  paste0("<a href='https://www.scopus.com/record/display.url?origin=inward&partnerID=40&eid=", href,
         "' target='_blank' rel='noopener noreferrer' title='", text, "'>", shorten(text), "</a>")
}

#' @importFrom utils URLencode
link_titlesearch <- function(title, text) {

  if (missing(text)) text <- title

  if (!nzchar(title) || !nzchar(text))
    return (NA_character_)

  title_search <- function(term)
    paste0("https://kth.diva-portal.org/smash/resultList.jsf",
           "?aq2=%5B%5B%5D%5D&af=%5B%5D&searchType=RESEARCH",
           "&sortOrder2=title_sort_asc&language=en",
           "&aq=%5B%5B%7B%22titleAll%22%3A%22", utils::URLencode(term), "%22%7D%5D%5D",
           "&sf=all&aqe=%5B%5D&sortOrder=author_sort_asc&onlyFullText=false&noOfRows=50&dswid=-4347")

  sprintf("<a href='%s' target='_blank' rel='noopener noreferrer' title='%s'>%s</a>",
          title_search(title), text, shorten(text))

}

link_freetextsearch <- function(title, text) {

  if (missing(text)) text <- title

  if (!nzchar(title) || !nzchar(text))
    return (NA_character_)

  ft_search <- function(term)
    paste0("https://kth.diva-portal.org/smash/resultList.jsf",
           "?language=en&searchType=RESEARCH&",
           "query=&af=%5B%5D&aq=%5B%5B%7B%22freeText%22%3A%22",
           utils::URLencode(term),
           "%22%7D%5D%5D&aq2=%5B%5B%5D%5D&aqe=%5B%5D&noOfRows=50",
           "&sortOrder=author_sort_asc&sortOrder2=title_sort_asc",
           "&onlyFullText=false&sf=all")

  sprintf("<a href='%s' target='_blank' rel='noopener noreferrer' title='%s'>%s</a>",
          blank(ft_search(title)), blank(text), blank(shorten(text)))
}

link_ISSNsearch <- function(title, text) {

  if (missing(text)) text <- title

  if (!nzchar(title) || !nzchar(text))
    return (NA_character_)

  issn_search <- function(term)
    paste0("https://portal.issn.org/api/search?search[]=MUST=allissnbis=%22", term, "%22")

  sprintf("<a href='%s' target='_blank' rel='noopener noreferrer' title='%s'>%s</a>", issn_search(title), text, shorten(text))

}

shorten <- function(x, w = 25) {
  stringr::str_trunc(x, width = w)
  #ifelse(nchar(x) > 20, paste0(substr(x, 1, 20), "..."), x)
}

check_multiplettes_article_title <- function(pubs = kth_diva_pubs()) {

  Year <- n_check <- LastUpdated <- NULL

  article_title_multiplettes <-
    pubs %>%
    filter(grepl("^Artikel", PublicationType)) %>%
    group_by(Title) %>%
    count(Title) %>%
    arrange(desc(n)) %>%
    filter(n > 1)

  atm <-
    article_title_multiplettes %>%
    left_join(
      pubs %>%
        filter(grepl("^Artikel", PublicationType)) %>%
        select(Title, PID)
      , by = "Title") %>%
    group_by(Title, n) %>%
    summarize(pids = paste0(collapse = " ", PID)) %>%
    arrange(desc(n)) %>%
    collect()

  # FIXME

  atm <-
    atm %>% tidyr::separate_rows(pids) %>%
    mutate(PID = as.double(pids)) %>%
    inner_join(pubs, by = c("Title", "PID")) %>%
    #mutate(year = lubridate::year(PublicationDate)) %>%
    select(Title, n, PID, Year, LastUpdated)

  a <-
    kth_diva_authors() %>%
    filter(PID %in% atm$PID) %>%
    mutate(last_name = gsub("(.+?)(,.*)", "\\1", name)) %>%
    group_by(PID) %>%
    summarise(initials = paste0(collapse = "", substr(last_name, 1, 1)))

  atm %>%
    inner_join(a, by = "PID") %>%
    mutate(check_key = paste0(initials, "_", Year)) %>%
    select(PID, Title, LastUpdated, check_key, n) %>%
    mutate(PID = link_diva(PID, PID)) %>%
    mutate(Title = link_titlesearch(Title, Title)) %>%
    group_by(check_key) %>%
    add_count(check_key, sort = TRUE, name = "n_check") %>%
    arrange(desc(LastUpdated), desc(check_key), desc(n_check), desc(Title)) %>%
    ungroup() %>%
    collect() %>%
    select(PID, Title, check_key, n, n_check, LastUpdated)

}

#' Clean HTML tags from string
#' @param x the string to clean out HTML tags from
#' @export
#' @importFrom rvest read_html html_text
tidy_html <- function(x)
  rvest::html_text(rvest::read_html(charToRaw(x)))

check_multiplettes_title <- function(pubs = kth_diva_pubs()) {

  Year <- LastUpdated <- clean_notes <- n_check <- NULL

  title_multiplettes <-
    pubs %>%
    #  filter(grepl("^Artikel", PublicationType)) %>%
    group_by(Title, PublicationType) %>%
    count(Title) %>%
    arrange(desc(n)) %>%
    filter(n > 1)

  tm <-
    title_multiplettes %>%
    left_join(
      pubs %>%
        #filter(grepl("^Artikel", PublicationType)) %>%
        select(Title, PID, PublicationType, DOI)
      , by = c("Title", "PublicationType")) %>%
    group_by(Title, n, PublicationType) %>%
    summarize(pids = paste0(collapse = " ", PID), .groups = "drop") %>%
    arrange(desc(n)) %>%
    collect()

  tm <-
    tm %>%
    tidyr::separate_rows(pids) %>%
    mutate(PID = as.double(pids)) %>%
    inner_join(pubs, by = c("Title", "PID", "PublicationType")) %>%
    #mutate(year = lubridate::year(PublicationDate)) %>%
    select(Title, n, PID, Year, LastUpdated, PublicationType, Notes, JournalISSN, JournalEISSN) %>%
    rowwise() %>%
    mutate(clean_notes = tidy_html(Notes)) %>%
#    count(JournalISSN, JournalEISSN)
    filter(JournalISSN == JournalEISSN | any(is.na(c(JournalISSN, JournalEISSN))))

  a <-
    kth_diva_authors() %>%
    filter(PID %in% tm$PID) %>%
    mutate(last_name = gsub("(.+?)(,.*)", "\\1", name)) %>%
    group_by(PID) %>%
    summarise(initials = paste0(collapse = "", substr(last_name, 1, 1)))

  tm %>%
    inner_join(a, by = "PID") %>%
    mutate(check_key = paste0(initials, "_", Year)) %>%
    select(PID, Title, PublicationType, LastUpdated, check_key, n, JournalISSN, JournalEISSN, clean_notes) %>%
    group_by(check_key) %>%
    add_count(check_key, sort = TRUE, name = "n_check") %>%
    arrange(desc(n_check), desc(check_key), desc(LastUpdated), desc(Title)) %>%
    ungroup() %>%
    mutate(PID = linkify(PID, target = "PID")) %>%
    mutate(Title = linkify(Title, target = "titlesearch")) %>%
    collect() %>%
    select(PID, Title, PublicationType, n_check, n, LastUpdated, JournalISSN, JournalEISSN, check_key, clean_notes)

}

check_invalid_submission_status <- function(pubs = kth_diva_pubs()) {

  # possibly a Zenodo DOI could exist, but in general no identifier
  # has yet been assigned if "only" submitted

  ScopusId <- ISI <- DOI <- NULL

  pubs %>%
    filter(Status == "submitted" &
        (!is.na(DOI) | !is.na(ISI) | !is.na(ScopusId))) %>%
    collect()
}

check_missing_kthid <- function(authors = kth_diva_authors()) {

  LastUpdated <- NULL

  authors %>%
    filter(!is.na(orgids) & is.na(kthid))  %>%
    collect() %>%
    inner_join(kth_diva_pubs() %>% select(PID, LastUpdated), by = "PID") %>%
    mutate(PID = linkify(PID, target = "PID")) %>%
    mutate(pids = linkify(pids, target = "freetextsearch")) %>%
    mutate(name = linkify(name, target = "freetextsearch")) %>%
    mutate(orgids = linkify(orgids, target = "freetextsearch")) %>%
    mutate(extorg = linkify(extorg, target = "freetextsearch")) %>%
    mutate(orcid = linkify(orcid, target = "ORCID")) %>%
    select(PID, name, LastUpdated, everything())

}

check_missing_date <- function(pubs = kth_diva_pubs()) {
  # TODO: PublicationDate may not be needed (earlier such issues are fixed)
  Year <- NULL
  pubs %>%
    #filter(PublicationType == "Konferensbidrag") %>%
    filter(is.na(PublicationDate), is.na(Year))  %>%
    collect()
}

check_missing_journals_identifiers <- function(pubs = kth_diva_pubs()) {

  ScopusId <- Journal <- LastUpdated <- NULL

  step <-
    pubs %>%
    filter(PublicationType == "Artikel i tidskrift") %>%
    filter(is.na(JournalISSN) & is.na(JournalEISSN))  %>%
    collect()

  step %>%
    mutate(PID = link_diva(PID, Title)) %>%
    mutate(Title = linkify(Title, target = "titlesearch")) %>%
    mutate(DOI = linkify(DOI, target = "DOI")) %>%
    mutate(ScopusId = linkify(ScopusId, target = "ScopusID")) %>%
    mutate(Name = linkify(Name, target = "freetextsearch")) %>%
    mutate(Journal = linkify(Journal, target = "freetextsearch")) %>%
    select(PID, Title, LastUpdated, starts_with("Journal"), DOI, ScopusId, Name)
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

  ISI <- NULL

  pubs %>%
    filter(!is.na(ISI)) %>%
    filter(nchar(ISI) != 15, !grepl("^A1", ISI), !grepl("^000", ISI))  %>%
    collect()

}

check_invalid_DOI <- function(pubs = kth_diva_pubs()) {

  pubs %>%
    filter(!is.na(DOI)) %>%
    filter(!grepl("^10[.]", DOI)) %>%
    select(DOI, PID)  %>%
    collect()

}

check_multiplettes_DOI <- function(pubs = kth_diva_pubs()) {

  ScopusId <- DOI_link <- Scopus_link <- n_pids <- LastUpdated <- NULL

  pubs %>%
    select(PID, DOI) %>%
    filter(!is.na(DOI)) %>%
    count(DOI) %>%
    filter(n > 1) %>%
    arrange(desc(n)) %>%
    inner_join(kth_diva_pubs(), by = "DOI") %>%
    select(PID, Title, n_pids = n, DOI, ScopusId, LastUpdated, ISI) %>%
    mutate(PID = linkify(PID, target = "PID")) %>%
    mutate(Title = linkify(Title, target = "titlesearch")) %>%
    mutate(DOI = linkify(DOI, target = "DOI")) %>%
    mutate(ScopusId = linkify(ScopusId, target = "ScopusId")) %>%
    mutate(ISI = linkify(ISI, target = "ISI")) %>%
    collect() %>%
    select(PID, Title, n_pids, DOI, ISI, ScopusId, LastUpdated) %>%
    arrange(desc(n_pids), desc(DOI), desc(LastUpdated))
}

check_multiplettes_scopusid <- function(pubs = kth_diva_pubs()) {

  ScopusId <- n_pids <- DOI_link <- ScopusId_link <- LastUpdated <-  NULL

  pubs %>%
    select(PID, DOI, ScopusId) %>%
    filter(!is.na(ScopusId)) %>%
    count(ScopusId) %>%
    filter(n > 1) %>%
    arrange(desc(n)) %>%
    inner_join(kth_diva_pubs(), by = "ScopusId") %>%
    select(DOI, n_pids = n, PID, Title, ScopusId, ISI, LastUpdated) %>%
    mutate(PID = linkify(PID, target = "PID")) %>%
    mutate(Title = linkify(Title, target = "titlesearch")) %>%
    mutate(DOI = linkify(DOI, target = "DOI")) %>%
    mutate(ScopusId = linkify(ScopusId, target = "ScopusId")) %>%
    mutate(ISI = linkify(ISI, target = "ISI")) %>%
    collect() %>%
    select(PID, Title, n_pids, ScopusId, DOI, ISI, LastUpdated) %>%
    arrange(desc(n_pids), desc(ScopusId), desc(LastUpdated))
}

check_multiplettes_ISI <- function(pubs = kth_diva_pubs()) {

  # ISI = UT = WoS identifier

  ScopusId <- n_pids <- n_scopusid <- LastUpdated <- NULL

  pubs %>%
  select(PID, ISI, ScopusId) %>%
  count(ISI) %>%
  filter(n > 1, !is.na(ISI)) %>%
  arrange(desc(n)) %>%
  inner_join(kth_diva_pubs(), by = "ISI") %>%
  select(ISI, n_pids = n, PID, Title, ScopusId, LastUpdated, DOI) %>%
  collect() %>%
  select(PID, Title, n_pids, ISI, DOI, ScopusId, LastUpdated) %>%
  mutate(PID = linkify(PID, target = "PID")) %>%
  mutate(ISI = linkify(ISI, target = "ISI")) %>%
  mutate(DOI = linkify(DOI, target = "DOI")) %>%
  mutate(ScopusId = linkify(ScopusId, target = "ScopusID")) %>%
  mutate(Title = linkify(Title, target = "titlesearch")) %>%
  arrange(desc(n_pids), desc(ISI), desc(LastUpdated))

}

check_invalid_ISSN <- function(pubs = kth_diva_pubs()) {

  # TODO: also check JournalEISSN here too..
  # https://portal.issn.org/api/search?search[]=MUST=allissnbis=%220003-682X%22

  re <- "\\d{4}-\\d{3}(X|x|\\d)"

  pubs %>%
    filter(!is.na(JournalISSN) & !is.na(JournalEISSN)) %>%
    filter(!grepl(re, JournalISSN) & !grepl(re, JournalEISSN)) %>%
    select(JournalISSN, JournalEISSN, PID) %>%
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

  checks <- list(
    article_title_multiplettes = check_multiplettes_article_title(),
    title_multiplettes = check_multiplettes_title(),
    submission_status_invalid = check_invalid_submission_status(),
    missing_kthid = check_missing_kthid(),
    missing_confpubdate = check_missing_date(),
    missing_journal_ids = check_missing_journals_identifiers(),
    odd_book_chapters = check_titles_book_chapters(),
    invalid_ISI = check_invalid_ISI(),
    invalid_DOI = check_invalid_DOI(),
    invalid_ISSN = check_invalid_ISSN(),
    invalid_orgid = check_invalid_orgid(),
    uncertain_published = check_published(),
    multiplettes_scopusid = check_multiplettes_scopusid(),
    multiplettes_DOI = check_multiplettes_DOI(),
    multiplettes_ISI = check_multiplettes_ISI(),
    swepub = swepub_checks()
  )

  stats <-
    checks %>%
    purrr::map(function(x) ifelse(is.null(x), NA, nrow(x))) %>%
    tibble::as_tibble() %>%
    tidyr::pivot_longer(cols = everything())

  checks$stats <- stats

  checks

}

#' Remaps column names using the internal check_mapping
#' @param x character vector of column names
#' @return character vector of new column names
#' @export
#' @importFrom tibble tibble
#' @importFrom dplyr left_join
#' @importFrom stats setNames
check_remap_colnames <- function(x) {

  desc_swe <- colname_en <- NULL

  colnamez <- kthcorpus::check_mapping

  nm <-
    tibble(colname_en = x) %>%
    left_join(colnamez, by = "colname_en") %>%
    mutate(desc_swe = ifelse(is.na(desc_swe), colname_en, desc_swe))

  setNames(nm$colname_en, nm$desc_swe)
}

# https://rdrr.io/cran/libbib/man/get_issn_check_digit.html
#library(libbib)
#libbib::get_issn_check_digit()
