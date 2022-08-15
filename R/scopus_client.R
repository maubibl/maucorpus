#' @importFrom lubridate ymd
#' @noMd
#'
format_date <- function(x) {
  ymd(x) %>% remove_char()
}

compact <- function (l) Filter(Negate(is.null), l)

remove_char <- function(x, char = "-") {
  gsub(char, "", x)
}

#' @importFrom httr status_code
#' @noMd
scopus_check_status <- function(res){
  attempt::stop_if_not(
    .x = httr::status_code(res),
    .p = ~ .x == 200,
    msg = paste0("The API returned an error: ", content(res)))
}

#' Configuration for credentials when using Scopus API
#'
#' Use .Renviron to provide credentials for API access.
#'
#' Set values for SCOPUS_API_KEY and optionally for SCOPUS_API_TOKEN if making
#' requests from outside of institutional IPs linked to the subscriber.
#'
#'      file.edit("~/.Renviron")
#'      readRenviron("~/.Renviron")
#' @param quiet whether to display messages for missing environment varibles,
#' by default FALSE
#' @export
scopus_config <- function(quiet = FALSE) {

  key <- Sys.getenv("SCOPUS_API_KEY")
  token <- Sys.getenv("SCOPUS_API_INSTTOKEN")

  if (key == "") {
    if (!quiet) message("No institutional token provided, please use .Renviron to set it in SCOPUS_API_KEY")
    key <- ""
  }

  if (token == "") {
    if (!quiet) message("No institutional token provided, please use .Renviron to set it in SCOPUS_API_INSTTOKEN")
    token <- NULL
  }

  list(
    apiKey = key,
    insttoken = token
  )
}

#' Retrieve publications from Scopus API from KTH - The Royal Institute of Technology.
#'
#' This function allows for using the "load date" when fetching publications for
#' KTH - The Royal Institute of Technology. By default the time interval for the
#' previous two weeks is used.
#'
#' Note: when using a subscriber API key, requests are only allowed from
#' institutional IPs. From Elsevier's API documentation:
#'
#' "Elsevier Research Products APIs rely primarily on Institutional IP address
#' for authentication. API access through proxies is not supported, however
#' Elsevier will provide remote access direct to the APIs using a special
#' access credential ("Institutional Token").
#' If you are working away from your main institutional network
#' or your institution accesses Scopus.com, Scival.com, or ScienceDirect.com
#' through a proxy, please contact us to enquire about Institutional Token access."
#'
#' The rate limits that apply for using Scopus Search is a max paging length of 25 for
#' complete views with a 5000 item total results limit and weekly 20k results and at
#' the most 9 requests per second.
#'
#' @param beg_loaddate date expressed as "yyyymmdd", by default current date minus 7 days
#' @param end_loaddate date expressed as "yyyymmdd", by default current date
#' @importFrom httr GET add_headers content
#' @importFrom glue glue
#' @importFrom purrr map_chr possibly pmap map map_df
#' @importFrom dplyr bind_cols
#' @importFrom progress progress_bar
#' @export
scopus_search_pubs_kth <- function(beg_loaddate, end_loaddate) {

  if (missing(end_loaddate))
    end_loaddate <- Sys.Date() %>% format_date()

  if (missing(beg_loaddate))
    beg_loaddate <- (Sys.Date() - 14) %>% format_date()

  beg_pubyear <- 2019L
  id_affiliation <- 60002014L

  criteria <- glue::glue(
    'AFFIL(((kth*) OR (roy* AND inst* AND tech*) OR ("Roy. Inst. T") OR ',
    '(alfven) OR (kung* AND tek* AND hog*) OR (kung* AND tek* AND h\\u00f6g*) OR ',
    '(kgl AND tek* AND hog*) OR (kung* AND tek* AND hg*) OR ',
    '(roy* AND tech* AND univ*)) AND (Sweden)) OR ',
    'AF-ID("The Royal Institute of Technology KTH" {id_affiliation}) AND ',
    'orig-load-date aft {beg_loaddate} AND pubyear aft {beg_pubyear}'
  )

  # req <- httr::GET("https://api.elsevier.com/content/search/scopus",
  #   query = list(
  #     query = criteria,
  #     apiKey = scopus_config()$apikey,
  #     view = "COMPLETE"
  #   ),
  #   add_headers("Content-Type" = "application/xml")
  # )

  req <- scopus_req(criteria, start = 0, count = 25)

  xml <- httr::content(req)

  hits <- as.integer(xml$`search-results`$`opensearch:totalResults`)
  page_length <- as.integer(xml$`search-results`$`opensearch:itemsPerPage`)
  pages <- purrr::map_chr(xml$`search-results`$link, "@href")
  res <- xml$`search-results`$entry

  if (length(pages) > 1) {
    message("Crawling pages, found ",
      glue::glue("{hits} hits, in {length(pages)} pages, each w at most {page_length} entries"))

    # count pages manually because the search results links appear to be wrong!
    urls <- pages[1:length(pages)]

    cri <- criteria

    mypager <-
      pager(hits, page_length) %>%
      dplyr::bind_cols(tibble(criteria = cri)) %>%
      select(c("criteria", "start", "count"))

    pb <- progress::progress_bar$new(
      format = "  scopus search [:bar] :percent eta: :eta",
      total = nrow(mypager), clear = FALSE, width = 60)

    crawl <- purrr::possibly(
      .f = function(criteria, start, count) {
        pb$tick()
        scopus_crawl(criteria, start, count)
      },
      otherwise = list()
    )

    therest <- mypager %>% purrr::pmap(crawl)

    message("Parsing and flattening nested data...")
    results <- therest %>% purrr::map(parse_scopus_entries)

    merged <- list(
      publications = purrr::map_df(results, "publications") %>% unique(),
      authors = purrr::map_df(results, "authors") %>% unique(),
      affiliations = purrr::map_df(results, "affiliations") %>% unique()
    )

    return(merged)

  }

  return(parse_scopus_entries(res))

}

scopus_req <- function(criteria, start, count) {
  resp <- httr::GET("https://api.elsevier.com/content/search/scopus",
    query = compact(list(
      query = criteria,
      apiKey = scopus_config(quiet=TRUE)$apiKey,
      insttoken = scopus_config(quiet=TRUE)$insttoken,
      view = "COMPLETE",
      sort = "orig-load-date",
      start = start,
      count = count
    )),
    httr::add_headers("Content-Type" = "application/xml"),
    httr::timeout(10L)
  )

  scopus_check_status(resp)

  if (httr::http_type(resp) != "application/json") {
    stop(sprintf("API returned %s", httr::http_type(resp)), call. = FALSE)
  }

  if (httr::status_code(resp) != 200) {
    hh <- httr::headers(resp)
    rl_total <- as.integer(hh$`x-ratelimit-limit`)
    rl_remaining <- as.integer(hh$`x-ratelimit-remaining`)
    rl_reset_ts <- as.integer(hh$`x-ratelimit-reset`)

    stop(
      sprintf(
        "API request failed [%s]\n%s\n<%s>",
        httr::status_code(resp),
        paste0("Rate Limit quota setting:", rl_total),
        paste0("Remaining RL quota: ", rl_remaining, " which resets at ", rl_reset_ts)
      ),
      call. = FALSE
    )
  }

  return (resp)
}

scopus_crawl <- function(criteria, start, count) {
  #res <- httr::content(httr::GET(url, add_headers("Content-Type" = "application/xml")))
  res <- httr::content(scopus_req(criteria, start, count))
  res$`search-results`$entry
}

#' @noRd
#' @importFrom readr read_lines
#' @importFrom purrr map_chr
scopus_fields <- function() {
      I("prism:url
      dc:identifier
      eid
      dc:title
      dc:creator
      prism:publicationName
      prism:eIssn
      prism:volume
      prism:issueIdentifier
      prism:pageRange
      prism:coverDate
      prism:coverDisplayDate
      prism:doi
      dc:description
      citedby-count
      prism:aggregationType
      subtype
      subtypeDescription
      authkeywords
      article-number
      source-id
      fund-no
      openaccess
      openaccessFlag") %>%
    readr::read_lines() %>%
    purrr::map_chr(., .f = function(x) trimws(x))
}

#' @noRd
#' @importFrom readr read_lines
#' @importFrom purrr map_chr map pluck map2_df
#' @importFrom tibble as_tibble tibble
#' @importFrom tidyr unnest_wider
#' @importFrom dplyr select any_of
parse_scopus_entries <- function(xml) {

    afid <- NULL

    fields <- scopus_fields()

    mylist <-
      purrr::map(fields, function(x) purrr::map_chr(xml, x, .default = NA_character_))

    mypubs <-
      tibble::as_tibble(setNames(mylist, nm = fields))

    t1 <- xml %>% purrr::map(function(x) tibble::tibble(sid = purrr::pluck(x, "dc:identifier")))

    t2 <-
      purrr::map(xml %>% purrr::map(function(x) tibble::tibble(sid = pluck(x, "author"))),
        function(x) tidyr::unnest_wider(x, "sid"))

    myaut <-
      purrr::map2_df(t1, t2, function(x, y) tibble::tibble(x, y)) %>%
      tidyr::unnest(afid) %>% tidyr::unnest(afid) %>% tidyr::unnest(afid) %>%
      dplyr::filter(afid != "true")

    t2 <-
      purrr::map(xml %>% purrr::map(function(x) tibble::tibble(sid = purrr::pluck(x, "affiliation"))),
        function(x) tidyr::unnest_wider(x, "sid"))

    myaff <- purrr::map2_df(t1, t2, function(x, y) tibble::tibble(x, y))

    list(publications = mypubs, authors = myaut, affiliations = myaff)

}

#' @importFrom dplyr mutate
#' @importFrom tibble tibble
#' @noRd
pager <- function(hits, page_length = 25) {
  n <- hits
  n_pages_full <- ifelse(n %% page_length == 0, n %/% page_length, n %/% page_length + 1)
  from <- seq.int(0, n_pages_full * page_length - 1, by = page_length)
  to <- cumsum(rep(page_length, n_pages_full)) - 1
  to[length(to)] <- n - 1
  tibble(start = from, end = to) %>% dplyr::mutate(count = to - from + 1)
}

#' Authors affiliated with KTH for a default search request
#' @importFrom dplyr left_join filter rename
#' @export
scopus_search_aut_kth <- function() {

  affilname <- `dc:identifier` <- NULL

  ss <- scopus_search_pubs_kth()

  ss$authors %>%
    dplyr::left_join(ss$affiliations, by = c("afid", "sid")) %>%
    dplyr::filter(stringr::str_starts(affilname, "The Royal Ins")) %>%
    dplyr::left_join(ss$publications %>%
    dplyr::rename(sid = `dc:identifier`), by = c("sid"))
}

#' Upload Scopus API search results to S3/minio bucket
#' @export
#' @importFrom purrr map2 map
#' @importFrom readr write_csv
scopus_upload <- function() {

  s <- scopus_search_pubs_kth()

  message("Uploading Scopus data to S3 bucket")

  filez <- file.path(tempdir(), sprintf("scopus-%s.csv",
    c("publications", "affiliations", "authors"))
  )

  on.exit(unlink(filez))

  write <- purrr::map2(s, filez, function(x, y) readr::write_csv(x, file = y))
  upload <- purrr::map(filez, function(x) diva_upload_s3(x))

  return(invisible(TRUE))
}

#' Scopus API ratelimit quota information
#'
#' Makes a request and displays the headers containing rate limit information
#'
#'     X-RateLimit-Limit       <----Shows API quota setting
#'     X-RateLimit-Remaining   <----Shows API remaining quota
#'     X-RateLimit-Reset       1234567891 <----Date/Time in Epoch seconds when API quota resets
#'
#' @return a list with the information from the headers
#' @export
#' @importFrom glue glue
#' @importFrom httr headers status_code
scopus_ratelimit_quota <- function() {

  # make request
  beg_loaddate <- (Sys.Date() - 14) %>% format_date()
  beg_pubyear <- 2019L
  id_affiliation <- 60002014L

  criteria <- glue::glue(
    'AFFIL(((kth*) OR (roy* AND inst* AND tech*) OR ("Roy. Inst. T") OR ',
    '(alfven) OR (kung* AND tek* AND hog*) OR (kung* AND tek* AND h\\u00f6g*) OR ',
    '(kgl AND tek* AND hog*) OR (kung* AND tek* AND hg*) OR ',
    '(roy* AND tech* AND univ*)) AND (Sweden)) OR ',
    'AF-ID("The Royal Institute of Technology KTH" {id_affiliation}) AND ',
    'orig-load-date aft {beg_loaddate} AND pubyear aft {beg_pubyear}'
  )

  resp <- scopus_req(criteria, 1, 1)

  # Read the RateLimit headers

  hh <- httr::headers(resp)
  rl_total <- as.integer(hh$`x-ratelimit-limit`)
  rl_remaining <- as.integer(hh$`x-ratelimit-remaining`)
  rl_reset_ts <- as.integer(hh$`x-ratelimit-reset`)

  msg <-
    sprintf(
      "API rate limit info: [%s]\n%s\n%s",
      paste0("API request HTTP status: ", httr::status_code(resp)),
      paste0("Rate Limit quota setting: ", rl_total),
      paste0("Remaining RL quota: ", rl_remaining, " which resets at ", rl_reset_ts)
    )

  message(msg)
  message("Reset in UTC is: ", as.POSIXct(rl_reset_ts, tz = "UTC", origin = "1970-01-01"))
  message("  Now in UTC is: ", as.POSIXct(Sys.time(), tz = "UTC"))

  list(
    `X-RateLimit-Limit` = rl_total,
    `X-RateLimit-Remaining` = rl_remaining,
    `X-RateLimit-Reset` = rl_reset_ts
  )
}
