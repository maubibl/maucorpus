#' Checks from swepub processing of KTH publication records
#' This function calls an API at SwePub for KTH data in a specific time period
#' @return tibble with results
#' @param config the configuration to use, by default diva_config (w org "kth")
#' @param year_beg the beginning year, default is the ybeg value from diva_config
#' @param year_end the ending year, default is the yend value from diva_config
#' @examples
#' \dontrun{
#' if(interactive()){
#'  swepub_checks()
#'  }
#' }
#' @export
#' @importFrom httr GET add_headers content
#' @importFrom readr read_tsv locale
#' @importFrom dplyr mutate arrange
swepub_checks <- function(
  config = diva_config(),
  year_beg = diva_config()$ybeg,
  year_end = diva_config()$yend
  ) {

  output_type <- mods_url <- repository_url <- publication_year <-
    value <- flag_type <- record_id <- NULL

  org <- config$org


  url <- paste0(
      "https://bibliometri.swepub.kb.se/api/v1/process/",
      sprintf("%s/export?from=%s&to=%s", org, year_beg, year_end),
      "&enrichment_flags=DOI_enriched,ISSN_enriched,ORCID_enriched",
      "&validation_flags=DOI_invalid,ISBN_invalid,ISSN_invalid,ORCID_invalid",
      "&audit_flags=creator_count_check_invalid"
#      "&audit_flags=UKA_comprehensive_check_invalid,creator_count_check_invalid"
    )


  tsv <-
    url |>
    httr::GET(config = httr::add_headers("Accept" = "text/tab-separated-values")) |>
    content(type = "text", encoding = "UTF-8")

  # json <-
  #   url %>%
  #   httr::GET(config = httr::add_headers("content-type" = "application/json")) %>%
  #   content(type = "text", encoding = "UTF-8")
  #
  #
  # RcppSimdJson::fparse(json)


  # introduced due to API change - new response format omitting column headers
  # Mon 23 May 2022
  # curl -H "Accept: text/tab-separated-values" https://bibliometri.swepub.kb.se/api/v1/process/kth/export?from=1900&to=2022&validation_flags=DOI_invalid,ISBN_invalid,ISSN_invalid,ORCID_invalid" | head -1


  colz <-
    readr::read_lines(
      "record_id
      source
      publication_year
      publication_type
      output_type
      flag_class
      flag_type
      flag_code
      validation_rule
      value
      old_value
      new_value
      path
      mods_url
      repository_url\n") |>
    sapply(trimws) |>
    unname()

  tsv <- readr::read_tsv(
      tsv,
      skip = 1,
      show_col_types = FALSE,
      col_names = colz,
      locale = readr::locale(encoding = "UTF-8")
    )

  if (nrow(tsv) < 1)
    return(tsv)

  #https://swepub.kb.se/bib/swepub:oai:DiVA.org:kth-324493
  #https://swepub.kb.se/showrecord?q=onr%3a%22swepub%3aoai%3aDiVA.org%3akth-324493%22&n=1&d=swepub&noredirect=true

  tsv |>
    mutate(output_type = linkify(gsub("term/swepub", "term/swepub/output", output_type))) |>
    mutate(swepub_url = linkify(swepub_url(record_id), text = record_id)) |>
    #mutate(PID = pid_from_urn(repository_url)) |>
    #mutate(PID = link_diva(PID, PID)) |>
    mutate(mods_url = linkify(mods_url)) |>
    mutate(repository_url = linkify(repository_url)) |>
    rowwise() |>
    mutate(value = linkify(value, target = flag_type)) |>
    arrange(desc(publication_year)) |>
    select(swepub_url, mods_url, repository_url, !starts_with("record_id"))

}

swepub_url <- function(record_id) {

  "https://swepub.kb.se/showrecord?q=onr:\"swepub:%s\"&n=1&d=swepub&noredirect=true" |>
    sprintf(record_id) |>
    utils::URLencode()

}

pid_from_urn <- function(repository_url) {

  resolve_pid <- function(url)
   url |> httr::HEAD() |> getElement("url") |>
    utils::URLdecode() |>
    stringr::str_extract(pattern = "pid=(.*[:](\\d+))", group = 2)

  repository_url |> purrr::map_chr(resolve_pid, .progress = TRUE)

}

