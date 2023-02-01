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
    value <- flag_type <- NULL

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
    url %>%
    httr::GET(config = httr::add_headers("Accept" = "text/tab-separated-values")) %>%
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
      repository_url\n") %>%
    sapply(trimws) %>%
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

  tsv %>%
    mutate(output_type = linkify(gsub("term/swepub", "term/swepub/output", output_type))) %>%
    mutate(PID = stringr::str_extract(repository_url, "[^-]\\d+$")) %>%
    mutate(PID = link_diva(PID, PID)) %>%
    mutate(mods_url = linkify(mods_url)) %>%
    mutate(repository_url = linkify(repository_url)) %>%
    rowwise() %>%
    mutate(value = linkify(value, target = flag_type)) %>%
    arrange(desc(publication_year)) %>%
    select(PID, mods_url, repository_url, everything())

}

