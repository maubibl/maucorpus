#' Checks from swepub processing of KTH publication records
#' This function calls an API at SwePub for KTH data in a specific time period
#' @return tibble with results
#' @param config the configuration to use, by default diva_config (w org "kth")
#' @param year_beg the beginning year, default 1900
#' @param year_end the ending year, default is the current year
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
  year_beg = 1900L,
  year_end = lubridate::year(Sys.time())
  ) {

  output_type <- mods_url <- repository_url <- publication_year <-
    value <- flag_type <- NULL

  org <- config$org

  url <- paste0(
      "https://bibliometri.swepub.kb.se/api/v1/process/",
      sprintf("%s/export?from=%s&to=%s&", org, year_beg, year_end),
      "validation_flags=DOI_invalid,ISBN_invalid,ISSN_invalid,ORCID_invalid"
    )

  tsv <-
    url %>%
    httr::GET(config = httr::add_headers("Accept" = "text/tab-separated-values")) %>%
    content(type = "text", encoding = "UTF-8")

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

  readr::read_tsv(
      tsv,
      skip = 1,
      show_col_types = FALSE,
      col_names = colz,
      locale = readr::locale(encoding = "UTF-8")
    ) %>%
    mutate(output_type = linkify(output_type)) %>%
    mutate(mods_url = linkify(mods_url)) %>%
    mutate(repository_url = linkify(repository_url)) %>%
    rowwise() %>%
    mutate(value = linkify(value, target = flag_type)) %>%
    arrange(desc(publication_year))

}

