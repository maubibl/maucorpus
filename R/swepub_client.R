#' Checks from swepub processing of KTH publication records
#' This function calls an API at SwePub for KTH data in a specific time period
#' @return tibble with results
#' @param org the organization, default: "kth"
#' @param year_beg the beginning year, default 1900
#' @param year_end the ending year, default 2020
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
swepub_checks <- function(org = "kth", year_beg = 1900L, year_end = 2022) {

  output_type <- mods_url <- repository_url <- publication_year <-
    value <- flag_type <- NULL

  tsv <- paste0(
    "https://bibliometri.swepub.kb.se/api/v1/process/",
    sprintf("%s/export?from=%s&to=%s&", org, year_beg, year_end),
    "validation_flags=DOI_invalid,ISBN_invalid,ISSN_invalid,ORCID_invalid") %>%
  httr::GET(config = httr::add_headers("Accept" = "text/tab-separated-values")) %>%
  content(type = "text", encoding = "UTF-8")

  readr::read_tsv(tsv, skip = 1, show_col_types = FALSE, locale = readr::locale(encoding = "UTF-8")) %>%
    mutate(output_type = linkify(output_type)) %>%
    mutate(mods_url = linkify(mods_url)) %>%
    mutate(repository_url = linkify(repository_url)) %>%
    rowwise() %>%
    mutate(value = linkify(value, target = flag_type)) %>%
    arrange(desc(publication_year))

}

