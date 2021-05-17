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
#' @importFrom httr GET add_headers
#' @importFrom readr read_tsv
swepub_checks <- function(org = "kth", year_beg = 1900L, year_end = 2020) {

  tsv <- paste0(
    "https://bibliometri.swepub.kb.se/api/v1/process/",
    sprintf("%s/export?from=%s&to=%s&", org, year_beg, year_end),
    "validation_flags=DOI_invalid,ISBN_invalid,ISSN_invalid,ORCID_invalid") %>%
  httr::GET(config = httr::add_headers("Accept" = "text/tab-separated-values")) %>%
  content(type = "text")

  readr::read_tsv(tsv, skip = 1)
}

