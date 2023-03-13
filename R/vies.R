#' Resolve vatNumber from European Union VIES REST API
#'
#' The VIES REST API, see <https://viesapi.eu/test-vies-api/>, allows for online
#' validation of European VAT numbers
#' @param x a string with a properly formatted European VAT number
#' @return tibble with results (such as address, organization name etc)
#' @examples
#' \dontrun{
#' if(interactive()){
#'  vat_resolve_vies("SE202100305401")
#'  }
#' }
#' @importFrom jsonlite fromJSON
#' @importFrom dplyr bind_rows select
vat_resolve_vies <- function(x) {

  vat <- vat_parse(x)

  stopifnot(vat$is_valid & vat$is_eu)

  res <- "https://ec.europa.eu/taxation_customs/vies/rest-api/ms/%s/vat/%s" |>
    sprintf(vat$country, vat$vat_number) |>
    jsonlite::fromJSON() |>
    dplyr::bind_rows() |>
    dplyr::select(any_of("vatNumber"), everything(), -starts_with("vies")) |>
    distinct()

  if (vat$is_swe) res$sweOrgNumber <- vat$se_org
  return (res)
}

#' Parses a European VAT number
#'
#' This function validates an European VAT number by checking
#' the length of the string and whether it begins with two
#' capital letters and is followed by 12 digits
#' @param x the VAT number
#' @return a list with slots for the segments
#' @examples
#' \dontrun{
#' if(interactive()){
#'  vat_parse("SE202100305401")
#'  }
#' }
#' @export
vat_parse <- function(x) {

  is_invalid <- nchar(x) != 14
  country <- gsub("(^[A-Z]{2}).*$", "\\1", x)
  is_eu <- country %in% vies_country_codes()
  is_swe <- country == "SE"
  vat_number <- gsub("^[A-Z]{2}(\\d{12})$", "\\1", x)
  se_org <- substring(vat_number, 1, 10)

  list(
    valid = !is_invalid,
    country = country,
    is_eu = is_eu,
    is_swe = is_swe,
    vat_number = vat_number,
    swe_org = se_org
  )

}

#' @importFrom jsonlite fromJSON
#' @noRd
vies_country_codes <- function(use_rest = FALSE) {

  codes <- "AT BE BG CY CZ DE DK EE EL ES EU FI FR
    HR HU IE IT LT LU LV MT NL PL PT RO SE SI SK XI"

  if (use_rest)
    codes <- jsonlite::fromJSON("https://ec.europa.eu/taxation_customs/vies/rest-api/countries?forRequester=true") |> tibble::as_tibble() |> pull("countryCode")

  codes |> strsplit(split = "\\s+") |> unlist()

}

#' Search Funding & tender opportunities
#'
#' The EU Single Electronic Data Interchange Area (SEDIA) offers a search API
#'
#' <https://ec.europa.eu/info/funding-tenders/opportunities/portal/screen/how-to-participate/participant-register-search>
#'
#' @param legal_name the legal name search string
#' @param country the country search string
#' @param reg_number the registration number search string
#' @param pic the Participant Identification Code (9 digits)
#' @param vat the VAT number
#' @param erasmus_code the Erasmus code
#' @return tibble with results
#' @examples
#' \dontrun{
#' if(interactive()){
#'  funders_eu_search(legal_name = "Kungliga Tekniska")
#'  funders_eu_search(vat = "SE202100305401")
#'  }
#' }
#' @export
#' @importFrom jsonlite fromJSON
#' @importFrom tibble as_tibble
funders_eu_search <- function(legal_name = "Kungliga Tekniska", country = NULL, reg_number = NULL, pic = NULL, vat = NULL, erasmus_code = NULL) {

  criteria <- list(
    legalName = legal_name,
    country = country,
    regNumber = reg_number,
    pic = pic,
    vat = vat,
    erasmusCode = erasmus_code)

  s <- sprintf("curl -s 'https://ec.europa.eu/info/funding-tenders/opportunities/api/organisation/search.json' -X POST  -H 'Accept: application/json, text/plain, */*' -H 'Content-Type: application/json' --data-raw '%s'", jsonlite::toJSON(criteria, null = "null", auto_unbox = T)) |> system(intern = T)

  jsonlite::fromJSON(s) |> tibble::as_tibble()


  # httr::with_config(
  #   httr::config(ssl_verifypeer = 0L),
  #   httr::POST(
  #     url = "https://ec.europa.eu/info/funding-tenders/opportunities/api/organisation/search.json",
  #     body = jsonlite::toJSON(criteria, null = "null", auto_unbox = T),
  #     httr::accept_json(),
  #     httr::add_headers("Content-Type" = "application/json"),
  #     httr::content_type_json()
  #   )
  # )

}


