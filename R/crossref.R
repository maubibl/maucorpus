read_funder <- function(x) {
  res <- purrr::pluck(x, "message", "items") |>
    tibble::enframe(name = NULL, value = "cr") |>
    tidyr::unnest_wider(simplify = TRUE, col = c("cr"))
  if (!"alt-names" %in% names(res)) return (res)
  res |> tidyr::unnest_wider(col = "alt-names", names_sep = "_")
}

#' Resolve funding organizations from Crossref Funding Registry from a text string
#'
#' The CrossRef Open Funder Registry contains more than 30 000 funder names with DOI URIs
#' @param x a string vector of strings to lookup
#' @return tibble with results
#' @importFrom jsonlite read_json
#' @examples
#' \dontrun{
#' if(interactive()){
#'   cr_funders_resolve(c("rymdstyrelsen", "volvo", "saab"))
#'  }
#' }
#' @export
#' @importFrom tibble enframe
#' @importFrom tidyr unnest_wider
#' @importFrom jsonlite read_json
#' @importFrom utils URLencode
#' @importFrom purrr possibly map map_dfr
#' @import dplyr
cr_funders_resolve <- function(x) {

  x <- vat$name |> head(50)

  y <- x |> unique() |> na.omit()
  param <- utils::URLencode(y, reserved = TRUE)

  urls <- "https://api.crossref.org/funders?query=%s" |>
    sprintf(param)

  reqs <-
    urls |>
    purrr::map(purrr::possibly(jsonlite::read_json), .progress = list(
      show_after = 0.2,
      name = "crossref",
      type = "iterator",
      format = "Fetching {cli::pb_bar} {cli::pb_percent}",
      clear = FALSE
    )) |>
    purrr::map_dfr(read_funder, .id = "seq",
      .progress = list(
        show_after = 0.2,
        name = "crossref",
        type = "iterator",
        format = "Resolving {cli::pb_bar} {cli::pb_percent}",
        clear = FALSE
      )
    )

  res <-
    tibble::tibble(lookup = y, seq = as.character(1:length(y))) |>
    dplyr::left_join(reqs, by = "seq", multiple = "all") |>
    dplyr::select(-any_of("tokens"))

  if (!"id" %in% names(res)) return(data.frame())
  return (res)
}

cr_GET <- function(route = "https://api.crossref.org/funders", filter, rows = 1000, offset = 0, timeout = 30L) {

  # https://api.crossref.org/swagger-ui/index.html#/Funders/get_funders
  ua <- "httr (https://github.com/KTH-Library/kthcorpus/; mailto:biblioteket@kth.se)"

  resp <- httr::GET(url = route,
    query = compact(list(
      filter = filter,
      rows = rows,
      offset = offset
    )),
    httr::add_headers("Content-Type" = "application/json"),
    httr::user_agent(ua),
    httr::timeout(timeout)
  )

  httr::stop_for_status(resp)

  if (httr::http_type(resp) != "application/json") {
    stop(sprintf("API returned %s", httr::http_type(resp)), call. = FALSE)
  }

  if (httr::status_code(resp) != 200) {
    hh <- httr::headers(resp)
    rl_total <- as.integer(hh$`X-Rate-Limit-Limit`)
    rl_interval <- as.integer(hh$`X-Rate-Limit-Interval`)

    stop(
      sprintf(
        "API request failed [%s]\n%s\n<%s>",
        httr::status_code(resp),
        paste0("Rate Limit quota setting:", rl_total),
        paste0("Rate Limit interval: ", rl_interval)
      ),
      call. = FALSE
    )
  }

  return (resp)
}

#' Retrieve Funders from Crossref Funder Registry
#'
#' This function crawls the Crossref Funder Registry for entries in a specific location.
#' @param location the geographical location, Default: 'Sweden'
#' @return tibble with results
#' @examples
#' \dontrun{
#' if(interactive()){
#'  cr_funders()
#'  }
#' }
#' @export
#' @importFrom glue glue
#' @importFrom httr content
#' @importFrom purrr pmap map map_dfr
#' @importFrom dplyr select
cr_funders <- function(location = "Sweden") {

  location <- URLencode(glue::glue("location:{location}"))
  p0 <- cr_GET(filter = location, rows = 1, offset = 0) |> httr::content()

  n <- p0$message$`total-results`
  p <- pager(n, page_length = 100)
  message(glue::glue("Found {n} results, fetchning {nrow(p)} pages..."))

  resp <- list(offset = p$start, rows = p$count) |> purrr::pmap(
    .f = function(rows, offset) cr_GET(filter = location, rows = rows, offset = offset),
    .progress = list(
      show_after = 0.2,
      name = "cr_funders",
      type = "iterator",
      format = "Fetching {cli::pb_bar} {cli::pb_percent}",
      clear = FALSE
    )
  )

  res <- resp |> purrr::map(httr::content) |>
    purrr::map_dfr(read_funder, .id = "seq",
       .progress = list(
         show_after = 0.01,
         name = "crossref",
         type = "iterator",
         format = "Merging {cli::pb_bar} {cli::pb_percent}",
         clear = FALSE
       )
    )

  res |> dplyr::select(-any_of(c("tokens", "seq"))) |>
    dplyr::select(any_of(c("id", "name", "uri")), starts_with("replace")) |>
    unnest_wider(col = "replaced-by", names_sep = "_") |>
    unnest_wider(col = "replaces", names_sep = "_")

}
