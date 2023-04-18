libraries <- function(libz) {

    pkgs <- readLines(con = textConnection(trimws(libz)))

    # install missing packages
    install.packages(setdiff(pkgs, unname(installed.packages()[, c(1)])))

    # load installed packages
    import <- function(x) do.call(library,
      list(package = x, character.only = TRUE, logical.return = TRUE)
    )

    libs <- Map(import, pkgs)

}

libraries("
httr
dplyr
bslib
emoji
")

# these functions could be moved into the bibliotools library

curator_api_cfg <- function() {
    # REMEMBER: add API credentials to .Renviron, using:
    # file.edit("~/.Renviron")
    # readRenviron("~/.Renviron")
    list(
        baseurl = "http://127.0.0.1:8000",
        user = "divaapan",
        pass = "secret",
        token = NULL
    )
}

curator_api_token <- function(cfg = curator_api_cfg()) {

    request_body <- list(user = cfg$user, password = cfg$pass)

    token_req <- httr::POST(
        paste0(cfg$baseurl, "/authentication"),
        httr::add_headers("accept" = "*/*"),
        body = request_body, encode = "json"
    )

    if (httr::status_code(token_req) == 200)
      return (httr::content(token_req)[[1]])

    return (NULL)

}

curator_api_status <- function(cfg = curator_api_cfg()) {

  status_docs <- httr::status_code(httr::GET(
    paste0(cfg$baseurl, "/__docs__/")
  ))

  has_token <- !is.null(cfg$token)

  list(
    is_online = status_docs == 200,
    is_authed = has_token
  )
}

curator_api_search <- function(term, cfg) {

    stopifnot(!is.null(cfg$token))

    my_url <- sprintf(
        "%s/v1/hr/search?search=%s",
        cfg$baseurl, term
    )

    hits <- httr::GET(my_url, httr::add_headers(
        "accept" = "text/csv",
        Authorization = paste("Bearer", cfg$token, sep = " "))
    )

    httr::stop_for_status(hits)

    httr::content(hits, as = "text") |>
      readr::read_csv(show_col_types = FALSE)
}

cfg <- curator_api_cfg()
cfg$token <- curator_api_token(cfg)
status <- curator_api_status(cfg)

match_near <- function(term, n_rows) {

  if (!status$is_online)
    stop("API is not online")

  if (!status$is_authed)
    stop("Not authenticated")

  curator_api_search(term = utils::URLencode(term), cfg = cfg) |>
    tibble::as_tibble() |>
    head(n_rows)

}
