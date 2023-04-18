library(kthcorpus)
library(plumber)
library(dplyr)
library(purrr)
library(htmlwidgets)
library(readr)
library(jsonlite)
#devtools::install_github("jandix/sealr")
library(sealr)
library(httr)
library(jose)
library(here)

#* @apiTitle Curator at KTH
#* @apiDescription KTH Corpus API for Curators.
#* Lightweight programmatic access for curating KTH corpus data.
#* @apiContact list(name = "API Support", url = "https://KTH-library.github.io", email = "biblioteket@kth.se")
#* @apiLicense list(name = "Apache 2.0", url = "https://www.apache.org/licenses/LICENSE-2.0.html")
#* @apiTag Corpus KTH Corpus Curator API
#* @apiTag Auth Bearer-token required
#* @apiVersion 0.1

#generate user database
# users <- bind_rows(
#   tibble(
#     id = 1, user = "divaapan",
#     password = bcrypt::hashpw("secret")
#   ),
#   tibble(
#     id = 2, user = "integration",
#     password = bcrypt::hashpw("secret")
#   )
# )
#
# saveRDS(users, "inst/plumber/curator/userdb.rda")


secret <- Sys.getenv("PLUMBER_API_SECRET")

if (secret == "") {
  cat("Please set environment variable for PLUMBER_SECRET_API")
  secret <- "3ec9aaf4a744f833e98c954365892583"
}

if (!file.exists("userdb.rda")) {
  cat("No userdb.rda found at ", here("."))
} else {
  users <- readRDS("userdb.rda")
}

#* Filter all requests to the API for auth, except requests for docs
#* @filter sealr-jwt
function (req, res) {
  if (grepl("docs|swagger|openapi", tolower(req$PATH_INFO))) {
    forward()
    return(res)
  }

  sealr::authenticate(
    req = req, res = res, is_authed_fun = sealr::is_authed_jwt,
    token_location = "header", secret = secret
  )
}

error_msg <- function(status, code, msg)
  list(status = status, code = code, message = msg)

#* Authentication - provide user/pass and get a token
#* @post /authentication
#* @preempt sealr-jwt
#* @tag Auth
function(req, res, user = NULL, password = NULL) {


  if (is.null(user) || is.null(password)) {
    res$status <- 404
    return (error_msg("Failed", 404, "Incorrect user or password"))
  }

  index <- match(user, users$user)

  if (is.na(index) || !bcrypt::checkpw(password, users$password[index])) {
    res$status <- 401
    return (error_msg("Failed", 401, "Incorrect user or password"))
  }

  # jwt payload; information about the additional fields at
  # https://tools.ietf.org/html/rfc7519#section-4.1
  payload <- jose::jwt_claim(
    userID = users$id[index],
    # NumericDate - seconds since the epoch (token expires in 30 days)
    exp = as.numeric(lubridate::as_datetime(lubridate::today() + 30))
  )

  secret_raw <- charToRaw(secret)
  jwt <- jose::jwt_encode_hmac(payload, secret = secret_raw)
  return(jwt = jwt)
}

library(RSQLite)
library(DBI)

con_search <- function(db_loc = "search.db") {
  DBI::dbConnect(RSQLite::SQLite(), db_loc)
}

if (!file.exists("search.db")) {
  cat("No search.db found at ... attempting download")
  sprintf("mc cp kthb/kthcorpus/search.db .") |> system()
}

if (!file.exists("search.db"))
  stop("No file found, download failed...")

con <- con_search()
lookup <- DBI::dbReadTable(con, "lookup")
colz <- paste0(collapse = ", ", colnames(lookup))

# for a set of search terms we generate the sql for an FTS search query
# and make sure to include the score
fts_query <- function(search_term) {
  terms <- paste0(collapse = " ", search_term)
  paste(
    sprintf("select bm25(fts) as score, %s from fts", colz),
    sprintf("where fts match 'NEAR(%s)' order by bm25(fts);", terms))
}

# fcn to execute a search against the database and return a tibble
match_near <- function(search_term, con = con_search()) {

  terms <- strsplit(split = "\\s+", search_term) |> unlist()
  term <- paste0(collapse = " ", paste0("\"", terms, "\""))

  hits <- con |> DBI::dbGetQuery(fts_query(term)) |> as_tibble() |>
    mutate(score = abs(score)) |>
    mutate(search = search_term) |>
    mutate(term = search_term) |>
    select(term, any_of(colnames(lookup)), score) |>
    mutate(across(-c("score"), as.character)) |>
    mutate(across("score", as.double)) |>
    collect()

  do <- con |> tbl("do") |> mutate(orgid = as.character(orgid)) |> collect()

  hits |> left_join(do, by = "orgid")
}

#* Current employees
#* @get /v1/hr/current
#* @response 400 Bad request
#* @tag HR
#* @serializer csv
function(res) {
  con |> tbl("hr_current") |> collect()
}


#* Employees summary
#* @get /v1/hr/summary
#* @response 400 Bad request
#* @tag HR
#* @serializer csv
function(res) {
  con |> tbl("hr_summary") |> collect()
}

#* Search employees
#* @get /v1/hr/search
#* @response 400 Bad request
#* @tag HR
#* @param search The search string with a term to search for
#* @serializer csv
function(res, search = "") {
  match_near(search_term = search)
}

#* @plumber
function(pr) {
  mypr <- pr %>%
    pr_set_docs("swagger") %>%
    pr_set_api_spec(function(spec) {
      spec$components$securitySchemes$bearerAuth$type <- "http"
      spec$components$securitySchemes$bearerAuth$scheme <- "bearer"
      spec$components$securitySchemes$bearerAuth$bearerFormat <- "JWT"
      spec$security[[1]]$bearerAuth <- ""
      spec
    }) %>%
    pr_set_debug(TRUE)

  mypr$registerHooks(list(
    "exit" = function() {
      cat("Cleaning up database connection to search.db!")
      message("Bye!")
      DBI::dbDisconnect(con)
    }))
}

options_plumber(port = 8000L)
