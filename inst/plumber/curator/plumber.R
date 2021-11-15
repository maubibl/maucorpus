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

# #generate user database
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

hrp <- hr_plus()

hrp_extra <- function() {

  hr <-
    hrp %>%
    left_join(ss_employment_title, by = c("emp_code" = "id")) %>%
    mutate(is_uf_ta = is_uf_ta == "UF")

  hr %>% left_join(research_areas, by = c("scb_topic" = "id")) %>%
    rename(
      "scb_topic_swe" = "swe",
      "scb_topic_eng" = "eng",
      "scb_topic_level" = "level"
    )

}

hrp_current <-
  hrp_extra() %>%
  filter(emp_beg <= lubridate::today(), emp_end > lubridate::today()) %>%
  group_by(kthid) %>%
  summarize(
    across(c("emp_lastmod", "emp_end", "emp_beg"), max)
  ) %>%
  arrange(desc(emp_lastmod, emp_end, emp_beg)) %>%
  inner_join(hrp) %>%
  unique()

# is latest modification data reasonable?
hrp_summary <-
  hrp_extra() %>% collect %>% group_by(kthid) %>%
  summarize(
    elm = max(emp_lastmod),
    ebeg = min(emp_beg),
    eend = max(emp_end),
    duration = eend - ebeg,
    ttl = eend - Sys.Date(),
    n = length(kthid)
  ) %>%
  arrange(desc(n), desc(elm))

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

#* Current employees
#* @get /v1/hr/current
#* @response 400 Bad request
#* @tag HR
#* @serializer csv
function(res) {
  hrp_current
}


#* Employees summary
#* @get /v1/hr/summary
#* @response 400 Bad request
#* @tag HR
#* @serializer csv
function(res) {
  hrp_summary
}

#* @plumber
function(pr) {
  pr %>%
    pr_set_docs("swagger") %>%
    pr_set_api_spec(function(spec) {
      spec$components$securitySchemes$bearerAuth$type <- "http"
      spec$components$securitySchemes$bearerAuth$scheme <- "bearer"
      spec$components$securitySchemes$bearerAuth$bearerFormat <- "JWT"
      spec$security[[1]]$bearerAuth <- ""
      spec
    }) %>%
    pr_set_debug(TRUE)
}
