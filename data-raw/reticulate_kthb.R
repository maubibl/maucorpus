library(reticulate)

#py_install("requests")
#py_install("ftfy")

py_bibliutils <- function() {

  repo <- "https://raw.githubusercontent.com/KTH-Library/bibliutils/main/"
  con1 <- url(sprintf("%s/bibapi.py", repo))
  on.exit(close(con1))
  writeLines(readLines(con1), "/tmp/bibapi.py")

  bibapi <- reticulate::import_from_path("bibapi_v2", path = "/tmp")
  #reticulate::import("ftfy")
  #biblib <- reticulate::source_python(sprintf("%s/bibformat.py", repo))

  con2 <- url(sprintf("%s/bibformat.py", repo))
  on.exit(close(con2))
  writeLines(readLines(con2), "/tmp/bibformat.py")

  bibformat <- reticulate::import_from_path("bibformat", path = "/tmp")
  list(bibapi = bibapi, bibformat = bibformat)

}

pyb <- py_bibliutils()

pyb$bibformat$fix_kthid("u2345678xxx", idtype = "unit")

# vectorizing

library(purrr)
library(dplyr)

myids <- c("u2345678xxx", "u2345678xxx")

fix_kthid <- function(id)
  pyb$bibformat$fix_kthid(id, idtype = "unit")

# vectorized
map_chr(myids, fix_kthid)

# vectorized with progress bar
fix_kthids <- function(ids) {

  pb <- progress::progress_bar$new(
    total = length(ids),
    format = " fixing kthids [:bar] :percent eta: :eta")

  fix_with_progress <- function(x) {
    pb$tick()
    fix_kthid(x)
  }

  ids %>% map_chr(fix_with_progress)
}

# call with 10k identifiers
fix_kthids(rep(myids, 5e3))

# attempt to parallelize with furrr

library(furrr)
plan(multisession, workers = 2)
#myids %>% future_map_chr(fix_kthid)
# oops! error with "attempt to apply non-function"?

options(future.globals.onReference = "error")
#myids %>% future_map_chr(fix_kthid)

# see https://future.futureverse.org/articles/future-4-non-exportable-objects.html#package-reticulate
# Error: Detected a non-exportable reference (‘externalptr’) in one of the globals (‘pyb’ of class ‘list’) used in the future expression

# Options seen ti be: a) do parallelization and vectorization on py side or b) on R side


# option B
fix_kthid2 <- function(x, type = c("any", "person", "unit")) {

  type <- match.arg(type)

  re <- switch(type,
    any = "u[12][a-z0-9]{6}",
    person = "u1[a-z0-9]{6}",
    unit = "u2[a-z0-9]{6}"
  )

  m <- regmatches(x, regexec(re, x))
  unlist(Map(function(x) ifelse(length(x) == 0, NA_character_, x), m))

}

# now parallelization and vectorization works with furrr
# (tested with Rscript reticulate_kthb.R)

library(tictoc)

message("Using multicore plan")

tic()
plan(multicore, workers = 10)
rep(myids, 5e3) %>%
  furrr::future_map_chr(function(x) fix_kthid2(x, type = "any")) %>%
  head()
toc()

message("Using sequental plan")

tic()
plan(sequential)
rep(myids, 5e3) %>%
  furrr::future_map_chr(function(x) fix_kthid2(x, type = "any")) %>%
  head()
toc()
