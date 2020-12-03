library(jqr)
library(jsonlite)
library(here)
library(rcrypt)

# this script embeds an encrypted datafile in the package
# under "inst/extdata"

ap <-
    file.path(here(), "data-raw", "dontshare", "authors_pubs_2912-2919.json")

phrase <-
  openssl::sha256(file("data-raw/dontshare/authors_pubs_2912-2919.json")) %>%
  as.character()

Sys.setenv("DIVA_PASS" = phrase)

# store the file in the extdata directory

edfile <-
  file.path(here(), "inst", "extdata", "ap.rcrypt")

rcrypt::encrypt(ap, edfile, passphrase = phrase)

