library(jqr)
library(jsonlite)
library(here)
library(rcrypt)

# this script embeds an encrypted datafile in the package
# under "inst/extdata"

ap <-
    file.path(here(), "data-raw", "dontshare", "pubs-2012-2019_augmented_further.JSON")

ndjson::validate(ap)

phrase <-
  openssl::sha256(file("data-raw/dontshare/pubs-2012-2019_augmented_further.JSON")) %>%
  as.character()

Sys.setenv("DIVA_PASS" = phrase)

# store the file in the extdata directory

edfile <-
  file.path(here(), "inst", "extdata", "ap.rcrypt")

if (file.exists(edfile)) {
  message("Replacing old version of this encrypted file")
  unlink(edfile)
}


rcrypt::encrypt(ap, edfile, passphrase = phrase)

