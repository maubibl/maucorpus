library(jqr)
library(jsonlite)
library(here)
library(rcrypt)

# this script embeds an encrypted datafile in the package
# under "inst/extdata"

# pubs-2012-2019_augmented_further_further_S2-20210108.JSON
# on Feb 8 -> authors-MA-RC.JSON instead

ap <-
    file.path(here(), "data-raw", "dontshare", "authors-MA-RC.JSON")

file.exists(ap)

ndjson::validate(ap)

phrase <-
  openssl::sha256(ap) %>%
  as.character()

#file.edit("~/.Renviron")

Sys.setenv("DIVA_PASS" = phrase)

# store the file in the extdata directory

edfile <-
  file.path(here(), "inst", "extdata", "ap.rcrypt")

if (file.exists(edfile)) {
  message("Replacing old version of this encrypted file")
  unlink(edfile)
}


rcrypt::encrypt(ap, edfile, passphrase = phrase)

unlink(file.path(rappdirs::app_dir("diva")$config(), "ap.json"))
