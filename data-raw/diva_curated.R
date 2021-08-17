library(jqr)
library(jsonlite)
library(here)
library(rcrypt)

# this script embeds an non-encrypted datafile in the package
# under "inst/extdata", using bzip2 compression

diva <-
  file.path(here(), "data-raw", "dontshare", "kth-exluding-theses-all-level2-2012-2019-corrected_pubs_S2.JSON")

file.exists(diva)

ndjson::validate(diva)

# store the file in the extdata directory

edfile <-
  file.path(here(), "inst", "extdata", "diva.json.bzip2")

if (file.exists(edfile)) {
  message("Replacing old version of this file")
  unlink(edfile)
}


R.utils::bzip2(filename = diva, destname = edfile, remove = FALSE)

unlink(file.path(rappdirs::app_dir("kthcorpus")$config(), "diva.json"))
