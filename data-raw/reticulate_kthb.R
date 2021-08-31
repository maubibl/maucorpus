library(reticulate)

#py_install("requests")
#py_install("ftfy")

py_bibliutils <- function() {

  repo <- "https://raw.githubusercontent.com/KTH-Library/bibliutils/main/"
  con1 <- url(sprintf("%s/bibapi.py", repo))
  on.exit(close(con1))
  writeLines(readLines(con1), "/tmp/bibapi_v2.py")

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
