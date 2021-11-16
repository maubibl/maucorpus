library(xml2)
library(httr)

pubs <- kth_diva_pubs()
pid <- pubs$PID[1]

mods_diva <- function(pid) {

  mods_url <- function(x) sprintf(paste0(
    "https://kth.diva-portal.org/smash/references",
    "?referenceFormat=MODS&pids=[diva2:%s]&fileName=export.xml"
  ), x)

  httr::content(httr::GET(mods_url(pid))) %>% xml2::as_list()
}

csvall2_diva <- function(pid) {

  csv_url <- function(x) sprintf(paste0(
    "https://kth.diva-portal.org/smash/references",
    "?referenceFormat=CSVALL2&pids=[diva2:%s]&fileName=export.csv"
  ), x)

  httr::content(httr::GET(csv_url(pid)), show_col_types = FALSE)

}

#mods_diva(pid) %>% as_xml_document() %>% xml_structure()
#csvall2_diva(pid)
