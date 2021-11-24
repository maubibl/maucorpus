library(xml2)
library(httr)

pubs <- kth_diva_pubs()
pid <- pubs$PID[1]

# functions to get DiVA MODS formatted records from the DiVA-portal
# and flat "CSVALL2" format

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



# retrieve lists with some of the enums/lookups used in DiVA MODS format

urls <-
  sprintf("https://wiki.epc.ub.uu.se/download/attachments/86082771/%s", c(
    "Funder.csv?version=4&modificationDate=1635168949009&api=v2",
    "H%C3%B6gskolepo%C3%A4ng.csv?version=1&modificationDate=1627030653157&api=v2",
    "Uppsatsniv%C3%A5.csv?version=2&modificationDate=1631543033423&api=v2",
    "Utf%C3%A4rdande%20l%C3%A4ros%C3%A4te.csv?version=1&modificationDate=1626958182874&api=v2"
  ))

library(httr)
library(purrr)

diva_lists <-
  urls %>%
  map(function(x) content(GET(x)))

id <- diva_lists[[4]] %>% filter(domain == "kth") %>% pull(organisation_id)
mods_url <- sprintf("https://kth.diva-portal.org/dice/mods?query=organisationId:%s", id)

library(xml2)

content(GET(mods_url))
