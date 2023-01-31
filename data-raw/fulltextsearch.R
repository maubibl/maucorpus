library(kthcorpus)
library(dplyr)
library(jsonlite)
library(httr)

# Using "Meili Search" as an alternative to ElasticSearch

# (see the docker-compose.yml-file and Meili Search (docker image))

# also:

# https://docs.meilisearch.com/reference/api/search.html#search-in-an-index-with-post-route
# https://github.com/shokme/meilisearch-dashboard
# https://codefibershq.com/blog/hands-on-meilisearch-a-next-generation-search-engine-for-modern-web#h6
# https://github.com/meilisearch/docs-scraper/tree/main/scraper/src

# export some data for an instant search web ui
mypubs <- kth_diva_pubs() %>% select(-Name)

# TODO: add username from KTH Profiles API? To get image url
# TODO: does wikidata have organisation images (log_image?)

myauthors <-
  kth_diva_authors() %>%
  distinct(name, kthid, orcid, orgid, extorg) %>%
  left_join(kth_diva_aliases() %>% filter(!grepl("^PI000|^-", kthid)), by = "kthid") %>%
  rename(fullname = name.x, orcid = orcid.x, n_aliases = n) %>%
  select(-c(name.y, orcid.y)) %>%
  unique() %>%
  arrange(desc(n_aliases), orcid, extorg) %>%
  mutate(rowid = 1:nrow(.)) %>%
  select(rowid, everything()) #%>%
#  mutate(image_url = paste0("https://www.kth.se/files/avatar/", username))

# TODO: affiliations within KTH? add org-identifiers...

extract_unit_abbr <- function(x) {
  unlist(lapply(strsplit(x, "/"), function(y) ifelse(length(y) > 0, toupper(rev(y)[[1]]), x)))
}

myorgs <-
  bibliomatrix::unit_info() %>%
    distinct(Diva_org_id, unit_short, unit_long_en, unit_long_sv, slug) %>%
    mutate(unit_abbr = extract_unit_abbr(slug)) %>%
    select(unit_abbr, orgid = Diva_org_id, desc = unit_long_en, d = unit_short) %>%
  union_all(
    bibliomatrix::abm_divisions() %>%
      mutate(unit_abbr = extract_unit_abbr(id)) %>%
    select(unit_abbr, desc)
  )


kth_diva_orgcombos <- function() {

  kdp <-
    kth_diva_pubs() %>%
    select(Year, PublicationDate, Name) %>%
    distinct() %>%
    filter(!is.na(Year), !is.na(PublicationDate), !is.na(Name))

  pb <- progress::progress_bar$new(
    format = "  crunching combos [:bar] :percent eta: :eta",
    total = length(kdp$Year), clear = FALSE, width= 73)

  unit_orgid_combos <- function(x, y) {
    pb$tick()
    kthcorpus::extract_affiliations(x) %>%
    filter(!is_extorg) %>%
    mutate(unit_descr = gsub("\\s?[[]\\d+[]]", "", aff)) %>%
    select(orgids = divaorgs, unit_descrs = unit_descr) %>%
    mutate(year = y) %>%
    distinct()
  }

  possible_combos <- purrr::possibly(unit_orgid_combos, data.frame())

  list(kdp$Name, kdp$Year) %>%
    purrr::pmap_dfr(possible_combos) %>%
    distinct() %>%
    mutate(orgids = gsub("\\s", ", ", orgids)) %>%
    select(year, orgids, unit_descrs)

}

kdc <- kth_diva_orgcombos()

# frequent affiliationstrings at KTH
kdc %>%
  filter(!grepl("NA", orgids)) %>%
  filter(!grepl("KTH$", unit_descrs)) %>%
  group_by(unit_descrs) %>%
  summarise(
    n_years = n_distinct(year),
    n_orgids = n_distinct(orgids),
    orgids = paste0(collapse = ";", unique(orgids)),
    years = paste0(collapse = ";", unique(sort(year)))
  ) %>%
  arrange(desc(n_orgids)) %>%
  View()

# frequent organization code combos and their affiliationstrings at KTH
affcombos <-
  kdc %>%
  filter(!grepl("NA", orgids)) %>%
  filter(!grepl("KTH$", unit_descrs)) %>%
  filter(!grepl(";", unit_descrs)) %>%
  mutate(
    n_commas_o = stringr::str_count(orgids, ","),
    n_commas_d = stringr::str_count(unit_descrs, ","),
    n_semis_d = stringr::str_count(unit_descrs, ";")
  ) %>%
  filter(n_commas_o == n_commas_d) %>%
  group_by(orgids) %>%
  summarise(
    descrs = paste0(collapse = "\n", unique(sort(unit_descrs))),
    n_descrs = n_distinct(unit_descrs),
    years = paste0(collapse = ";", unique(sort(year)))
  ) %>%
  arrange(desc(n_descrs)) %>%
  filter(
    grepl("^\\d{3}$", orgids) |
    grepl("^\\d{3}, \\d{4}$", orgids) |
    grepl("^\\d{3}, \\d{4}, \\d{5}$", orgids) |
    grepl("^\\d{3}, \\d{4}, \\d{5}, \\d{6}$", orgids, perl = TRUE)
    #grepl("^(\\d{3})?, (\\d{4})?, (\\d{5})?, (\\d{6})?$", orgids, perl = TRUE)
  ) %>%
  arrange(desc(orgids)) %>%
  mutate(combo_id = 1:nrow(affcombos)) %>%
  select(combo_id, everything())


affcombos %>% View()


diva_orgs <- function() {

  # Organisationer från Diva som, efter ommappning, har publikationer i Bibmet.
  # Vissa subdepartments står angivna som Closed, och har inte mappats om.
  # Det viktiga har varit school och department, inte subdep

  org_desc <-
    readr::read_csv(kthcorpus::minio_get("bibmet_diva_orgs_pubs.csv"),
                    show_col_types = FALSE, na = "NULL")

  l1 <-
    org_desc %>%
    select(orgid = Diva_school_id, desc = School_name) %>%
    mutate(level = "school") %>%
    distinct()

  l2 <-
    org_desc %>%
    select(orgid = Diva_dep_id, desc = Dep_Name) %>%
    mutate(level = "dep") %>%
    distinct() %>%
    filter(!is.na(orgid), !is.na(desc))

  l3 <-
    org_desc %>%
    select(orgid = Diva_sub_dep_id, desc = Sub_dep_name) %>%
    mutate(level = "subdep") %>%
    mutate(is_closed = grepl("[(][Cc]losed\\s+\\d+[)]", desc)) %>%
    mutate(close_date = ifelse(is_closed == TRUE, gsub(".*?[(][Cc]losed\\s+(\\d+)[)].*", "\\1", desc, perl = TRUE), NA_character_)) %>%
    mutate(desc = gsub("[(][Cc]losed\\s+\\d+[)]", "", desc)) %>%
    distinct() %>%
    filter(!is.na(orgid), !is.na(desc))

  bind_rows(l1, l2, l3) %>%
    mutate(is_closed = ifelse(is.na(is_closed), FALSE, is_closed)) %>%
    mutate(close_date = lubridate::ymd(close_date)) %>%
    mutate(desc = trimws(desc))
}


# TODO: how match desc strings here to get slugs?
diva_orgs() %>% left_join(myorgs)

do <- diva_orgs()

do %>% filter(is_closed)

orgz <- affcombos

write_rds(orgz, "orgz.rds")




mytmp <- tempdir()

hrplus <- hr_plus()

write_json(mypubs, file.path(mytmp, "pubs.json"))
write_json(myauthors, file.path(mytmp, "authors.json"))
write_json(hrplus, file.path(mytmp, "hrplus.json"))
write_json(do, file.path(mytmp, "orgs.json"))
write_json(affcombos, file.path(mytmp, "affs.json"))

wc <- function(data, filename) {
  write_csv(data, file.path(mytmp, filename), na = "", quote = "all", escape = "double")
}

wc(mypubs, "pubs.csv")
wc(myauthors, "authors.csv")
wc(hrplus, "hrplus.csv")
wc(do, "orgs.csv")
wc(affcombos, "affs.csv")

library(readxl)

gvs <-
  read_xlsx("data-raw/Externfinansierade projekt 2021.xlsx", sheet = "GVS") %>%
  select(-c("T", "Ver.nr", "Ver.datum", "Period", "Konto", "Konto (T)", "Resk.nr")) %>%
  rename(
    projectname = Projektnamn,
    unit = Dim1,
    unit2 = Dim2,
    projectdesc = Text,
    amount = Belopp,
    funder = Finansiär
  ) %>%
  mutate(rowid = 1:nrow(.)) %>%
  select(rowid, everything())

write_json(gvs, file.path(mytmp, "gvs.json"))
wc(gvs, "gvs.csv")

#file.edit("~/.Renviron")
#readRenviron("~/.Renviron")
meili_base <- "https://search.bibliometrics.lib.kth.se"
#meili_cfg <- add_headers(`X-Meili-API-Key` = Sys.getenv("MEILI_KEY"))
meili_cfg <- add_headers(`Authorization` =  paste("Bearer", Sys.getenv("MEILI_KEY")))

#meili_base <- "http://localhost:7700"
#meili_cfg <- add_headers(`X-Meili-API-Key` = "masterKey")

meili_createindex <- function(index, idfield = NULL) {

  res <- POST(paste0(meili_base, "/indexes"),
       config = meili_cfg,
       body = list(uid = index, primaryKey = idfield),
       encode = "json"
  )

  content(res)
}

meili_ingest <- function(index, jsonfile) {

  res <- POST(
    url = sprintf(paste0(meili_base, "/indexes/%s/documents"), index),
    config = meili_cfg,
    body = upload_file(jsonfile)
  )
  content(res)
}

meili_ingest_csv <- function(index, csvfile) {
    res <- POST(
    url = sprintf(paste0(meili_base, "/indexes/%s/documents"), index),
    config = meili_cfg,
    content_type("text/csv"),
    body = upload_file(csvfile)
  )
  content(res)
}

meili_status <- function(index, jobid) {

  res <-
    GET(
      url = sprintf(paste0(meili_base, "/indexes/%s/updates/%s"),
                    index, jobid),
      config = meili_cfg)

  content(res)
}

meili_tasks <- function() {

  res <-
    GET(
      url = sprintf(paste0(meili_base, "/tasks")),
      config = meili_cfg)

  res <- content(res)

  purrr::map(res$results, as.data.frame) %>%
    purrr::map_df(bind_rows) %>%
    tibble::as_tibble()

}

meili_settings <- function(index) {

    content(GET(
      url = sprintf(paste0(meili_base, "/indexes/%s/settings"),
                    index),
      config = meili_cfg))


}

meili_document <- function(index, id) {
      content(GET(
      url = sprintf(paste0(meili_base, "/indexes/%s/documents/%s"),
                    index, id),
      config = meili_cfg))
}

# we create an index, send it json data and check the status until is "processed"

meili_createindex("pubs", "PID")
jobid <- meili_ingest_csv("pubs", file.path(mytmp, "pubs.json"))$taskUid

meili_createindex("authors", idfield = "rowid")
meili_tasks()
#jobid <- meili_ingest("authors", file.path(mytmp, "authors.json"))$taskUid
jobid <- meili_ingest_csv("authors", file.path(mytmp, "authors.csv"))$taskUid

#meili_createindex("hrplus")
jobid <- meili_ingest_csv("hrplus", file.path(mytmp, "hrplus.csv"))$taskUid

meili_ingest_csv("orgs", file.path(mytmp, "orgs.csv"))$taskUid

meili_ingest_csv("affs", file.path(mytmp, "affs.csv"))

meili_tasks()
meili_ingest_csv("gvs", file.path(mytmp, "gvs.csv"))


# while (meili_status("hrplus", jobid)$status != "processed") {
#   Sys.sleep(1)
#   cat(".")
# }

# check the web ui for the search index
browseURL(meili_base) # use masterKey


# Let us see if we can find some authors missing kthids...
"https://bibliometrics.lib.kth.se/qc/v1/check/report#missing-kthid" %>%
  browseURL()



# or use the search functionality from meili
meili_search <- function(
  index, query = "", offset = 0L, limit = 20L,
  filter = NULL,
  facetsDistribution = NULL,
  attributesToRetrieve = "*",
  attributesToCrop = NULL,
  cropLength = 200L,
  attributesToHighlight = NULL,
  matches = TRUE) {

  b <- list(q = query, offset = offset, limit = limit, filter = filter,
            facetsDistribution = facetsDistribution,
            attributesToRetrieve = list(attributesToRetrieve),
            attributesToCrop = attributesToCrop, cropLength = cropLength,
            attributesToHighlight = attributesToHighlight, matches = matches)

  res <- POST(
    url = sprintf("%s/indexes/%s/search", meili_base, index),
    config = meili_cfg,
    body = b, encode = "json"
  )

  s <- content(res)

  s

  # TODO: iterate over pages and return results as data frame?
  #purrr::map_df(s$hits, dplyr::as_tibble)

}

library(purrr)

meili_search("authors", "Maguire")

meili_search("authors", "Xuezhi")$hits %>% map_df(dplyr::as_tibble)
meili_search("hrplus", "Xuezhi")$hits %>% map_df(dplyr::as_tibble)
meili_search("pubs", "Wahl")$hits %>% map_df(dplyr::as_tibble)

# TODO....

# automate lookups for all names with missing kth identifiers?
checks <- kth_diva_checks()
mia <- checks$missing_kthid
mynames <- head(chartr(".,", "  ", unique(mia$name)))

search_names <- function(x) {

  s <- function(x)
    meili_search("hrplus", x)$hits %>% map_df(dplyr::as_tibble)

  x %>% map_df(s)

}

search_names(mynames)


kdc <- kth_diva_curated()

library(tidyr)

kdc %>% select(c("PID", starts_with("S2"))) %>% slice(1:10) %>%
  pivot_longer(
    cols = matches("S2_authors\\.\\d+\\.name"),
    #names_to = c("s2an"),
    #names_pattern = "S2_authors\\.\\d+\\.name",
    values_to = "s2an"
  ) %>%
  select(PID, s2an) %>%
  filter(!is.na(s2an))

kdc %>% select(c("PID", starts_with("S2"))) %>% slice(1:10) %>%
  mutate(across(.cols = matches("S2_authors\\.\\d+\\.id"), .fns = as.integer)) %>%
  pivot_longer(
    cols = matches("S2_authors\\.\\d+\\.id"),
    #names_to = c("s2an"),
    #names_pattern = "S2_authors\\.\\d+\\.name",
    values_to = "s2id"
  ) %>%
  select(PID, s2id) %>%
  filter(!is.na(s2id))



# slug versus hr$unit_abbr

researchers_all <- readRDS("~/researchers.rds")

hrp <- hr_plus()

# mismatches (HR unit_abbr versus slug)

researchers_all %>% inner_join(hrp) %>%
  select(kthid, slug, unit_abbr, unit_name) %>%
  mutate(slug_3 = toupper(strsplit(slug, "/") %>% map_chr(function(x) ifelse(nchar(x[[3]]) > 0, x[[3]], NA_character_)))) %>%
#  View()
  filter(unit_abbr != slug_3) %>%
  View()
