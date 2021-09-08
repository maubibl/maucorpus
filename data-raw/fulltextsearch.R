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

myauthors <- kth_diva_authors()
mytmp <- tempdir()

hrplus <- hr_plus()

write_json(mypubs, file.path(mytmp, "pubs.json"))
write_json(myauthors, file.path(mytmp, "authors.json"))
write_json(hrplus, file.path(mytmp, "hrplus.json"))

meili_createindex <- function(index, idfield = NULL) {

  res <- POST("http://localhost:7700/indexes",
       config = add_headers(`X-Meili-API-Key` = "masterKey"),
       body = list(uid = index, primaryKey = idfield),
       encode = "json"
  )

  content(res)
}

meili_ingest <- function(index, jsonfile) {

  res <- POST(
    url = sprintf("http://localhost:7700/indexes/%s/documents", index),
    config = add_headers(`X-Meili-API-Key` = "masterKey"),
    body = upload_file(jsonfile)
  )
  content(res)
}

meili_status <- function(index, jobid) {

  res <-
    GET(
      url = sprintf("http://localhost:7700/indexes/%s/updates/%s",
                    index, jobid),
      config = add_headers(`X-Meili-API-Key` = "masterKey"))

  content(res)
}


# we create an index, send it json data and check the status until is "processed"

meili_createindex("pubs", "PID")
jobid <- meili_ingest("pubs", file.path(mytmp, "pubs.json"))$updateId
meili_status("pubs", jobid)$status

meili_createindex("authors")
jobid <- meili_ingest("authors", file.path(mytmp, "authors.json"))$updateId
meili_status("authors", jobid)$status

meili_createindex("hrplus")
jobid <- meili_ingest("hrplus", file.path(mytmp, "hrplus.json"))$updateId
meili_status("hrplus", jobid)$status


while (meili_status("hrplus", jobid)$status != "processed") {
  Sys.sleep(1)
  cat(".")
}

# check the web ui for the search index
browseURL("http://localhost:7700") # use masterKey

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
    url = sprintf("http://localhost:7700/indexes/%s/search", index),
    config = add_headers(`X-Meili-API-Key` = "masterKey"),
    body = b, encode = "json"
  )

  s <- content(res)

  # TODO: iterate over pages and return results as data frame?
  #purrr::map_df(s$hits, dplyr::as_tibble)

}

meili_search("authors", "Xuezhi")$hits %>% map_df(dplyr::as_tibble)
meili_search("hrplus", "Xuezhi")$hits %>% map_df(dplyr::as_tibble)
meili_search("pubs", "Wahl")$hits %>% map_df(dplyr::as_tibble)

# TODO....

# automate lookups for all names with missing kth identifiers?
checks <- kth_diva_checks()
mia <- checks$missing_kthid
mynames <- head(chartr(".,", "  ", unique(mia$name)))

# lookup also on semantic scholar?

