library(kthcorpus)
library(plumber)
library(dplyr)
library(purrr)
library(htmlwidgets)
library(readr)

#* @apiTitle Data from KTH DiVA portal
#* @apiDescription DiVA data from the KTH DiVA portal.
#* Lightweight programmatic access to paper and author data.
#* @apiContact list(name = "API Support", url = "https://KTH-library.github.io", email = "biblioteket@kth.se")
#* @apiLicense list(name = "Apache 2.0", url = "https://www.apache.org/licenses/LICENSE-2.0.html")
#* @apiTag Publication Functionality related to retrieving publication data
#* @apiTag Author Functionality related to retrieving author data
#* @apiTag Issues Potential data quality issues related to author data
#* @apiTag Check A check performed on publication or author data
#* @apiVersion 0.1

pubs <- kth_diva_pubs()

#* Data for a publication given a publication identifier (DiVA PID or paper identifer)
#* @get /v1/paper/<PaperId>
#* @response 400 Invalid input.
#* @param PaperId:string A publication identifier; examples for various identifiers: DiVA: PID:1242492, DOI: DOI:10.1109/ECOC.2017.8345960, PubMed ID: PMID:19872477, UT/ISI: ISI:000208837900085
#* @tag Publication
function(PaperId) {

  id <- trimws(URLdecode(PaperId))
  cat("Got PaperId:", PaperId, "\n")

  re <- function(prefix, x)
    sapply(regmatches(x, regexec(sprintf("%s:(.*)", prefix), x)), "[", 2)

  pid <- re("PID", id)
  pmid <- re("PMID", id)
  doi <- re("DOI", id)
  isi <- re("ISI", id)

  res <- data.frame()

  cat("Identifiers: ", pid, pmid, doi, isi)

  # use fts here! with column:-syntax
  if (!is.na(pid) && nchar(pid) > 0)
    res <- pubs %>% filter(PID == pid) %>% collect()

  if (!is.na(doi) && nchar(doi) > 0)
    res <- pubs %>% filter(DOI == doi) %>% collect()

  if (!is.na(pmid) && nchar(pmid) > 0)
    res <- pubs %>% filter(PMID == pmid) %>% collect()

  if (!is.na(isi) && nchar(isi) > 0)
    res <- kth_diva_pubs() %>% filter(ISI == isi) %>% collect()

  return(res)
}

#* Data for an author, given an author identifier (KTH identifier or ORCID)
#* @get /v1/author/<AuthorId>
#* @response 400 Invalid input.
#* @param AuthorId:string An author identifier; examples for various identifiers (kthid, ORCID): u1mzh00z, 0000-0001-5302-1698
#* @tag Author
function(AuthorId) {

  get_identifier <- function(x, re)
    sapply(regmatches(x, gregexpr(re, x, perl = TRUE)), "[", 1)

  re_orcid <- sprintf("(%s)", paste0(collapse = "-",
                                     c(rep("\\d{4}", 3), "\\d{3}[1-9Xx]")))

  re_kthid <- "(u1.{6})"

  .kthid <- get_identifier(AuthorId, re_kthid)
  .orcid <- get_identifier(AuthorId, re_orcid)

  res <- data.frame()

  if (!is.na(.kthid) && nchar(.kthid) > 0)
    res <- kth_diva_authors() %>% filter(kthid == .kthid) %>% collect()

  if (!is.na(.orcid) && nchar(.orcid) > 0)
    res <- kth_diva_authors() %>% filter(orcid == .orcid) %>% collect()

  return(res)
}

aliases <- kth_diva_aliases()

#* DiVA author aliases
#* @get /v1/aliases
#* @response 400 Invalid input.
#* @tag Author
#* @serializer csv
function() {
  aliases
}

issues <- kth_diva_issues()

#* Full set of potential issues due to multiple author identifiers etc
#* @get /v1/issues
#* @response 400 Invalid input.
#* @tag Issues
function() {
  issues
}

#* Overview of multiplettes (ORCIDs with multiple associated kthids)
#* @get /v1/issues/overview
#* @response 400 Invalid input.
#* @tag Issues
#* @serializer csv
function() {
  issues$overview
}

#* Publication identifiers associated with author identifier multiplettes,
#* showing potential candidates for merging
#* @get /v1/issues/details
#* @response 400 Invalid input.
#* @tag Issues
#* @serializer csv
function() {
  issues$details
}

#* Publications where one kth identifier is associated with multiple ORCIDs
#* @get /v1/issues/publications
#* @response 400 Invalid input.
#* @tag Issues
#* @serializer csv
function() {
  issues$pubs
}

checks <- kth_diva_checks()

# #* Check for missing kthids for publications affiliated with KTH
# #* @get /v1/check/missing/kthid
# #* @response 400 Invalid input.
# #* @tag Check
# #* @serializer csv
# function() {
#   #checks$missing_kthid
# }

#* Check for missing conference paper publication dates
#* @get /v1/check/missing/confpubdate
#* @response 400 Invalid input.
#* @tag Check
#* @serializer csv
function() {
  checks$missing_confpubdate
}

# #* Check for publications in journals that have no ISSN specified
# #* @get /v1/check/missing/journalids
# #* @response 400 Invalid input.
# #* @tag Check
# #* @serializer csv
# function() {
#   #checks$missing_journal_ids
# }

#* Check for publication title multiplettes
#* @get /v1/check/multiplettes/title
#* @response 400 Invalid input.
#* @tag Check
#* @serializer csv
function() {
  checks$title_multiplettes
}

#* Check for invalid submission status
#* @get /v1/check/invalid/submissionstatus
#* @response 400 Invalid input.
#* @tag Check
#* @serializer csv
function() {
  checks$submission_status_invalid
}

#* Check for publications with odd book chapter titles
#* @get /v1/check/odd/bookchaptertitles
#* @response 400 Invalid input.
#* @tag Check
#* @serializer csv
function() {
  checks$odd_book_chapters
}

#* Check for publications with invalid ISI identifiers
#* @get /v1/check/invalid/ISI
#* @response 400 Invalid input.
#* @tag Check
#* @serializer csv
function() {
  checks$invalid_ISI
}

#* Check for publications with invalid DOI identifiers
#* @get /v1/check/invalid/DOI
#* @response 400 Invalid input.
#* @tag Check
#* @serializer csv
function() {
  checks$invalid_DOI
}

#* Check for publications with invalid ISSN identifiers
#* @get /v1/check/invalid/ISSN
#* @response 400 Invalid input.
#* @tag Check
#* @serializer csv
function() {
  checks$invalid_ISSN
}

f <- rmarkdown::render(
  system.file(package = "kthcorpus",
    "rmarkdown", "checks-report.Rmd", mustWork = TRUE),
  quiet = TRUE)

#* A report in HTML for the checks
#* @get /v1/check/report
#* @response 400 Invalid input.
#* @tag Check
function(res) {
  include_html(file = f, res = res)
}

needs_refresh <- function(timeout_hours = 23) {

  timez <- kthcorpus:::diva_meta()$age
  all(timez > timeout_hours)

}

needs_render <- function() {
  t_render <- file.mtime(f)
  age <- as.double(difftime(Sys.time(), t_render, units = "h"))

  t_update <- kthcorpus:::diva_meta()$age

  all(age > t_update) || all(is.na(t_update))

}

#* Trigger new rendering of report
#* @get /v1/trigger/render
#* @response 400 Invalid input.
#* @tag Triggers
function(res) {

  if (!needs_render())
    return(FALSE)

  f <<- rmarkdown::render(
    system.file(package = "kthcorpus",
                "rmarkdown", "checks-report.Rmd", mustWork = TRUE),
    quiet = TRUE)

  return(file.exists(f))
}



#* Trigger refreshing of data
#* @get /v1/trigger/update
#* @response 400 Invalid input.
#* @tag Triggers
function(res) {

  if (!needs_refresh())
    return(FALSE)

  diva_refresh()

}


#* Curated DiVA corpus with S2 identifiers
#* @get /v1/curated/pubs/jq/<query:string>
#* @response 400 Invalid input.
#* @param query a jq query such as 'select(.S2_publication_ID == "f00ec4c737ca6722a4629eb5bcc25787c2173df5")'
#* @tag Curated
function(query) {
  kth_diva_curated(jq = URLdecode(query))
}

#* Total number of pubauth issues
#* @get /v1/curated/pubauthissues/rows
#* @response 400 Invalid input.
#* @param passphrase the passphrase for accessing this data
#* @tag Curated
function(passphrase) {
  nrow(kth_issues_pubauth(pass = passphrase))
}

#* A specific pubauth issue
#* @get /v1/curated/pubauthissues/row/<id:integer>
#* @response 400 Invalid input.
#* @param passphrase the passphrase for accessing this data
#* @param id the row number
#* @tag Curated
function(passphrase, id) {
  kth_issues_pubauth(pass = passphrase) %>%
    dplyr::slice(as.integer(id))
}

#* A set of pubauth issues given by a jq query
#* @get /v1/curated/pubauthissues/jq/<query:string>
#* @response 400 Invalid input.
#* @param passphrase the passphrase for accessing this data
#* @param query a jq query such as select(.kthid | test("^⚠"))
#* @tag Curated
function(passphrase, query) {
  kth_issues_pubauth(pass = passphrase, jq = URLdecode(query))
}

