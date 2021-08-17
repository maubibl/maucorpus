#' Curated DiVA corpus data
#' A curated version of DiVA data with S2_publication_ID and S2_authors
#'
#' @param jq query for [filtering the data](https://docs.ropensci.org/jqr/)
#' @return tibble
#' @examples
#' \dontrun{
#'  kth_diva_curated(jq = 'select(.PID == "13125" or .PID == "461429")')
#'  kth_diva_curated(jq = 'select(.S2_publication_ID == "f00ec4c737ca6722a4629eb5bcc25787c2173df5")')
#' }
#' @export
#' @importFrom rappdirs app_dir
#' @importFrom readr read_lines
#' @importFrom ndjson stream_in
#' @importFrom jqr jq
#' @importFrom R.utils bzip2 bunzip2
kth_diva_curated <- function(jq = NULL) {

  diva_tmp <- function(file) {
    fp <- file.path(rappdirs::app_dir("kthcorpus")$config(), file)
    if (!dir.exists(dirname(fp)))
      dir.create(dirname(fp), recursive = TRUE)
    fp
  }

  if (!file.exists(diva_tmp("diva.json"))) {
    R.utils::bunzip2(
      filename = system.file(package = "kthcorpus", "extdata", "diva.json.bzip2"),
      destname = diva_tmp("diva.json")
    )
  }

  if (!file.exists(diva_tmp("diva.json")))
    return(character(0))

  # if no jq query is specified, return all data in flat format
  if (is.null(jq))
    return(ndjson::stream_in(diva_tmp("diva.json"), cls = "tbl"))

  cb <- function(x, pos) {
    jqr::jq(x, jq) %>%
      textConnection() %>%
      jsonlite::stream_in(simplifyDataFrame = FALSE, verbose = FALSE)# %>%
  }

  res <-
    readr::read_lines_chunked(diva_tmp("diva.json"),
      callback = ListCallback$new(cb), chunk_size = 9e4)

  return(res)
}

#' Issues with publication and author affiliations

#' @param pass passphrase required for accessing the data,
#'   Default: Sys.getenv("DIVA_PASS")
#' @param jq query for [filtering the data](https://docs.ropensci.org/jqr/)
#' @return json object
#' @examples
#' \dontrun{
#'  kth_issues_pubauth()
#'  issues <- kth_issues_pubauth(jq = 'select(.kthid | test("^⚠"))')
#' }
#' @seealso
#'  \code{\link[rappdirs]{app_dir}}
#'  \code{\link[rcrypt]{decrypt}}
#'  \code{\link[readr]{read_file}}
#' @export
#' @importFrom rappdirs app_dir
#' @importFrom rcrypt decrypt
#' @importFrom readr read_lines
#' @importFrom ndjson stream_in
#' @importFrom jqr jq
kth_issues_pubauth <- function(pass = Sys.getenv("DIVA_PASS"), jq = NULL) {

  stopifnot(nzchar(pass))

  diva_tmp <- function(file) {
    fp <- file.path(rappdirs::app_dir("kthcorpus")$config(), file)
    if (!dir.exists(dirname(fp)))
      dir.create(dirname(fp), recursive = TRUE)
    fp
  }

  if (!file.exists(diva_tmp("ap.json"))) {
    rcrypt::decrypt(
      system.file(package = "kthcorpus", "extdata", "ap.rcrypt"),
      diva_tmp("ap.json"),
      passphrase = pass
    )
  }

  if (!file.exists(diva_tmp("ap.json")))
    return(character(0))

  # if no jq query is specified, return all data in flat format
  if (is.null(jq))
    return(ndjson::stream_in(diva_tmp("ap.json"), cls = "tbl"))



  cb <- function(x, pos) {
    jqr::jq(x, jq) %>% #, flags = jqr::jq_flags(ascii = TRUE)) %>%
      textConnection() %>%
      jsonlite::stream_in(simplifyDataFrame = FALSE, verbose = FALSE)# %>%
    #jsonlite::toJSON(pretty = TRUE) %>%
    #as.character()
  }

  res <-
    readr::read_lines_chunked(diva_tmp("ap.json"),
      callback = ListCallback$new(cb), chunk_size = 9e4)
  #readLines(diva_tmp("ap.json"), encoding = "utf-8") %>%
  #gsub(., pattern = "⚑", replacement = "F", fixed = TRUE) %>%
  #gsub(., pattern = "⚠⚠", replacement = "WTF", fixed = TRUE) %>%
  #jqr::jq(jq) %>% #, flags = jqr::jq_flags(ascii = TRUE)) %>%
  #textConnection() %>%
  #jsonlite::stream_in(simplifyDataFrame = FALSE, verbose = FALSE) %>%
  #jsonlite::toJSON()

  return(res)
}

#jsonlite::stream_in(textConnection(readLines(diva_tmp("ap.json")))) %>%
#as_tibble() %>% toJSON()select(kthid) %>% unique() %>% filter(grepl("^⚠", kthid))

# readLines("data-raw/dontshare/test5.json", encoding = "utf-8") %>%
# #  gsub(., pattern = "⚑", replacement = "F", fixed = TRUE) %>%
# #  gsub(., pattern = "⚠⚠", replacement = "WTF", fixed = TRUE) %>%
#   jqr::jq("[.kthid, .orcid, .kth, .note] | @csv", flags = jqr::jq_flags(stream = TRUE)) %>%
#   #textConnection() %>%
#   read_csv(quote = "\"")
#   jsonlite::stream_in(simplifyDataFrame = FALSE ,verbose = FALSE) %>%
#   jsonlite::toJSON()

# authors_curated <-
#   setNames(authors_curated, gsub("$", "_", names(authors_curated), fixed = TRUE))
#
# authors_curated %>%
#   select_if(is.character) %>%
#   mutate(across(.fns = function(x) na_if(x, "")))
#
# authors_curated %>%
#   select_if(function(x) !is.character(x))
#
# pull(aliases) %>% purrr::map_df(bind_rows)
