#' KTH APIs
#'
#' DiVA is a Swedish research publication database with national and
#' institutional portals available. This R package downloads and makes
#' data available to use from R.
#'
#' There is a national DiVA database portal and institutional database portals:
#'
#'  - [National DiVA portal](https://www.diva-portal.org/smash/searchad.jsf)
#'  - [Institutional DiVA portal for KTH Royal Institute of Technology](https://kth.diva-portal.org/smash/search.jsf)
#' @name diva
#' @docType package
#' @keywords package
#' @aliases package-diva
#'
# needed for use of . in magrittr pipelines
utils::globalVariables(
  c(
    ".", "description.en", "Name", "PID", "extorg",
    "is_external", "is_remappable_extorg", "kthid", "n2", "n_commas",
    "n_pid", "name", "orcid", "orcid.x", "orcid.y", "orgids", "pids", "pubs",
    "DOI", "ISI", "JournalEISSN", "JournalISSN", "Notes", "PublicationDate",
    "PublicationType", "Status", "Title", "has_notes", "is_QSorNQC", "is_not_published"
  )
)
NULL

#print_global_vars <- function(undef_global_vars)
#  cat(paste(collapse = ", ", sprintf("\"%s\"", unlist(strsplit(undef_global_vars, " ")))))
