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
#' @name kthcorpus
#' @docType package
#' @keywords package
#' @aliases package-kthcorpus
#'
# needed for use of . in magrittr pipelines
utils::globalVariables(
  c(
    ".", "description.en", "Name", "PID", "extorg",
    "is_external", "is_remappable_extorg", "kthid", "n2", "n_commas",
    "n_pid", "name", "orcid", "orcid.x", "orcid.y", "orgids", "pids", "pubs",
    "DOI", "ISI", "JournalEISSN", "JournalISSN", "Notes", "PublicationDate",
    "PublicationType", "Status", "Title", "has_notes", "is_QSorNQC", "is_not_published",
    "emp_beg", "emp_degree", "emp_end", "emp_lastmod", "LastModified",
    "mid", "Key", "check_key", "initials", "last_name"
  )
)
NULL

#print_global_vars <- function(undef_global_vars)
#  cat(paste(collapse = ", ", sprintf("\"%s\"", unlist(strsplit(undef_global_vars, " ")))))

#' @title Research Subject Areas from https://uka.se
#' @description Research Subject Areas have codes, descriptions and are arranged
#' in three levels, 6 at level 1, 42 at level 2, 260 at level 3 (5 digit codes)
#' See \url{https://www.uka.se/statistik--analys/information-om-statistiken/amneslistor-och-huvudomraden/2017-02-14-forskningsamnen.html}
#' @format A data frame with 308 rows and 4 variables:
#' \describe{
#'   \item{\code{id}}{integer subject area code}
#'   \item{\code{level}}{double a level, granularity for code (1..3)}
#'   \item{\code{swe}}{character description in Swedish}
#'   \item{\code{eng}}{character description in English}
#'}
#' @details Data from \url{https://www.uka.se/download/18.7391c377159bc0155b81ef8/1487841861615/forskningsamnen-standard-2011.xlsx}
"research_areas"

#' @title Employment Titles from Statistics Sweden
#' @description Employment Titles have codes and descriptions
#' @format A data frame with 7626 rows and 4 variables:
#' \describe{
#'   \item{\code{id}}{character code for employment title}
#'   \item{\code{desc_swe}}{character description in Swedish}
#'   \item{\code{cat_desc}}{character staff category}
#'   \item{\code{is_uf_ta}}{character UF denotes educational or research title, TA denotes technical or administrative}
#'}
#' @details See \url{https://www.h6.scb.se/anstallningsbenamning/}
"ss_employment_title"

#' @title HR mapping
#' @description Field mappings used in HR data extract
#' @format A data frame with 14 rows and 5 variables:
#' \describe{
#'   \item{\code{colname}}{character lowersnakecase column name}
#'   \item{\code{export}}{character export table non-ascii name}
#'   \item{\code{name}}{character Name of column}
#'   \item{\code{table}}{character Source table}
#'   \item{\code{field}}{character Source field}
#'}
"hr_mapping"


#' @title Mappings for check data
#' @description Field massings for the check data used for column names etc
#' \describe{
#'   \item{\code{colname_en}}{character lowersnakecase column name}
#'   \item{\code{desc_swe}}{character Swedish language explanation for field}
#'}
"check_mapping"
