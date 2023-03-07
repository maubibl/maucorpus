#' @import dplyr cordis
kth_cordis <- function() {
  #devtools::install_github("kth-library/cordis")

  if (!requireNamespace("cordis", quietly = TRUE)) {
    stop(
      "Package \"cordis\" must be installed to use this function.",
      call. = FALSE
    )
  }

  shortName <- projectID <- projectAcronym <- totalCost <- NULL

  cordis::cordis_import()

  #cordis:::cordis_dropdb(confirm = TRUE)
  #cordis::cordis_import()

  con <- cordis::cordis_con()
  on.exit(duckdb::dbDisconnect(con))

  he_project_ids <-
    con |> tbl("he_organization") |>
    filter(shortName == "KTH") |> collect() |>
    distinct(projectID, projectAcronym) |>
    pull(projectID)

  he_projects <-
    con |> tbl("he_project") |> filter(id %in% he_project_ids) |> collect()

  #he_projects |> View()

  fp7_project_ids <-
    con |> tbl("fp7_organization") |>
    filter(shortName == "KTH") |> collect() |>
    distinct(projectID, projectAcronym) |>
    pull(projectID)

  fp7_projects <-
    con |> tbl("fp7_project") |> filter(id %in% fp7_project_ids) |> collect() |>
    mutate(totalCost = readr::parse_number(totalCost)) #|>
  #arrange(desc(totalCost)) |>
  #pull(totalCost)

  h2020_project_ids <-
    con |> tbl("h2020_organization") |>
    filter(shortName == "KTH") |> collect() |>
    distinct(projectID, projectAcronym) |>
    pull(projectID)

  h2020_projects <-
    con |> tbl("h2020_project") |> filter(id %in% h2020_project_ids) |> collect()

  # combine Horizon Europe, H2020 and FP7 projects into one table
  kth_cordis <-
    list(h2020_projects, he_projects, fp7_projects) |> bind_rows()

  return (kth_cordis)
  #cordis::cordis_tables()

}

#' @import formas
kth_formas <- function() {

  if (!requireNamespace("formas", quietly = TRUE)) {
    stop(
      "Package \"formas\" must be installed to use this function.",
      call. = FALSE
    )
  }

  tictoc::tic()

  message("Requesting projects from Formas")
  formas <- formas::formas_projects()

  #message("Requesting Formas lookup tables")
  #flt <- formas::formas_lookup_tables()

  # extract those related to KTH
  formas_rows <-
    formas |> getElement("medelsf\u00f6rvaltareNamn") |>
    grep(pattern = "Kungl.* Tekniska h.gskolan")

  kth_formas <- formas[formas_rows,]

  tictoc::toc()
  message("Done")

  return (kth_formas)

}

#' @import vinnova
kth_vinnova <- function() {

  if (!requireNamespace("vinnova", quietly = TRUE)) {
    stop(
      "Package \"vinnova\" must be installed to use this function.",
      call. = FALSE
    )
  }

  message("Requesting projects from Vinnova")

  tictoc::tic()
  vinnova <- vinnova::vinnova_latest(from_date = "2010-01-01")

  vinnova_ids <-
    vinnova$projects$KoordinatorOrg |>
    grep(pattern = "Kungliga") |> unique()

  kth_vinnova <- vinnova$projects[vinnova_ids,]
  tictoc::toc()

  message("Done")

  return(kth_vinnova)

}

#' @import cordis
cordis_swe_vat <- function() {

  if (!requireNamespace("cordis", quietly = TRUE)) {
    stop(
      "Package \"cordis\" must be installed to use this function.",
      call. = FALSE
    )
  }

  organisationID <- vatNumber <- shortName <- name <- country <- NULL

  con <- cordis::cordis_con()
  on.exit(duckdb::dbDisconnect(con, shutdown = TRUE))

  cordis_org_vat <-
    list("he_organization", "fp7_organization", "h2020_organization") |>
    purrr::map(
      function(x) con |> tbl(x) |>
        filter(country == "SE") |>
        distinct(organisationID, vatNumber, shortName, name) |>
        collect()
    ) |> bind_rows() |> distinct() |> select(-shortName) |>
    filter(!is.na(vatNumber))

  return(cordis_org_vat)
}

#' @import swecris
kth_swecris <- function() {

  if (!requireNamespace("swecris", quietly = TRUE)) {
    stop(
      "Package \"swecris\" must be installed to use this function.",
      call. = FALSE
    )
  }

  organisationNameSv <- organisationId <- NULL

  message("Requesting projects from SweCRIS")

  tictoc::tic()

  kth_orgid <-
    swecris::swecris_organisations() |>
    filter(organisationNameSv == "KTH, Kungliga tekniska h\u00f6gskolan") |>
    pull(organisationId)

  kth_swecris <- swecris::swecris_projects(orgid = kth_orgid)

  tictoc::toc()

  message("Done")

  return (kth_swecris)

}

#' @import OpenAIRE
kth_openaire <- function(format = c("tsv", "xml")) {

  if (!requireNamespace("OpenAIRE", quietly = TRUE)) {
    stop(
      "Package \"OpenAIRE\" must be installed to use this function.",
      call. = FALSE
    )
  }

  # XML format takes longer (200x, 3+ minutes), but provides more details
  message("Requesting projects from OpenAIRE")
  tictoc::tic()
  fmt <- match.arg(format)
  res <- OpenAIRE::openaire("projects", params = OpenAIRE::api_params(
    format = fmt,
    proj_country = "SE",
    proj_org = "Royal Institute of Technology"
  ))
  tictoc::toc()
  message("Done")
  return(res)
}

projects_upload <- function() {

  kth_cordis <- kth_cordis() #
  kth_formas <- kth_formas() # 21 s, ca 471 projects
  kth_vinnova <- kth_vinnova() # 441 projects, ca 4 minutes
  kth_swecris <- kth_swecris() # 5 secs
  kth_openaire <- kth_openaire() # 2 secs or 3 minutes, 813 projects
  ko <- kthid_orcid()

  ko |> readr::write_csv("/tmp/kthid_orcid.csv")

  kth_openaire |> readr::write_csv("/tmp/projects_openaire.csv")
  kth_cordis |> readr::write_csv("/tmp/projects_cordis.csv")
  kth_vinnova |> readr::write_csv("/tmp/projects_vinnova.csv")
  kth_formas |> readr::write_csv("/tmp/projects_formas.csv")
  kth_swecris |> readr::write_csv("/tmp/projects_swecris.csv")

  diva_upload_s3("/tmp/kthid_orcid.csv")
  diva_upload_s3("/tmp/projects_openaire.csv")
  diva_upload_s3("/tmp/projects_cordis.csv")
  diva_upload_s3("/tmp/projects_vinnova.csv")
  diva_upload_s3("/tmp/projects_formas.csv")
  diva_upload_s3("/tmp/projects_swecris.csv")

}


# # frequency of associated people
# # 21% of projects have no info about Principal Investigator
# kth_swecris |>
#   left_join(
#     kth_swecris |>
#     unnest(peopleList) |>
#     select(projectId, peopleList) |>
#     unnest_wider(peopleList) |>
#       filter(roleEn == "Principal Investigator") |>
#     group_by(projectId) |>
#     count() |>
#     arrange(desc(n))
#   ) |>
#   group_by(projectId) |>
#   summarize(n = sum(n)) |>
#   pull(n) |> table(useNA = "ifany")

