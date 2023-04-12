#' Get latest Zenodo ROR filename
#' @param url the URL for Zenodo ROR data
#' @importFrom curl curl
#' @importFrom jsonlite fromJSON
#' @importFrom purrr pluck
#' @export
latest_ror_url <- function(url = "https://zenodo.org/api/records/?communities=ror-data&sort=mostrecent"){
  curl(url, open = "rb") %>%
    fromJSON(flatten = TRUE) %>%
    pluck("hits", "hits", "files", 1, "links.self")
}

#' Get ROR data from Zenodo
#' @param url the URL for ROR data from Zenodo
#' @import jsonlite dplyr purrr tidyr zip
#' @export
tidy_rorzip <- function(url = latest_ror_url()) {

  t0 <- Sys.time()
  message("Downloading the ROR data, patience please (tidying the data takes time), starting at: ", t0)

  rorzip <- file.path(tempdir(), "ror.zip")
  on.exit(unlink(rorzip))
  url |> download.file(destfile = rorzip)

  fn <- zip::zip_list(rorzip) |> filter(grepl("\\.json$", filename)) |> pull(filename)

  message("Reading and parsing the json in the zip (flattening and simplyfying w jsonlite), patience pls...")
  ror <- as_tibble(
    fromJSON(txt = unz(description = rorzip, filename = fn),
             simplifyDataFrame = TRUE, flatten = TRUE)
  )

  message("Tidying the data into tabular tables (rectangular one-to-one and one-to-many data)")

  # extract non-list columns (one-to-one) in core table
  message("Generating core table...")

  orgs <- ror |> select_if(.predicate = function(x) !is.list(x)) |> as_tibble()

  # extract some specific one-to-many relationships into child tables
  message("Generating child tables...")

  otm_cols <- c("aliases", "acronyms", "links", "types")

  unnest_col <- function(x)
    ror |> select(id, x) |> unnest(cols = -"id")

  otm <-
    map(otm_cols, unnest_col) |>
    setNames(nm = paste0("ror_", otm_cols))

  # extract list columns for external identifiers (one-to-many)

  message("Generating external identifiers table...")

  colz <- ror |> select(ends_with("all")) |> colnames()

  unnest_colz <- function(x) {
    ror |> select(id, all_of(x)) |> unnest(cols = all_of(x))
  }

  links <- map(colz, unnest_colz, .progress = TRUE)

  extids <-
    map(links, function(x) pivot_longer(x, cols = -"id")) |>
    map_df(bind_rows)

  message("Tidying the table column names")

  rename_colz <- function(x)
    x %>%
    stringr::str_split("\\.", 3) %>%
    map_chr(function(x) ifelse(length(x) > 1 , pluck(x, 2), pluck(x, 1)))

  names(orgs) <- rename_colz(names(orgs))
  extids <- extids |> mutate(name = rename_colz(name))
  # remove duplicated column (names and values)

  orgs <- orgs |> select(-which(duplicated(names(orgs))))

  t1 <- Sys.time()
  duration <- round(difftime(t1, t0, units = "secs"))
  message("Done, returning the results at: ", t1, " (duration was ", duration, " seconds)")

  # combine all results
  c(otm,
    list(
      ror_orgs = orgs,
      ror_ids = extids)
  )
}

#' Save ROR lookup data to Minio
#' @param ror the ROR dataset as collected from Zenodo with `tidy_rorzip()`
#' @importFrom aws.s3 s3write_using
#' @import institutions dplyr stringr
#' @export
update_ror_lookup_minio <- function(ror = tidy_rorzip()){

  lookup <-
    ror$ror_ids %>%
    mutate(sid = str_extract(id, "[^/]\\d+.*?$")) %>%
    left_join(ror$ror_orgs, by = "id") %>%
    select(sid, grid_id = GRID) %>%
    left_join(institutions_table("addresses"), by = "grid_id") %>%
    select(sid, grid_id, lat, lng, city, state, country) %>%
    left_join(institutions_table("institutes"), by = "grid_id") %>%
    select(ror_id = sid, everything()) %>%
    distinct() %>%
    select(ror_id, grid_id, name, lat, lng, city, state, country)

  s3write_using(x = lookup, FUN = write.csv, row.names = FALSE, object = "ROR_geo_lookup.csv", bucket = "copub-data")
}

#' Get ROR lookup table from Minio
#' @importFrom aws.s3 s3read_using
#' @export
get_ror_lookup <- function() {
  s3read_using(FUN = read_csv, show_col_types = FALSE, locale = locale(encoding = "UTF-8"), object = "ROR_geo_lookup.csv", bucket = "copub-data")
}

#' Get org/country geo lookup table from Minio
#' @importFrom aws.s3 s3read_using
#' @import readr
#' @export
get_geocountry <- function() {
  s3read_using(FUN = read_csv, show_col_types = FALSE, locale = locale(encoding = "UTF-8"), object = "geo_country.csv", bucket = "copub-data")
}

#' Get org/city/country lookup table from Minio
#' @importFrom aws.s3 s3read_using
#' @import readr
#' @export
get_geocity <- function() {
  s3read_using(FUN = read_csv, show_col_types = FALSE, locale = locale(encoding = "UTF-8"), object = "geo_city.csv", bucket = "copub-data")
}
