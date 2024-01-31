#' Read Web Of Science tab separated file(s)
#'
#' Read one or more wos tsv files
#'
#' @param file one or more paths to wos tsv files
#' @param format one of "unnested" (default) or "compact"
#' @return either one tibble (compact) or a list of several unnested tibbles
#' @export
#' @importFrom stringr str_match_all str_count
wos_read_tsv <- function(file, format = c("unnested", "compact")) {

  tag <- accession_number <- value <- author_address <- n_char <-
    addresses <- authors <- inst <- auth <- n_i <- n_a <- inst_value <-
    auth_value <- NULL

  early_exit <- switch(match.arg(format),
     "unnested" = FALSE,
     "compact" = TRUE
  )

  rt <- function(x) read_tsv(x,
    show_col_types = FALSE,
    col_types = list(.default = col_character())
  )

  df <- file |> map_dfr(rt)

  wf <- kthcorpus::wos_tag_fields |> rename(tag_short = tag)

  wos_map <- function(x) {
    tibble(tag_short = x) |>
      inner_join(wf, by = "tag_short") |>
      pull("field")
  }

  wos <- df |> rename_with(wos_map) |>
    mutate(across(contains(c("_count", "year_published", "volume", "pubmed_id")),
      \(x) parse_integer(x)))

  if (early_exit)
    return(wos)

  contains_sep <- function(x)
    str_count(x, "; ")

  contains_number <- function(x)
    sum(as.integer(x), na.rm = TRUE)

  myfun <- contains_sep

  nested_cols <-
    wos |>
    mutate(across(everything(), myfun)) |>
    summarize(across(everything(), \(x) sum(x, na.rm = TRUE))) |>
    pivot_longer(cols = everything()) |>
    filter(value > 0) |> pull(name)

  special_col <- "author_address"
  nested_cols <- nested_cols[nested_cols != special_col]

  w <-
    wos |>
    mutate(across(any_of(nested_cols), \(x) strsplit(split = "; ", x))) |>
    mutate(across(any_of(nested_cols), \(x) map(x, \(x) as_tibble(x))))

  woz <- nested_cols |> map(\(x) {
    w |> unnest(c(x, accession_number)) |>
      select(any_of(c("accession_number", "value"))) |>
      filter(!is.na(value)) |>
      rename_with(\(y) x, .cols = "value")
  }) |> setNames(nm = nested_cols)

  # write to disk
  #map(names(woz), \(x) woz |> getElement(x) |>
  #arrow::write_parquet(sprintf("~/repos/buckets/xcheck/wos_2023_%s.parquet", x)))

  a <- wos |>
    mutate(n_char = map_int(author_address, nchar)) |>
    arrange(desc(n_char))

  unpack_address <- function(id, a) {

    m <- a |> str_match_all("(?<authz>\\[.*?\\])?(?:\\s+)(?<instz>.*?; )")
    gs <- function(x, p, r) gsub(pattern = p, replacement = r, x = x)
    instz <- m[[1]][ ,"instz"] |> gs("(.*?);\\s+", "\\1")
    authz <- m[[1]][ ,"authz"] |> gs("\\[(.*?)\\]", "\\1")

    a2 <-
      tibble(addresses = instz, authors = authz) |>
      mutate(inst = strsplit(addresses, split = "; ")) |>
      mutate(auth = strsplit(authors, split = "; ")) |>
      mutate(n_i = lengths(inst), n_a = lengths(auth)) |>
      arrange(desc(n_i), desc(n_a)) |>
      mutate(inst = map(inst, as_tibble)) |>
      mutate(auth = map(auth, as_tibble))

    a2 |>
      unnest(cols = c(inst, auth), names_sep = "_") |>
      select(inst_value, auth_value) |>
      bind_cols(accession_number = id) |>
      rename(inst = 1, auth = 2) |>
      select(3, 1, 2)

  }

  punpack <- possibly(unpack_address, otherwise = tibble(), quiet = TRUE)

  aa <-
    a |> select(accession_number, author_address) |>
    pmap_dfr(.progress = TRUE,
      .f = \(accession_number, author_address)
        punpack(accession_number, author_address)
    )

  res <- append(list(
      publications = wos |> select(!any_of(c(nested_cols, special_col))),
      inst_auth = aa
    ), woz)

  return (res)

}
