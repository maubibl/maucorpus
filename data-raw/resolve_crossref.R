library(rcrossref)

rcrossref::cr_funders(query = "sverige")

readr::read_csv("https://doi.crossref.org/funderNames?mode=list")

"https://api.crossref.org/funders" |> jsonlite::read_json(simplifyVector = T)

