library(cordis)

cordis_tables()

con <- cordis_con()

cod <- function(table)
  con |> tbl(table) |> collect() |>
  mutate(totalCost = as.character(totalCost))

cordis_orgs <- list(
  cod("he_organization"),
  cod("fp7_organization"),
  cod("h2020_organization")
)

# 1243 organisations with VAT number found in CORDIS data
vat <-
  cordis_orgs |> # 350 000 records
  bind_rows() |>
  filter(country == "SE") |> # 10000+ records
  select(organisationID, vatNumber, name) |>
  filter(!is.na(vatNumber)) |>
  distinct()

crf <-
  vat$name |> unique() |> gsub(pattern = "HOEGSKOLAN", replacement = "HÖGSKOLAN") |>
  head(10) |>
  cr_funders_resolve()

crf |> filter(!is.na(id)) |> group_by(seq) |> summarise(across(everything(), first)) |>
  mutate(lookup = gsub(lookup, pattern = "HÖGSKOLAN", replacement = "HOEGSKOLAN")) |>
  left_join(vat, by = c("lookup" = "name")) |>
  select(any_of(c("lookup", "uri", "name", "vatNumber")))

# # A tibble: 6 × 4
# lookup                        uri                                     name                          vatNumber
# <chr>                         <chr>                                   <chr>                         <chr>
# 1 KAROLINSKA INSTITUTET         http://dx.doi.org/10.13039/501100004047 Karolinska Institutet         SE202100297301
# 2 LUNDS UNIVERSITET             http://dx.doi.org/10.13039/501100003252 Lunds Universitet             SE202100321101
# 3 UPPSALA UNIVERSITET           http://dx.doi.org/10.13039/501100007051 Uppsala Universitet           SE202100293201
# 4 RYMDSTYRELSEN                 http://dx.doi.org/10.13039/501100001859 Swedish National Space Agency SE202100258501
# 5 VÄSTRA GÖTALANDSREGIONEN      http://dx.doi.org/10.13039/100007212    Västra Götalandsregionen      SE232100013101
# 6 SVERIGES LANTBRUKSUNIVERSITET http://dx.doi.org/10.13039/501100004360 Sveriges Lantbruksuniversitet SE202100281701



# # lookup GRID from local database of institutions
# vat$name |> unique() |>
#   gsub(pattern = "&|,|-|[(]|[)]|[\\.]|[/]|[']|[:]", replacement = " ") |> #  nth(567)
#   map_dfr(institutions::institutions_search, .id = "id")
