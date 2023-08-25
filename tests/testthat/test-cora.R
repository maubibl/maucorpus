test_that("Requests against DiVA CORA organisations search API works", {

  orgs <- diva_organisations()

  cora <- diva_organisations_cora()

  res <-
    cora |>
    left_join(orgs, by = c("orgid")) |>
    filter(p_orgid.x == p_orgid.y)

  # root node w orgid = 177 is not present in CORA API result
  is_valid <- nrow(res) == nrow(orgs) - 1

  expect_true(is_valid)
})

# cora_person_search(freetext = "Jeppsson", name = "Tobias Jeppsson") |>
#   flatten_cora_records()

# cora_person("5850") |>
#    flatten_cora_records()

# cora_organisation(5858) |>
#   flatten_cora_records()

# ps <- cora_person_search("Anders Wändahl")
#
#
# ps %>%
#   pluck("dataList", "data") %>%
#   map("record", "data") %>%
#   map(list("actionLinks", "read", "url"))
#
# ps %>% hoist(dataList)
#
# ps %>%
#   pluck("dataList", "data") %>%
#   map("record", "data") %>%
#   map(list("data", "name"))
#
# ps %>%
#   pluck("dataList", "data") %>%
#   map("record", "data") %>%
#   map(list("data", "children")) %>%
#   map(list(1, 1, 1)) %>%
#   map_df(.f = function(x) tibble(n = pluck(x, "name"), v = pluck(x, "value")))
#
# ps %>%
#   pluck("dataList", "data") %>%
#   map("record", "data") %>%
#   map(list("data", "children")) %>%
#   map(list(1, 1)) %>%
#   View()
#   map_df(.f = function(x) tibble(n = pluck(x, "name"), v = pluck(x, "value")))
#
#
# ps %>%
#   pluck("dataList", "data") %>%
#   head(1) %>%
#   map(list("record", "children", "data")) %>%
#   map("data") %>%
#   map("children", "children")
#   View()
