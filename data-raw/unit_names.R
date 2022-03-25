unit_names <- readr::read_csv("data-raw/unit_names.csv", show_col_types = FALSE)

# readr::write_csv(unit_names, "data-raw/unit_names.csv")

usethis::use_data(unit_names, overwrite = TRUE)
