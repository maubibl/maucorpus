library(readr)

# data types
ct <- cols(
 .default = col_character(),
 is_closed = col_logical(),
 level = col_integer(),
 orgid = col_integer()
)

unit_names <-
  readr::read_csv("data-raw/unit_names.csv", show_col_types = FALSE, col_types = ct)

# readr::write_csv(unit_names, "data-raw/unit_names.csv", na = "")

usethis::use_data(unit_names, overwrite = TRUE)

#system("mc cp data-raw/unit_names.csv kthb/kthcorpus")
