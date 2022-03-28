# check to see if data-raw/kth_besta.csv diffs from embedded data
ct <- readr::cols(.default = col_character())

besta_diff <-
  daff::diff_data(
    kth_besta,
    readr::read_csv("data-raw/kth_besta.csv", col_types = ct)
  )

daff::render_diff(besta_diff)


# if it diffs, then rebundle data from CSV
kth_besta <-
  readr::read_csv("data-raw/kth_besta.csv", col_types = ct)

#readr::write_csv(kth_besta, "data-raw/kth_besta.csv", na = "")

usethis::use_data(kth_besta, overwrite = TRUE)

# system("mc cp data-raw/kth_besta.csv kthb/kthcorpus/kth_besta.csv")
