kth_besta <- readr::read_csv("data-raw/kth_besta.csv")

#readr::write_csv(kth_besta, "data-raw/kth_besta.csv", na = "")
#kth_besta <-
#  kth_besta %>%
#  select(emp_code = Kod, emp_title_sv = Befattning, emp_title_en = Position)

usethis::use_data(kth_besta, overwrite = TRUE)
