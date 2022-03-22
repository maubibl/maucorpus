kth_besta <- read.csv("data-raw/kth_besta.csv", fileEncoding = "UTF-8")

usethis::use_data(kth_besta, overwrite = TRUE)
