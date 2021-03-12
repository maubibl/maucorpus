library(humaniformat)

authors <-
  kth_diva_aliases()$name %>%
  format_reverse() %>%
  format_period() %>%
  parse_names() %>%
  as_tibble() %>%
  collect()

my_mode <- function(x) {
  dd <- unique(x)
  dd[which.max(tabulate(match(x,dd)))]
}

name_glue <- function(first, mid, last) {
  midfirst <- paste0(collapse = " ", na.omit(c(mid, first)))
  paste0(collapse = ", ", na.omit(c(last, midfirst)))
}

# candidates for improving parsable name variations
kth_diva_aliases() %>%
  collect() %>%
  bind_cols(authors) %>%
  group_by(kthid) %>%
  summarise_each(funs = list(my_mode)) %>%
  arrange(name) %>%
  mutate(fullname = pmap_chr(list(first_name, middle_name, last_name), name_glue)) %>%
  mutate(is_reversibly_parsable = fullname == name) %>%
  filter(!is_reversibly_parsable)

