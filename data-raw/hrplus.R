# read data from minio
library(aws.s3)
library(dplyr)
library(readr)

# set up credentials for accessing minio using S3 protocol
# these environment variables need to be set
#AWS_ACCESS_KEY_ID=supersecret
#AWS_SECRET_ACCESS_KEY=supersecret
#AWS_S3_ENDPOINT=lib.kth.se:9000
#AWS_DEFAULT_REGION=bibliometrics

#file.edit("~/.Renviron")
#readRenviron("~/.Renviron")

# these are available buckets/datasets
bucket_list_df(use_https = FALSE)

# these are the files in the hrplus bucket
get_bucket_df("hrplus", use_https = FALSE)

my_file <-
  get_bucket_df("hrplus", use_https = FALSE) %>%
  arrange(LastModified) %>%
  head(1) %>% pull(Key)

# get the data in the file
f1 <- get_object(my_file, "hrplus", use_https = FALSE)

# read the file
#hr <- readr::read_csv(f1, quote = "\"")
hr <- hr_read_csv(f1)

# resolve "ÄMNESKOD" (using UKÄ data)
# what does ÄMNESKOD 999 mean? What does 10 or 100 mean?
# these codes do not seem to exist in UKÄ?

ra <-
  research_areas %>%
  mutate(id = as.character(id)) %>%
  rename(research_area = eng, scb_topic = id) %>%
  select(scb_topic, research_area)

d1 <- hr %>% left_join(ra)

d1 %>% count(scb_topic, research_area) %>% View()

probs <- problems(readr::read_csv(t1, col_names = FALSE))

# list of staff which has a employment code for a title which is marked
# as "Educational or Research" by Statistics Sweden
researchers <-
  hr %>%
  left_join(ss_employment_title, by = c("emp_code" = "id")) %>%
  filter(is_uf_ta == "UF")

############

library(vroom)
library(lubridate)
library(dplyr)
library(daff)

# TODO: check escaping of fields with comma, which is parsed as a
# field separator, need quoting of string values or escaping, otherwise...
# "wandering columns"

# use all files in a specific directory
src <- function() "~/repos/shinyproxy-debug/hr-share"
filez <- dir(src(), pattern = "*.csv", full.names = TRUE)
ts <- ymd(gsub("(\\d{8})_abu\\.csv", "\\1", filez))

# determine latest and previous files
ts2 <- ts[order(ts)]
latest <- sprintf("%s/%s_abu.csv", src(), format(max(ts2), "%Y%m%d"))
previous <- sprintf("%s/%s_abu.csv", src(), format(rev(ts2)[2], "%Y%m%d"))

# fcn to parse and read data from hr file
hr_read_csv <- function(file) {

  cs <- cols(.default = col_character(),
             `FÖDELSEÅR` = col_integer(),
             DATUM_NUV_BEF = col_integer(),
             BEF_TOM = col_integer(),
             SYSS_GRAD = col_integer()
  )

  #stopifnot(file.exists(file))

  # parse and remap colnames; use lowersnakecase field names
  readr::read_csv(file = file, col_types = cs, quote = "\"") %>%
    rename(
      kthid = KTHID,
      yob = `FÖDELSEÅR`,
      unit_abbr = ORG_NR,
      unit_name = ORG_NAMN,
      firstname = `FÖRNAMN`,
      lastname = EFTERNAMN,
      gender = `MAN/KVINNA`,
      emp_code = TJ_BEN_KOD,
      emp_desc = TJ_BE_TEXT,
      emp_nr = BEF_NR,
      emp_beg = BEF_FROM,
      emp_end = BEF_TOM,
      emp_lastmod = DATUM_NUV_BEF,
      emp_degree = SYSS_GRAD,
      scb_topic = `ÄMNESKOD`
    )

}

# fcn to diff arbitrary dataset to another one
hr_diff <- function(f1, f2) {
  d1 <- hr_read_csv(f1)
  d2 <- hr_read_csv(f2)
  dd <- daff::diff_data(d2, d1, ids = "KTHID",
                        never_show_order = TRUE, always_show_order = FALSE,
                        ignore_whitespace = TRUE, ordered = FALSE,
                        show_unchanged = FALSE, show_unchanged_columns = FALSE)
  dd
}

# read the latest dataset
hr_read_csv(latest)

# check what changes were since the previous dataset
render_diff(hr_diff(latest, previous))

# fcn to generate diffs for several datasets
hr_diffs <- function(max_tail = 1) {

  diffs <-
    tibble(current = ts, previous = lag(ts)) %>%
    # exclude first pair (no previous exists)
    slice(-1) %>%
    tail(max_tail)

  fd <- function(date, suffix = "_abu.csv")
    paste0(src(), "/", format(parse_date_time(date, "%Y%m%d"), "%Y%m%d"), suffix)

  diffs %>%
    #  tail(1) %>%
    mutate(d2 = fd(current), d1 = fd(previous)) %>%
    select(d1, d2) %>%
    mutate(diff = purrr::pmap(.l = ., .f = function(d1, d2) hr_diff(d1, d2)))

}

# create diffs for the previous three changes
diffz <- hr_diffs(max_tail = 3)

dz1 <- diffz$diff[[3]]
render_diff(dz1)

# read the fourth dataset
d1 <-
  diffz %>% head(10) %>% tail(1) %>% pull(d1) %>%
  hr_read_csv()

# inspect employee descriptions
d1 %>% count(emp_desc) %>% arrange(desc(n))

d1 %>% filter(grepl("ARVODIST", emp_desc))
latest %>% hr_read_csv %>% filter(grepl("ARVODIST", emp_desc))

# presence of "industridoktorand"?
latest %>% hr_read_csv %>% count(emp_desc) %>%
  arrange(desc(n)) %>% filter(grepl("INDUSTRI", emp_desc))

# more than 115 years old
d1 %>%
  mutate(age = year(Sys.Date()) - yob) %>%
  filter(age >= 115) %>%
  mutate(note = "Oldtimer, age > 115 years")


d1 %>%
  filter(is.na(as.integer(emp_code))) %>%
  select(kthid, emp_code, firstname, lastname)

d1 %>% mutate(
  ebeg = ymd(emp_beg),
  eend = ymd(emp_end),
  emod = ymd(emp_lastmod)
) %>%
  filter(ebeg <= Sys.Date(), eend <= Sys.Date(), emod <= Sys.Date()) %>%
  select(kthid, ebeg, eend, emod) %>%
  group_by(kthid) %>%
  count(kthid) %>%
  arrange(desc(n))

d1 %>% mutate(
  ebeg = ymd(emp_beg),
  eend = ymd(emp_end),
  emod = ymd(emp_lastmod)
) %>%
  filter(ebeg <= Sys.Date(), eend <= Sys.Date(), emod <= Sys.Date()) %>%
  filter(kthid == "u1n04lwv") %>%
  arrange(desc(ebeg, eend, emod)) %>%
  mutate(age = year(Sys.Date()) - yob) %>%
  select(unit_abbr, emp_code, emp_desc, emp_nr, ebeg, eend, emod, age)

#a) nuvarande anställning dvs "Är personen i nuläget anställd? Har personen publikationer som ska hänföras till KTH i dagsläget?"
#b) anställningsperiod dvs "När började anställningen på KTH? När avslutades den, ifall den är avslutad"
#c) anställningskategori dvs "Rör det sig om en industridoktorand, gästforskare, stipendiat, adjungerad professor?"

hr_read_csv(latest) %>%
  filter(gender != "M" & gender != "K") %>%
  pull(kthid) %>%
  head(1)

