#' Mappings used for HR data
#'
#' This meta data describes mapping for the HR data used when making the extract
#' @importFrom readr read_delim locale
#' @importFrom
#' @export
#' Mappings used for HR data
#'
#' This meta data describes mapping for the HR data used when making the extract
#' @importFrom readr read_delim
#' @export
hr_mapping <-
  readr::read_delim(delim = "|", locale = locale(encoding = "UTF-8"), file =
"colname|export|name|table|field
kthid|KTHID|Kth-id (U1nr)|kthid|kth004
yob|FÖDELSEÅR|Födelseår|p_person|p_p11000 fyra första tecken
unit_abbr|ORG_NR|Orgenhet nr|p_befreg|p_k41502
unit_name|ORG_NAMN|Orgenhet namn|allm010|allm014
firstname|FÖRNAMN|Förnamn|p-person|p_p10101
lastname|EFTERNAMN|Efternamn|p-person|p_p10102
gender|MAN/KVINNA|Kön|p_person|P_p41801
emp_code|TJ_BEN_KOD|Tjänstebenämningskod|p_befreg|P_k40400
emp_desc|TJ_BE_TEXT|Benämning|p_befreg|P_k12200
emp_nr|BEF_NR|Bef.nr|p_befreg|P_kxxx93
emp_beg|BEF_FROM|Bef from|p_befreg|P_k12000
emp_end|BEF_TOM|Bef tom|p_befreg|P_k12100
emp_lastmod|DATUM_NUV_BEF|Datum nuv bef|p_befreg|P_k12300
emp_degree|SYSS_GRAD|Syss.grad|p_befreg|P_k13200
scb_topic|ÄMNESKOD|Ämneskod|p_befreg|P_k55001"
  )

use_data(hr_mapping, overwrite = TRUE)


hr <- hr_latest()

# is latest modification data reasonable?
hr_latest() %>% collect %>% summarize(elm = max(emp_lastmod, na.rm = TRUE))

# no gender given? are all genders M or K?
hr %>% filter(!gender %in% c("M", "K")) %>% View()
hr %>% filter(yob == 1900) %>% View()
# yob has large span, for example 1900 -> check ages
# emp_lastmod has 200 NAs -> why are these missing?
summary(hr)

check_research_area <- function(data) {

  ra <-
    research_areas %>%
    mutate(id = as.character(id)) %>%
    rename(research_area = eng, scb_topic = id) %>%
    select(scb_topic, research_area)

  topics <- data %>%
    left_join(ra) %>%
    filter(is.na(research_area), !is.na(scb_topic)) %>%
    count(scb_topic) %>%
    arrange(desc(n))

  message("Topics for research area not recognized at Statistics Sweden:")

  print(topics)

}


# what does ÄMNESKOD 999 mean? What does 10 or 100 mean?
# these codes do not seem to exist in UKÄ?
check_research_area(hr)

hr %>% filter(grepl("athanasios", tolower(firstname)))

# list of staff which has a employment code for a title which is marked
# as "Educational or Research" by Statistics Sweden
researchers <-
  hr %>%
  left_join(ss_employment_title, by = c("emp_code" = "id")) %>%
  filter(is_uf_ta == "UF")

View(researchers)

# inspect employee descriptions
hr %>% count(emp_desc) %>% arrange(desc(n))

hr %>% filter(grepl("ARVODIST", emp_desc))

# presence of "industridoktorand"?
hr %>% distinct(kthid, emp_desc) %>% count(emp_desc) %>%
  arrange(desc(n)) %>%
  filter(grepl("INDUSTRI", emp_desc))

# note: more than 115 years old
hr %>%
  mutate(age = lubridate::year(Sys.Date()) - yob) %>%
  filter(age >= 115) %>%
  mutate(note = "Oldtimer, age > 115 years")

# missing employee code
hr %>%
  filter(is.na(as.integer(emp_code))) %>%
  select(kthid, emp_code, firstname, lastname)

# records of researchers who have ended employment (?)
hr %>%
  filter(emp_beg <= Sys.Date(), emp_end <= Sys.Date(), emp_lastmod <= Sys.Date()) %>%
  select(kthid, emp_beg, emp_end, emp_lastmod) %>%
  group_by(kthid) %>%
  count(kthid) %>%
  arrange(desc(n))

hr %>%
  filter(kthid == "u1n04lwv") %>%
  arrange(desc(emp_beg, emp_end, emp_lastmod)) %>%
  mutate(age = lubridate::year(Sys.Date()) - yob) %>%
  select(kthid, unit_abbr, emp_code, emp_desc, emp_nr, emp_beg, emp_end, emp_lastmod, age)

esquisse::esquisser(data = hr, viewer = "browser")

# https://getpocket.com/explore/item/the-history-of-the-pivot-table-the-spreadsheet-s-most-powerful-tool?utm_source=pocket-newtab

#TODO:
#a) nuvarande anställning dvs "Är personen i nuläget anställd? Har personen publikationer som ska hänföras till KTH i dagsläget?"
#b) anställningsperiod dvs "När började anställningen på KTH? När avslutades den, ifall den är avslutad"
#c) anställningskategori dvs "Rör det sig om en industridoktorand, gästforskare, stipendiat, adjungerad professor?"

##########
### These issues with the data format have now been fixed!

# problems(hr) # approx 21 problems ie 0.1 % of records appear ambiguous (due to CSV formatting issues)
#
# problems(hr) %>%
#   count(expected, actual)
#
# header <- readLines(rawConnection(f1))[1] %>% strsplit(split = ",") %>% unlist()
#
# # skip header
# lines <- readLines(rawConnection(f1))[-1]
# # three fields (last missing) after the three date fields
# lines[(problems(hr)$row)] %>% head()
# # two fields (last missing) after the three date fields
# lines[-(problems(hr)$row)] %>% head()
#
# hr %>% slice(problems(hr)$row) %>% View()




############

library(tibble)
library(dplyr)
library(purrr)
library(stringr)

library(magick)

hr_mapping <- image_read("data-raw/hr-explanation.jpg")

library(tesseract)
#tesseract_download(lang = "eng")

p1 <-
  image_ocr(hr_mapping, language = "swe", ) %>%
  str_split("\n") %>% unlist %>%

cat(p1)



#############





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


