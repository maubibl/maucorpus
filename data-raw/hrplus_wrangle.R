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
scb_topic|ÄMNESKOD|Ämneskod|p_befreg|P_k55001
is_public|VISAS_HEMSIDA|Visas publikt på KTH-webben||
emp_title_swe|FUNKTION_SV|Forskarens egen angivna funktionsbeskrivning||
emp_title_eng|FUNKTION_ENG|Forskarens egen angivna funktionsbeskrivning||
"
)

use_data(hr_mapping, overwrite = TRUE)

library(dplyr)

hr <- hr_plus()

# is latest modification data reasonable?
shr <-
  hr %>% collect %>% group_by(kthid) %>%
  summarize(
    elm = max(emp_lastmod),
    ebeg = min(emp_beg),
    eend = max(emp_end),
    duration = eend - ebeg,
    ttl = eend - Sys.Date(),
    n = length(kthid)
    ) %>%
  arrange(desc(n), desc(elm))

# forskargrupp? vad är detta?
shr %>% filter(ebeg < lubridate::ymd("1921-01-01"))
hr %>% filter(kthid == "u17tp77x")


# several have end date set 6 years into the future
shr %>% filter(eend > Sys.Date(), eend < lubridate::ymd("2999-12-31")) %>%
  arrange(desc(eend))

# would these be new phd students?
shr %>% filter(
  ttl > lubridate::duration(5, units="years"),
  ttl < lubridate::duration(100, unit = "years")
)

summary(shr)




# modification date is same as beginning date?
# this person seems to have ended employment
shr %>% filter(elm == lubridate::ymd("1974-07-31"))

# 23 persons have last modification date set in the future?
# 2 have ended employment already?
shr %>% filter(elm > Sys.Date()) %>% arrange(ttl)



shr %>%
  filter(
    ebeg > lubridate::ymd("2013-01-01")
  ) %>%
  mutate(is_active = ttl > 0) %>%
  filter(is_active) %>%
  summary()


warnings()
hr %>%
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


hr %>% left_join(research_areas, by = c(scb_topic = "id"))
hr %>% filter(grepl("athanasios", tolower(firstname)))

# list of staff which has a employment code for a title which is marked
# as "Educational or Research" by Statistics Sweden
researchers <-
  hr %>%
  left_join(ss_employment_title, by = c("emp_code" = "id")) %>%
  mutate(is_uf_ta = is_uf_ta == "UF") %>%
  filter(is_uf_ta)

researchers %>%
  count(emp_desc)

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


# namn, casing

proper_case <- function(x)
  stringi::stri_trim_both(x) %>%
  stringi::stri_trans_general("Title")

researchers_pc <-
  hr %>%
  mutate(across(c("firstname", "lastname"), proper_case)) %>%
  mutate(fullname = paste0(lastname, " ", firstname)) %>%
  distinct(fullname)

# special cases? could be cleaned

# 18 deceased
researchers_pc %>%
  filter(grepl("Dödsbo", fullname))

# 7 "Af"
researchers_pc %>%
  filter(grepl("Af ", fullname))

# 40 "Von"
researchers_pc %>%
  filter(grepl("Von ", fullname))

# 1 "Von Der"
researchers_pc %>%
  filter(grepl("Von Der ", fullname))

# 119 "De "
researchers_pc %>%
  filter(grepl("De ", fullname))

# 8 with double spaces
researchers_pc %>%
  filter(grepl("\\s{2,}", fullname, perl = TRUE))

# 16 with . or , in the name
researchers_pc %>%
  filter(grepl("\\.|,", fullname)) %>%
  View()

# 3176 with non-ascii characters
researchers_pc %>%
  filter(grepl("[[:cntrl:]]", stringi::stri_enc_toascii(fullname))) %>%
  View()

researchers_pc %>%
  pull(fullname) %>%
  humaniformat::parse_names() %>%
  as_tibble() %>%
  count(middle_name) %>%
  arrange(desc(n)) %>%
  View()

library(dplyr)
library(lubridate)

hr_plus_extra <- function() {

  hr <- hr_plus()

  hr <- hr %>%
    left_join(ss_employment_title, by = c("emp_code" = "id")) %>%
    mutate(is_uf_ta = is_uf_ta == "UF")

  hr %>% left_join(research_areas, by = c("scb_topic" = "id")) %>%
    rename(
      "scb_topic_swe" = "swe",
      "scb_topic_eng" = "eng",
      "scb_topic_level" = "level"
    )

  # hr %>%
  #   filter(emp_beg <= lubridate::today(), emp_end > lubridate::today()) %>%
  #   group_by(kthid) %>%
  #   summarize(
  #     across(c("emp_lastmod", "emp_end", "emp_beg"), max)
  #   ) %>%
  #   arrange(desc(emp_lastmod, emp_end, emp_beg)) %>%
  #   inner_join(hr) %>%
  #   # some rows have duplicates ...
  #   # filter(kthid == "u1fzcxlt") %>%
  #   #select(-c(starts_with("emp"), is_public, scb_topic)) %>%
  #   unique()

}

# current employees
current <-
  hr_plus_extra() %>% View()

current %>%
  count(kthid) %>%
  arrange(desc(n))

# employees with two different current orglocations at KTH
current %>%
  count(kthid) %>%
  filter(n > 1) %>%
  inner_join(hr)

emp_periods <-
  hr %>% group_by(kthid) %>% summarize(
    across(c("emp_lastmod", "emp_end", "emp_beg"), min)
  ) %>%
  rename_with(function(x) paste0("min_", x), .cols = -c("kthid")) %>%
  left_join(
    hr %>% group_by(kthid) %>% summarize(
      across(c("emp_lastmod", "emp_end", "emp_beg"), max)
    ) %>%
      rename_with(function(x) paste0("max_", x), .cols = -c("kthid"))
  )

ts_beg <- lubridate::as_date("2017-01-17")
ts_end <- lubridate::as_date("2018-01-01")

emp_periods %>%
  filter(min_emp_beg <= ts_beg & max_emp_end >= ts_end) %>%
  arrange(desc(max_emp_end), desc(min_emp_end))

current

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

library(kthcorpus)
library(daff)

latest <- hr_plus()
earlier <- hr_plus(offset = 1)

render_diff(diff_data(latest, earlier))

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


