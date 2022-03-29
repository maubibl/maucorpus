
do <-
  diva_organisations() %>%  filter(! (unit_en %in% c(
  "Centres",
  "Biomechanics", "Chemical Engineering",
  "Swedish Center for Biomimetic Fiber Engineering, BioMime")
  | is_closed))


ad <-
  bibliomatrix::abm_divisions() %>% distinct(desc, id)

fuzzy_match <- function(a, b, c, method = "lv") {
  ids <- stringdist::amatch(toupper(a), toupper(b), method = method)
  tibble(id = ids, slug = c[ids], candidate = b[ids])
}

en <-
  do %>% bind_cols(
    fuzzy_match(do$unit_en, ad$desc, ad$id, method = "jw")
  ) %>%
  filter(!is.na(slug)) %>%
  select(unit_en, slug, candidate) %>%
  distinct()

sv <-
  do %>% bind_cols(
    fuzzy_match(do$unit_sv, ad$desc, ad$id, method = "jw")
  ) %>%
  filter(!is.na(slug)) %>%
  select(unit_sv, slug, candidate) %>%
  distinct()

candidates <-
  bind_rows(sv %>% select(unit = unit_sv, slug), en %>% select(unit = unit_en, slug)) %>% distinct()

candidates

out <-
  do %>%
  left_join(en, by = "unit_en") %>%
  left_join(sv, by = "unit_sv") %>%
  mutate(slug = ifelse(is.na(slug.x), slug.y, slug.x)) %>%
  mutate(DESC = ifelse(is.na(candidate.x), candidate.y, candidate.x)) %>%
  select(!any_of(c("slug.x", "slug.y", "candidate.x", "candidate.y")))

writexl::write_xlsx(out, "~/diva_orgs.xlsx")

View(out)


# TODO: check why some unit names reoccur!

# > do %>% filter(!is_closed) %>% group_by(unit_en) %>% count(unit_en) %>% arrange(desc(n))
# # A tibble: 249 × 2
# # Groups:   unit_en [249]
# unit_en                                                            n
# <chr>                                                          <int>
#   1 Centres                                                           11
# 2 Biomechanics                                                       2
# 3 Chemical Engineering                                               2
# 4 Swedish Center for Biomimetic Fiber Engineering, BioMime           2
# 5 ACCESS Linnaeus Centre                                             1
# 6 Accounting, finance, economics and organization (AFEO)             1
# 7 Affinity Proteomics                                                1
# 8 Albanova VinnExcellence Center for Protein Technology, ProNova     1
# 9 Alfvén Laboratory Centre for Space and Fusion Plasma Physics       1
# 10 Applied Electrochemistry                                           1
# # … with 239 more rows
# > do %>% filter(!is_closed) %>% filter(unit_en == "Biomechanics")
# # A tibble: 2 × 9
# orgid p_orgid n_diva_pubs level unit_en      unit_sv    is_closed closed_date url
# <int>   <int>       <dbl> <int> <chr>        <chr>      <lgl>     <chr>       <chr>
#   1 882671  882658          16     4 Biomechanics Biomekanik FALSE     NA          https://kth.diva-portal.org/smash/resultList.jsf?dswid=9113&…
# 2 882670  882657           1     4 Biomechanics Biomekanik FALSE     NA          https://kth.diva-portal.org/smash/resultList.jsf?dswid=9113&…
# > do %>% filter(orgid %in% c(882658, 882657))
# # A tibble: 2 × 9
# orgid p_orgid n_diva_pubs level unit_en                                   unit_sv                               is_closed closed_date url
# <int>   <int>       <dbl> <int> <chr>                                     <chr>                                 <lgl>     <chr>       <chr>
#   1 882658  882656         359     3 Fluid Mechanics and Engineering Acoustics Strömningsmekanik och Teknisk Akustik FALSE     NA          http…
# 2 882657  882656         507     3 Vehicle Engineering and Solid Mechanics   Farkostteknik och Solidmekanik        FALSE     NA          http…
# > do %>% filter(!is_closed) %>% filter(unit_en == "Swedish Center for Biomimetic Fiber Engineering, BioMime")
# # A tibble: 2 × 9
# orgid p_orgid n_diva_pubs level unit_en                                                  unit_sv                 is_closed closed_date url
# <int>   <int>       <dbl> <int> <chr>                                                    <chr>                   <lgl>     <chr>       <chr>
#   1  5917    5914          13     3 Swedish Center for Biomimetic Fiber Engineering, BioMime Strategiskt Centrum fö… FALSE     NA          http…
# 2  5953    5948           2     3 Swedish Center for Biomimetic Fiber Engineering, BioMime Strategiskt Centrum fö… FALSE     NA          http…
# > do %>% filter(orgid %in% c(5914, 5948))
# # A tibble: 2 × 9
# orgid p_orgid n_diva_pubs level unit_en unit_sv is_closed closed_date url
# <int>   <int>       <dbl> <int> <chr>   <chr>   <lgl>     <chr>       <chr>
#   1  5914    5903         283     2 Centres Centra  FALSE     NA          https://kth.diva-portal.org/smash/resultList.jsf?dswid=9113&searchTyp…
# 2  5948    5923         856     2 Centres Centra  FALSE     NA          https://kth.diva-portal.org/smash/resultList.jsf?dswid=9113&searchTyp…
# > do %>% filter(!is_closed) %>% filter(unit_en == "Chemical Engineering")
# # A tibble: 2 × 9
# orgid p_orgid n_diva_pubs level unit_en              unit_sv              is_closed closed_date url
# <int>   <int>       <dbl> <int> <chr>                <chr>                <lgl>     <chr>       <chr>
#   1 879314  879224         623     2 Chemical Engineering Kemiteknik           FALSE     NA          https://kth.diva-portal.org/smash/resultLi…
# 2 879327  879314          22     3 Chemical Engineering Kemisk apparatteknik FALSE     NA          https://kth.diva-portal.org/smash/resultLi…
# > do %>% filter(orgid %in% c(879224, 879314))
# # A tibble: 2 × 9
# orgid p_orgid n_diva_pubs level unit_en                                                                 unit_sv is_closed closed_date url
# <int>   <int>       <dbl> <int> <chr>                                                                   <chr>   <lgl>     <chr>       <chr>
#   1 879224     177        5766     1 School of Engineering Sciences in Chemistry, Biotechnology and Health … Skolan… FALSE     NA          http…
# 2 879314  879224         623     2 Chemical Engineering                                                    Kemite… FALSE     NA          http…

chip_orgs <-
  readxl::read_xlsx("data-raw/DiVA_177_2021-12-13-with-acronyms-v2-20220105.xlsx")

cm <-
  chip_orgs %>%
  select(orgid  = organisation_id, slug = organisation_code) %>%
  filter(!is.na(slug))

extras <-
  candidates %>%
  filter(slug %in% setdiff(candidates$slug, cm$slug))


missing_slugs <-
  do %>%
  left_join(cm) %>%
  left_join(extras) %>%
  filter(is.na(slug)) %>%
  distinct() %>%
  select(orgid, unit_en, unit_sv)

guess_slugs <- function(orgids) {

  kda <-
    kth_diva_authors()


  suggest_slug <- function(myorgid) {

    top3a <-
      kda %>%
      filter(orgid == myorgid) %>%
      group_by(kthid) %>%
      count() %>%
      arrange(desc(n)) %>%
      head(5) %>%
      pull(kthid)

    lookup_researcher_slug <- function(x) {
      res <- suppressMessages(kthapi::kth_profile(kthid = x))
      res$content$worksFor$items %>% select(path, name)
    }

    lookup <- purrr::possibly(.f = lookup_researcher_slug, otherwise = data.frame())

    top3a %>%
      map_df(lookup) %>%
      bind_cols(orgid = myorgid) %>%
      distinct()

  }

  orgids %>%
    map_df(suggest_slug)

}

guesses <-
  missing_slugs$orgid %>%
  guess_slugs() %>%
  left_join(missing_slugs) %>%
  as_tibble()

# guesses %>%
#   mutate(level = stringr::str_count(path, "/") + 1) %>%
#   group_by(orgid) %>%
#   filter(level == max(level)) %>%
#   ungroup() %>%
#   View()

#readr::write_csv(guesses, "data-raw/slugs_for_units.csv")

# 34 orgids still missing (not mapped to "slugs")
readr::read_csv("data-raw/slugs_for_units.csv") %>%
  View()

#system("mc cp data-raw/slugs_for_units.csv kthb/kthcorpus")



library(kthcorpus)
library(kthapi)

hrp <- hr_plus()

hrp_extra <- function() {

  hr <-
    hrp %>%
    left_join(ss_employment_title, by = c("emp_code" = "id")) %>%
    mutate(is_uf_ta = is_uf_ta == "UF")

  hr %>% left_join(research_areas, by = c("scb_topic" = "id")) %>%
    rename(
      "scb_topic_swe" = "swe",
      "scb_topic_eng" = "eng",
      "scb_topic_level" = "level"
    )

}

hrp_current <-
  hrp_extra() %>%
  filter(emp_beg <= lubridate::today(), emp_end > lubridate::ymd(190101)) %>%
  group_by(kthid) %>%
  summarize(
    across(c("emp_lastmod", "emp_end", "emp_beg"), max)
  ) %>%
  arrange(desc(emp_lastmod), desc(emp_end), desc(emp_beg)) %>%
  inner_join(hrp) %>%
  unique()

ug <- kthapi::ug_orcid_kthid_unit()

# units given for a kthid via UG
ug$kthid_with_unit

hrp_current %>%
  left_join(ug$kthid_with_unit %>% select(kthid = ugKthid, unit), by = "kthid") %>%
  group_by(unit) %>%
  summarize(n = n_distinct(kthid)) %>%
  arrange(desc(n))

# appears to have different unit and unit_abbr
hrp_current %>%
  left_join(ug$kthid_with_unit %>% select(kthid = ugKthid, unit), by = "kthid") %>%
  filter(unit != unit_abbr, !is.na(unit)) %>%
  select(kthid, unit, unit_abbr)


kda <-
  kthcorpus::kth_diva_authors()

# check "valid" combos of kthid and orcid in DiVA

kda %>% group_by(kthid, orcid) %>% count(orcid)

# check "valid" combos of kthid and name in DiVA
kda %>% group_by(kthid, name) %>%
  filter(!is.na(kthid)) %>%
  group_by(kthid, name) %>%
  count(name) %>%
  arrange(desc(n))


