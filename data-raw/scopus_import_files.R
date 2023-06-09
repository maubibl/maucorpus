# get latest DiVA-data from minio (clear local cache)
diva_refresh()

scopus <- scopus_from_minio()
kthid_orcid_lookup <- kthid_orcid()
sids <- scopus$publications |> filter(subtype == "cp") |> pull(`dc:identifier`)

keys <-
  scopus$publications |>
  group_by(`prism:aggregationType`, subtypeDescription) |>
  count() |> arrange(desc(n)) |>
  purrr::pmap_chr(.f = function(`prism:aggregationType`, subtypeDescription, ...) frag_genre2(`prism:aggregationType`, subtypeDescription) |> names())

scopus$publications |>
  group_by(`prism:aggregationType`, subtypeDescription) |>
  count() |> arrange(desc(n)) |>
  bind_cols(key = keys)

#    `prism:aggregationType` subtypeDescription     n key
#    <chr>                   <chr>              <int> <chr>
#  1 Journal                 Article              123 article
#  2 Conference Proceeding   Conference Paper      63 conferencePaperPublished
#  3 Journal                 Review                 9 review
#  4 Book                    Book Chapter           8 chapter
#  5 Journal                 Editorial              3 editorialMaterial
#  6 Book                    Editorial              2 collection
#  7 Journal                 Note                   2 articleErratum
#  8 Book                    Book                   1 book
#  9 Conference Proceeding   Editorial              1 conferenceProceedings
# 10 Journal                 Erratum                1 articleErratum

pubs <- kth_diva_pubs()

# We can see which of those in the Scopus batch that already have been imported in DiVA
# (matches on ScopusId)
already_imported <-
  pubs |> filter(ScopusId %in% scopus$publications$eid) |> select(PID, DOI, ScopusId)

# We now match on DOIs instead DiVA could add these ScopusIds (currently missing)
pids_with_doi_and_missing_scopusid <-
  pubs |>
  filter(DOI %in% na.omit(scopus$publications$`prism:doi`)) |>
  left_join(scopus$publications, by = c("DOI" = "prism:doi")) |>
  select(PID, DOI, ScopusId = eid)

# this file could be imported to DiVA (PID and ScopusID pairs)
# this is an update in DiVA of existing records
pids_with_doi_and_missing_scopusid |> select(-DOI) |>
  write_csv("~/temp/modz/scopusid_updates_to_diva_support.csv")

intersect(pids_with_doi_and_missing_scopusid$ScopusId, already_imported$ScopusId)

# these could be imported from Scopus, since not already imported
# this is creating new records in DiVA
import <-
  scopus$publications |> filter(
    # exclude from the Scopus import, because those are already imported
    !eid %in% pids_with_doi_and_missing_scopusid$ScopusId,
    !eid %in% already_imported$ScopusId
  )

sids_cp <-
  import |>
  filter(
#    `prism:aggregationType` == "Conference Proceeding",
    `subtypeDescription` == "Conference Paper") |>
  pull(`dc:identifier`)

sids_ar <-
  import |>
  filter(
#    `prism:aggregationType` == "Conference Proceeding",
    `subtypeDescription` == "Article") |>
  pull(`dc:identifier`)

sids_other <-
  import |> filter(!`dc:identifier` %in% c(sids_cp, sids_ar)) |>
  pull(`dc:identifier`)

my_mods_ar <- scopus_mods_crawl(sids = sids_ar, scopus = scopus, ko = kthid_orcid_lookup)
my_mods_cp <- scopus_mods_crawl(sids = sids_cp, scopus = scopus, ko = kthid_orcid_lookup)
my_mods_other <- scopus_mods_crawl(sids = sids_other, scopus = scopus, ko = kthid_orcid_lookup)

#system("mkdir -p ~/temp/modz")

my_mods_other |> create_diva_modscollection() |> write_file("~/temp/modz/scopus_other.xml")
my_mods_ar |> create_diva_modscollection() |> write_file("~/temp/modz/scopus_ar.xml")
my_mods_cp |> create_diva_modscollection() |> write_file("~/temp/modz/scopus_cp.xml")

system("firefox ~/temp/modz/scopus_cp.xml")
system("firefox ~/temp/modz/scopus_ar.xml")
system("firefox ~/temp/modz/scopus_other.xml")
