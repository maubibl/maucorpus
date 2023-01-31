library(bibliotools)
library(dplyr)
library(odbc)
library(aws.s3)

## Fetch pairs of KTH_id/ORCID_iD from Diva_Author

q <- "SELECT ORCID_iD, KTH_id, count(*) AS p
FROM Diva_author
WHERE ORCID_iD IS NOT NULL AND KTH_id LIKE 'u1%'
GROUP BY ORCID_id, KTH_id"

con <- con_bib_mssql()

orcid_kthid <- dbGetQuery(con, q)

## Remove ORCID_iD:s with more than 1 KTH_id
keep <- orcid_kthid %>% group_by(ORCID_iD) %>% count() %>% filter(n == 1) %>% select(ORCID_iD)
orcid_kthid <- keep %>% inner_join(orcid_kthid, by = "ORCID_iD")

## Write to S3 storage
s3write_using(orcid_kthid, FUN = write.csv, row.names = FALSE, fileEncoding = "UTF-8", object = "orcid_kthid_diva.csv", bucket = "kthcorpus")

# -----

# Fetching similar data from DiVA directly and comparing

# DiVA authors (name-orcid-kthid-PID)
aut <-
  kth_diva_authors() |> select(name, orcid, kthid, PID) |>
  filter(!is.na(kthid), !is.na(orcid), grepl("^u1.{6}$", kthid))

# orcids with one or more kthids
diva_orcid_kthids <-
  aut |> group_by(orcid) |>
  summarize(.groups = "drop_last",
    kthid = unique(kthid),
    author = paste(collapse = "|", unique(trimws(name))),
    n_kthid = n_distinct(kthid),
    n_pubs = n_distinct(PID)
  ) |>
  arrange(desc(n_kthid), desc(n_pubs), orcid)

# kthids with one or more orcids
diva_kthid_orcids <-
  aut |> group_by(kthid) |>
  summarize(.groups = "drop_last",
    orcid = unique(orcid),
    author = paste(collapse = "|", unique(trimws(name))),
    n_orcid = n_distinct(orcid),
    n_pubs = n_distinct(PID)
  ) |>
  arrange(desc(n_orcid), desc(n_pubs), kthid)

# NB: if we look for kthids with more than 1 orcids, we get 397 rows
#diva_kthid_orcids |> filter(n_orcid > 1)

# NB: if we look for orcids with more than 1 kthids, we get 114 rows
#diva_orcid_kthids |> filter(n_kthid > 1)

# kthid and orcid pairs with one-to-one relationships
upload <-
    diva_kthid_orcids |> filter(n_orcid == 1) |>
    inner_join(
      diva_orcid_kthids |> filter(n_kthid == 1)
    ) |>
    select(orcid, kthid, n_pubs)

# upload to bucket "kthcorpus"
readr::write_csv(upload, "/tmp/diva_kthid_orcid.csv")
diva_upload_s3(path = "/tmp/diva_kthid_orcid.csv", "diva_kthid_orcid.csv")

bibmet_kthid_orcids <-
  minio_get("orcid_kthid_diva.csv", bucket = "kthcorpus") |>
  readr::read_csv() |>
  rename(orcid = ORCID_iD, kthid = KTH_id, n_pubs = p)

# click @@ column heading (cycle to dark blue rows) to see difference
daff::render_diff(daff::diff_data(
  bibmet_kthid_orcids |> select(-n_pubs),
  upload |> select(-n_pubs)
))
