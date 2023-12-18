library(bibliotools)
library(dplyr)
library(odbc)
library(aws.s3)

# fetch pairs of KTH_id/ORCID_iD from Diva_Author

q <-
  "SELECT ORCID_iD, KTH_id, count(*) AS p
  FROM Diva_author
  WHERE ORCID_iD IS NOT NULL AND KTH_id LIKE 'u1%'
  GROUP BY ORCID_id, KTH_id"

con <- con_bib_mssql()

orcid_kthid <- dbGetQuery(con, q)

# remove ORCID_iD:s with more than 1 KTH_id
keep <-
  orcid_kthid |>
  group_by(ORCID_iD) |>
  count() |>
  filter(n == 1) |>
  select(ORCID_iD)

orcid_kthid <-
  keep |>
  inner_join(orcid_kthid, by = "ORCID_iD") |>
  as_tibble()

# write to S3 storage
s3write_using(
  orcid_kthid,
  object = "orcid_kthid_diva.csv",
  bucket = "kthcorpus",
  FUN = write.csv,
  row.names = FALSE,
  na = "",
  fileEncoding = "UTF-8",
  )

# -----

# Fetching similar data from DiVA directly and comparing

# DiVA authors (name-orcid-kthid-PID)
aut <-
  kth_diva_authors(use_cache = TRUE) |>
  select(name, orcid, kthid, PID) |>
  filter(!is.na(kthid), !is.na(orcid), grepl("^u1.{6}$", kthid))


# orcids with one or more kthids
diva_orcid_kthids <-
  aut |> group_by(orcid) |>
  summarise(.groups = "drop_last",
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

# compare

x <- upload |> select(1:2) |> arrange(desc(kthid), desc(orcid))
y <- orcid_kthid |> select(orcid = "ORCID_iD", kthid = "KTH_id") |> arrange(desc(kthid), desc(orcid))

waldo::compare(x, y)

# click @@ column heading (cycle to dark blue rows) to see difference
daff::diff_data(x, y) |> daff::render_diff()

# differences in DiVA (x) versus BibMet (y)
x |> left_join(y, by = "kthid") |> filter(orcid.x != orcid.y)

setdiff(x$kthid, y$kthid) # 55 kthid in DiVA are not in BibMet
setdiff(y$kthid, x$kthid) # 391 kthid in BibMet are not in DiVA
intersect(x$kthid, y$kthid) |> length() # 5115 are the same

setdiff(x$orcid, y$orcid) # 61 orcid in DiVA are not in BibMet
setdiff(y$orcid, x$orcid) # 397 orcid in BibMet are not in DiVA
intersect(x$orcid, y$orcid) |> length() # 5107 are the same

# pairs in DiVA but not in BibMet
x |> anti_join(y, by = c("kthid", "orcid"))

# pairs in BibMet but not in DiVA
y |> anti_join(x, by = c("kthid", "orcid"))
