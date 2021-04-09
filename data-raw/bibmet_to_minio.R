library(aws.s3)
library(dplyr)
library(bibliomatrix)
library(arrow)
library(DBI)
library(readr)

### Needs environment variables:
#AWS_ACCESS_KEY_ID=
#AWS_SECRET_ACCESS_KEY=
#AWS_S3_ENDPOINT=lib.kth.se:9000
#AWS_DEFAULT_REGION=bibliometrics
#DBHOST=bibmet-prod.ug.kth.se
#BIBMET_USER=
#BIBMET_PASS=

# Get masterfile and address data to put on minio server
con <- con_bib()
m <- con %>% tbl("masterfile") %>% collect()
dbDisconnect(con)

con_bibmet <- dbConnect(
  odbc::odbc(),
  driver = "ODBC Driver 17 for SQL Server",
  Port = 1433,
  server = Sys.getenv("DBHOST"),
  database = "BIBMET",
  UID = Sys.getenv("BIBMET_USER"),
  PWD = Sys.getenv("BIBMET_PASS"),
  timeout = 30,
  encoding = "Windows-1252")

rasw <- con_bibmet %>% tbl("v_BestResAddrSwe") %>% collect()

q <- "SELECT ra.*
FROM (SELECT DISTINCT ISI AS UT FROM BIBMET.dbo.Diva WHERE ISI IS NOT NULL) diva_isi
INNER JOIN BIBMET.dbo.Document doc ON (doc.UT = diva_isi.UT)
INNER JOIN BIBMET.dbo.v_BestResAddr ra ON (ra.Doc_id = doc.Doc_id)"

rakth <- dbGetQuery(con_bibmet, q)

dbDisconnect(con_bibmet)

# Write to bibliometrics bucket
datum <- format(Sys.Date(), "%Y%m%d")
s3write_using(m, FUN = write_parquet,
              bucket = "bibliometrics",
              object = paste0("masterfile_", datum, ".parquet"),
              opts = list(use_https = FALSE))
s3write_using(rasw, FUN = write_parquet,
              bucket = "bibliometrics",
              object = paste0("BestResAddrSWE_", datum, ".parquet"),
              opts = list(use_https = FALSE))
s3write_using(rakth, FUN = write_parquet,
              bucket = "bibliometrics",
              object = paste0("BestResAddrKTH_", datum, ".parquet"),
              opts = list(use_https = FALSE))

# get_bucket("bibliometrics", use_https = FALSE)
