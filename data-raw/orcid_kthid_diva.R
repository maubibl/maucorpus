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
