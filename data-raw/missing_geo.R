library(bibliotools)
library(dplyr)
library(aws.s3)
library(arrow)

# Update address data from Bibmet
update_copub_data()

# Get latest ROR data from Zenodo
ror_data_url <- "https://zenodo.org/api/records/?communities=ror-data&sort=mostrecent"
ror_file_url <- latest_ror_url(ror_data_url)
ror_data <- tidy_rorzip(ror_file_url)

# Create lookup table for ROR -> geocodes and put in copub-data bucket
update_ror_lookup_minio(ror_data)

# Check which KTH copublication addresses are missing geocodes
addresses <- s3read_using(FUN = read_parquet, object = 'BestResAddr_KTH.parquet', bucket = 'copub-data') %>%
  filter(!is.na(Name_eng))
ror_lookup <- get_ror_lookup()
org_country <- get_geocountry()
org_city <- get_geocity()

addresses_noror <- addresses %>%
  left_join(ror_lookup, by = "ror_id") %>%
  filter(is.na(lat)) %>%
  select(names(addresses))

addresses_nogeo <- addresses_noror %>%
  anti_join(org_country, by = c("Name_eng", "Country_name")) %>%
  anti_join(org_city, by = c("Name_eng", "Country_name", "City"))

orgs_nogeo <- addresses_nogeo %>%
  mutate(ror_id = trimws(ror_id)) %>%
  group_by(Unified_org_id, Name_eng, Country_name, ror_id) %>%
  summarise(p = n_distinct(UT), .groups = "drop") %>%
  arrange(-p)

s3write_using(x = orgs_nogeo, FUN = write.csv, row.names = FALSE, na = "", object = 'orgs_nogeo.csv', bucket = 'copub-data')
