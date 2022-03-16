# kthcorpus

## kthcorpus 0.2.0

* Added function to scrape DiVA portal for units/organisations (en/sv)
* Dockerfile and Makefile moved to inst/docker and GHA adjusted for this
* Download of persons data from DiVA to remove need for time consuming name parsing
* Functions to upload and download from S3 buckets
* Bootstrap v 5 w latest DT and vertical scrolling fix
* Added checks for missing kthid, invalid ISSN, ISBN, orcid

## kthcorpus 0.1.0

* Refactored functions to remove non-ascii variable names due to Windows build issue, see https://developer.r-project.org/Blog/public/2020/05/02/utf-8-support-on-windows/
* Added GitHub Actions CI workflow
* Added function to retrieve SwePub checks data in tsv format
* Renamed the package to "kthcorpus" and put it on GitHub
* Added function to allow retrieval of data from S3-enabled object storage backend, given appropriate credentials present in local .Renviron
* Added neo4j bulk extract function to allow export of data for further processing using neo4j
* Added UKÄ reference data

## kthcorpus 0.0.0.9000

* Added a `NEWS.md` file to track changes to the package.
