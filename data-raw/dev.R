# devtools::install_github("r-lib/desc")
library(usethis)

use_data_raw()
use_readme_rmd()
use_mit_license("CC0")

library(devtools)
library(desc)

unlink("DESCRIPTION")
my_desc <- description$new("!new")
my_desc$set("Package", "institutions")
my_desc$set("Authors@R", "person('Markus', 'Skyttner', email = 'markussk@kth.se', role = c('cre', 'aut'))")

my_desc$del("Maintainer")

my_desc$set_version("0.0.0.9000")

my_desc$set(Title = "Swedish Research Publications Data")
my_desc$set(Description = "DiVA is a Swedish research publication database with national and institutional portals available. This R package downloads and makes data available to use from R.")
my_desc$set("URL", "https://github.com/KTH-Library/diva")
my_desc$set("BugReports", "https://github.com/KTH-Library/diva/issues")
my_desc$set("License", "MIT")
my_desc$write(file = "DESCRIPTION")

# use_mit_license(name = "Markus Skyttner")
# use_code_of_conduct()
use_news_md()
use_lifecycle_badge("Experimental")

use_tidy_description()

use_testthat()
use_roxygen_md()
use_pkgdown()

#

styler::style_pkg()
lintr::lint_package()

use_package("curl")
use_package("purrr")
use_package("jsonlite")
use_package("progress")
use_package("dplyr")
use_package("httr")
use_package("tibble")
use_package("rlang")
use_package("stringi")
use_package("rappdirs")
use_package("readr")
use_package("xml2")

document()
test()
check()
pkgdown::build_site()

# test building of vignettes without any downloaded data present
build_vignettes()
library(usethis)
use_github_action("check-standard")
use_package("rcrypt")
use_package("ndjson")
use_package("jqr")
use_package("dataReporter")
