#' List contents of minio S3 bucket
#'
#' Before using this function, set up credentials for accessing minio using S3
#'
#' @details
#' For example use environment variables like these:
#'   #AWS_ACCESS_KEY_ID=supersecret
#'   #AWS_SECRET_ACCESS_KEY=supersecret
#'   #AWS_S3_ENDPOINT=lib.kth.se
#'   #AWS_DEFAULT_REGION=data
#' This can be achieved by adding these lines to the ~/.Renviron file.
#'   #file.edit("~/.Renviron")
#'   #readRenviron("~/.Renviron")
#' @param bucket name of bucket to list
#' @importFrom aws.s3 get_bucket_df
#' @importFrom dplyr arrange
#' @export
hr_ls <- function(bucket = "hrplus") {

  # these are available buckets/datasets
  #bucket_list_df(use_https = FALSE)

  # these are the files in the hrplus bucket
  aws.s3::get_bucket_df(bucket, use_https = TRUE) %>%
    arrange(desc(LastModified))
}

#' List contents of minio S3 bucket
#' @param bucket name of bucket to list
#' @importFrom `aws.s3` get_object
#' @importFrom utils head
#' @export
hr_latest <- function(bucket = "hrplus") {

  my_file <- hr_ls() %>% head(1) %>% pull("Key")
  f1 <- get_object(my_file, bucket, use_https = FALSE)
  hr_read_csv(f1)

}

#' Parse and read the HR data in CSV format
#' @param file path to file
#' @importFrom readr read_csv cols parse_double locale
#' @importFrom dplyr rename mutate
#' @import lubridate
#' @export
hr_read_csv <- function(file) {

  cs <- cols(
    .default = col_character(),
    `FÖDELSEÅR` = col_integer(),
    DATUM_NUV_BEF = col_integer(),
    BEF_TOM = col_integer(),
    SYSS_GRAD = col_character()
  )

  # parse and remap colnames; use lowersnakecase field names
  hr <- readr::read_csv(file = file, col_types = cs, quote = "\"") %>%
    rename(
      kthid = "KTHID",
      yob = "FÖDELSEÅR",
      unit_abbr = "ORG_NR",
      unit_name = "ORG_NAMN",
      firstname = "FÖRNAMN",
      lastname = "EFTERNAMN",
      gender = "MAN/KVINNA",
      emp_code = "TJ_BEN_KOD",
      emp_desc = "TJ_BE_TEXT",
      emp_nr = "BEF_NR",
      emp_beg = "BEF_FROM",
      emp_end = "BEF_TOM",
      emp_lastmod = "DATUM_NUV_BEF",
      emp_degree = "SYSS_GRAD",
      scb_topic = "ÄMNESKOD"
    )

  # data types parsing
  hr %>%
    mutate(emp_beg = lubridate::ymd(as.character(emp_beg))) %>%
    mutate(emp_lastmod = lubridate::ymd(as.character(emp_lastmod))) %>%
    mutate(emp_end = lubridate::ymd(as.character(emp_end))) %>%
    mutate(emp_degree = parse_double(emp_degree, locale = locale(decimal_mark = ",")))

}
