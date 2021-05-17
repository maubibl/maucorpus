#' List files in an S3 bucket
#' @param bucket the bucket to use, default is "bibliometrics"
#' @importFrom aws.s3 get_bucket_df
#' @importFrom dplyr arrange
#' @export
minio_ls <- function(bucket = "bibliometrics")
  get_bucket_df(bucket, use_https = TRUE) %>%
  arrange(desc(LastModified))

#' Get a file from an S3 bucket
#' @param file the file
#' @param bucket the bucket to use, default is "bibliometrics"
#' @importFrom aws.s3 get_object
#' @importFrom dplyr filter pull
#' @export
minio_get <- function(file, bucket = "bibliometrics") {
  my_file <- minio_ls() %>% filter(Key == file) %>% pull("Key")
  get_object(my_file, bucket, use_https = TRUE)
}
