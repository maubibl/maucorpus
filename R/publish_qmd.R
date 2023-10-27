qmd_path <- function()
  system.file(package = "kthcorpus", "rmarkdown", "report-mods", "mods.qmd")

qmd_params <- function(begdate, enddate) {

  if (!missing(begdate) && !missing(enddate)) {
    stopifnot(all(vapply(c(begdate, enddate), is.Date, logical(1))))
    params <- list(
      begdate = as.character(begdate),
      enddate = as.character(enddate)
    )
  } else {
    params <- list(use_minio = 'true')
  }
  return(params)
}

#' @noRd
#' @importFrom R.utils copyDirectory
publish_qmd_report <- function(src = qmd_path(), params = qmd_params(),
  destdir = "kthb/kthcorpus/mods", cleanup = TRUE) {

  tgt <- file.path(tempdir(), "report-mods")
  if (cleanup) on.exit(unlink(tgt, recursive = TRUE))

  message("Copying ", dirname(src), " to ", tgt)
  is_created <- if (!dir.exists(tgt)) dir.create(tgt, recursive = TRUE) else TRUE
  is_copied <- R.utils::copyDirectory(dirname(src), tgt, recursive = TRUE)

  i <- file.path(tgt, basename(src))
  message("Rendering ", i, " ...")
  is_rendered <-
    quarto::quarto_render(
      input = i,  output_format = "html",
      execute_params = params,
      execute_dir = tgt
    )

  # TODO: move mods.html and the generated subdir to kthb/kthcorpus/mods
  #file.rename(dir(tgt, "\\d{8}_\\d{6}"), d6)
  #file.rename(dir(tgt, "*.html"))
  #browseURL(file.path(tgt, "mods.html"))
  #dir(tgt)

  d6 <- strftime(Sys.time(), "%Y%m%d_%H%M")
  dst <- file.path(destdir, d6)

  message("Publishing ", tgt, " to ", dst)
  #options("minioclient.dir" = dirname(Sys.which("mc")))
  #minioclient::mc_mirror(tgt, dst, overwrite = TRUE)

  mc_cmd <- glue::glue("mc mirror --overwrite {tgt} {dst}")
  system(mc_cmd)
}
