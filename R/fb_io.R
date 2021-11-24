#' @title fb_file_name
#'
#' @description Build a file name from the given flowBunch. The project path and name of the flowBunch are concatenated to locate the directory of the flowBunch. If a format string is supplied, a file name is appended.
#'
#' @param fb flowBunch
#' @param s_format string, the format used in a sprintf. It eases to build standardized file names.
#'
#' @importFrom checkmate assertClass assertString
#' @export

fb_file_name <- function(
  fb,
  s_format = NULL
) {
  assertClass(fb, "flowBunch")
  assertString(s_format, null.ok = TRUE)
  proj_name <- fb@output$name
  proj_path <- fb@output$path
  assertString(proj_name)
  assert(dir.exists(proj_path))
  if (is.null(s_format)) {
    file.path(proj_path, proj_name)
  } else {
    if (!grepl("%s", s_format, fixed = TRUE))
      s_format <- paste0("%s", s_format)
    file.path(proj_path, proj_name, sprintf(s_format, proj_name))
  }
}


#' @title fb_write
#'
#' @description .
#'
#' @param fb flowBunch
#'
#' @importFrom checkmate assertClass
#' @export

fb_write <- function(
  fb
) {
  assertClass(fb, "flowBunch")
  proj_dir <- fb_file_name(fb)
  if (!dir.exists(proj_dir))
    dir.create(proj_dir)
  fb_write_pheno(fb)
  fb_write_panel(fb)
  save(fb, file = fb_file_name(fb, "%s-fcsBunch.RData"))
}


#' @title fb_reload
#'
#' @description .
#'
#' @param fb flowBunch
#'
#' @importFrom checkmate assertClass
#' @export

fb_reload <- function(
  fb
) {
  assertClass(fb, "flowBunch")
  fb <- fb_open_pheno(fb)
  fb <- fb_open_panel(fb)
  fb
}
