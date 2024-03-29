#' @title fb_file_path
#'
#' @description Give the storage path of the given flowBunch. The project dirname
#'   and basename of the flowBunch are concatenated to locate the storage directory of
#'   the flowBunch.
#'
#' @param fb flowBunch
#' @param ... string, folders to append to the path.
#'
#' @importFrom checkmate assertClass assertString testNull
#' @export

fb_file_path <- function(
    fb,
    ...
) {
  assertClass(fb, "flowBunch")
  proj_name <- fb@storage$basen
  proj_dirname <- fb@storage$dirn
  assertString(proj_name)
  if (testNull(proj_dirname))
    return(NULL)
  assert(dir.exists(proj_dirname))
  file.path(proj_dirname, proj_name, ...)
}


#' @title fb_file_name
#'
#' @description Build a file name from the given flowBunch within the storage
#'   folder. The format string is used to build the file name.
#'
#' @param fb flowBunch
#' @param s_format string, the format used in a sprintf. It eases to build
#'   standardized file names.
#'
#' @importFrom checkmate assertClass assertString testNull
#' @export

fb_file_name <- function(
  fb,
  s_format = NULL
) {
  assertClass(fb, "flowBunch")
  assertString(s_format, null.ok = TRUE)
  if (is.null(s_format)) return(NULL)
  if (!grepl("%s", s_format, fixed = TRUE))
    s_format <- paste0("%s", s_format)
  proj_name <- fb@storage$basen
  fb_file_path(fb, sprintf(s_format, proj_name))
}


#' @title fb_write
#'
#' @description .
#'
#' @param fb flowBunch
#' @param write_exprs logical, write expressions; default to FALSE.
#'
#' @importFrom checkmate assertClass testNull assertLogical
#' @export

fb_write <- function(
  fb,
  write_exprs = FALSE
) {
  assertClass(fb, "flowBunch")
  assertLogical(write_exprs)
  proj_dir <- fb_file_path(fb)
  if (testNull(proj_dir))
    return(NULL)
  if (!dir.exists(proj_dir))
    dir.create(proj_dir)
  fb_write_pheno(fb)
  fb_write_panel(fb)
  if (!write_exprs)
    fb@exprs <- NULL
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
  fb <- fb_read_pheno(fb)
  fb <- fb_read_panel(fb)
  fb
}
