#' @title fb_open_panel
#'
#' @description .
#'
#' @param fb flowBunch
#' @param file string
#'
#' @importFrom readxl read_excel
#' @importFrom checkmate assertClass assertString assert testClass
#' @export

fb_open_panel <- function(
  fb,
  file
) {
  assertClass(fb, "flowBunch")
  if (missing(file))
    file <- fb_file_name(fb, "%s-panel.xlsx")
  assertString(file)
  assert(file.exists(file))
  if (grepl("xlsx$", file)) {
    panel <- readxl::read_excel(file)
    panel <- as.data.frame(panel)
  } else
    stop("Unrecognized file format for panel")
  if (testClass(panel, "data.frame")) {
    # TODO: do some basic checks
  }
  fb@panel <- panel
  fb
}



#' @title fb_write_panel
#'
#' @description .
#'
#' @param fb flowBunch
#'
#' @export

fb_write_panel <- function(
  fb
) {
  assertClass(fb, "flowBunch")
  file <- fb_file_name(fb, "%s-panel.xlsx")
  assert(dir.exists(dirname(file)))
  if (grepl("xlsx$", file)) {
    if (file.exists(file))
      file.rename(file, timetag(file))
    writexl::write_xlsx(fb@panel, file)
  } else
    stop("Unrecognized file format for panel. No data written to disk.")
}
