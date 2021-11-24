#' @title fb_open_pheno
#'
#' @description .
#'
#' @param fb flowBunch
#' @param file string
#'
#' @importFrom readxl read_xlsx
#' @importFrom checkmate assertClass assertString assert testClass
#' @export

fb_open_pheno <- function(
  fb,
  file
) {
  assertClass(fb, "flowBunch")
  if (missing(file))
    file <- fb_file_name(fb, "%s-pheno.xlsx")
  assertString(file)
  assert(file.exists(file))
  if (grepl("xlsx$", file)) {
    pheno <- readxl::read_excel(file)
    pheno <- as.data.frame(pheno)
  } else
    stop("Unrecognized file format for pheno.")
  if (testClass(pheno, "data.frame")) {
    # do some basic checks
  }
  fb@pheno <- pheno
  fb
}

# TODO: check file_no if given

#' @title fb_write_panel
#'
#' @description .
#'
#' @param fb flowBunch
#'
#' @importFrom writexl write_xlsx
#' @importFrom checkmate assertClass
#' @export

fb_write_pheno <- function(
  fb
) {
  assertClass(fb, "flowBunch")
  file <- fb_file_name(fb, "%s-pheno.xlsx")
  assert(dir.exists(dirname(file)))
  if (grepl("xlsx$", file)) {
    if (file.exists(file))
      file.rename(file, timetag(file))
    writexl::write_xlsx(fb@pheno, file)
  } else
    stop("Unrecognized file format for pheno. No data written to disk.")
}
