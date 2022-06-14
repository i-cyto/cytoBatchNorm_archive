#' @title fb_read_pheno
#'
#' @description .
#'
#' @param fb flowBunch
#' @param file string
#'
#' @importFrom readxl read_xlsx
#' @importFrom checkmate assertClass assertString assertFileExists testClass
#' @export

fb_read_pheno <- function(
  fb,
  file
) {
  assertClass(fb, "flowBunch")
  if (missing(file))
    file <- fb_file_name(fb, "%s-pheno.xlsx")
  assertString(file)
  assertFileExists(file)
  if (grepl("xlsx$", file)) {
    pheno <- readxl::read_excel(file)
    pheno$batch_id <- as.character(pheno$batch_id)
    pheno <- as.data.frame(pheno)
  } else
    stop("Unrecognized file format for pheno.")
  if (testClass(pheno, "data.frame")) {
    # TODO: check that it is a pheno file
  }
  # TODO: check same number of lines
  # TODO: check lines are matching
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
#' @importFrom checkmate assertClass assertPathForOutput
#' @export

fb_write_pheno <- function(
  fb,
  mini = FALSE
) {
  assertClass(fb, "flowBunch")
  file <- fb_file_name(fb, "%s-pheno.xlsx")
  assertPathForOutput(file, overwrite = TRUE)
  if (grepl("xlsx$", file)) {
    if (file.exists(file))
      file.rename(file, timetag(file))
    # minimalize pheno
    pheno <- fb@pheno
    if (mini) {
      # simplify file_name
      pheno$file_name <- unname(sapply(pheno$file_name, basename))
    }
    # write pheno
    writexl::write_xlsx(pheno, file)
  } else
    stop("Unrecognized file format for pheno. No data written to disk.")
}



#' @title fb_check_pheno
#'
#' @description .
#'
#' @param fb flowBunch
#'
#' @importFrom checkmate assertClass
#' @export

fb_check_pheno <- function(
  fb
) {
  assertClass(fb, "flowBunch")
  pheno <- fb@pheno

  errors <- list()
  # identify FCS with no batch_id
  ko <- is.na(pheno$batch_id) | trimws(pheno$batch_id) == ""
  if (any(ko)) errors <- c(errors, fcs_without_batch = sprintf(
    "FCS do not have batch_id: %s",
    paste0(pheno$sample_id[ko], collapse = ", ")))
  # extract anchor per batch
  tbl <- table(pheno$sample_is_ref, pheno$batch_id)
  # identify batch with 0 anchor
  ko <- tbl["Y",] == 0
  if (any(ko)) errors <- c(errors, batch_without_anchor = sprintf(
    "Batches do not have an anchor: %s",
    paste0(colnames(tbl)[ko], collapse = ", ")))
  # identify batch with >1 anchors
  ko <- tbl["Y",] > 1
  if (any(ko)) errors <- c(errors, batch_with_many_anchors = sprintf(
    "Batches have more than 1 anchor: %s",
    paste0(colnames(tbl)[ko], collapse = ", ")))
  # extract reference
  tbl <- table(pheno$batch_is_ref, pheno$batch_id)
  # reference is an anchor
  ko <- sum(tbl["Y",]) == 0
  if (any(ko)) errors <- c(errors, bunch_without_ref =
                             "No batch is reference")
  # reference is unique
  ko <- sum(tbl["Y",]) > 1
  if (any(ko)) errors <- c(errors, bunch_with_many_refs =
                             "More than 1 batch is reference")
  # done
  errors
}
