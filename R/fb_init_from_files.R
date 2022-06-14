#' @title fb_init_from_files
#'
#' @description Initialize a flowBunch from a set of files, checking their
#'   parameters and channels. Based on fb_scan_files function, similar to
#'   read.flowSet function.
#'
#' @param fb flowBunch
#' @param files strings, the vector of file names. NULL by default, which leads
#'   to load all FCS files in the given path.
#' @param path string, the unique path where the files are located.
#' @param pattern string.
#' @param which.lines integer, the events to load for the quick scan.
#' @param channel_alias strings, see read.flowSet
#' @param ... parameters passed to read.flowSet
#' @param verbose integer, level of verbosity
#'
#' @importFrom stats quantile
#' @importFrom checkmate assertClass assertLogical assertIntegerish
#' @export

fb_init_from_files <- function(
  fb,
  files = NULL,
  path = ".",
  pattern = NULL,
  which.lines = 1:50,
  channel_alias = NULL,
  ...,
  verbose = 0
) {
  assertClass(fb, "flowBunch")
  assertIntegerish(verbose, lower = 0, len = 1)
  # TODO: manage options overlap with ...
  # clear expressions
  fb@exprs <- NULL
  # read all FCS files through read.flowSet which does consistency checks
  if (verbose) message("Scanning a bunch of FCS files...")
  fscan <- do.call(
    "fb_scan_files",
    c(
      files = files, path = path, pattern = pattern,
      outfile = NULL,
      which.lines = which.lines, channel_alias = channel_alias,
      fb@options$read_fcs,
      ...
    )
  )
  # return
  if (attr(fscan, "is_compliant") == FALSE) {
    message("Initialization aborted: files are not homogeneous.")
    return(NULL)
  }
  if (verbose) message("Preparing panel and pheno")
  # create panel
  panel <- data.frame(
    fcs_colname = as.character(fscan[1,grep("^P\\d+N", colnames(fscan))]),
    antigen = as.character(fscan[1,grep("^P\\d+S", colnames(fscan))]),
    guess_antigen = NA,
    comment = ""
  )
  idx <- is.na(panel$antigen)
  panel[idx, "antigen"] <- panel[idx, "fcs_colname"]
  panel$guess_antigen <- guess_antigen(panel$antigen)
  fb@panel <- panel
  # create pheno/metadata
  pheno <- data.frame(
    file_name = as.character(fscan[, "file_fullname"]),
    sample_id = NA,
    fscan[, c("TOT", "PAR", "DATE", "BTIM", "ETIM")],
    stringsAsFactors = FALSE
  )
  sample_id <- basename(pheno$file_name)
  sample_id <- gsub("\\.fcs$", "", sample_id, ignore.case = TRUE)
  pheno$sample_id <- sample_id
  pheno$TOT <- as.integer(pheno$TOT)
  pheno$PAR <- as.integer(pheno$PAR)
  fb@pheno <- pheno
  # store input
  fb@input$files <- files
  fb@input$path <- path
  fb@input$pattern <- pattern
  # return
  message("Initialization done.")
  if (verbose) {
    message(sprintf("flowBunch initialized from %d FCS files", nrow(pheno)))
    message(sprintf("FCS files directory: %s", paste(unique(dirname(pheno$file_name)), collapse = ',')))
    message(sprintf("Channels/columns: %d", nrow(panel)))
    message(sprintf("Cells/rows: %d  (%s)", sum(pheno$TOT), paste(quantile(pheno$TOT), collapse = "/")))
  }
  fb
}
