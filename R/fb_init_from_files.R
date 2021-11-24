#' @title fb_init_from_files
#'
#' @description Mainly based on read.flowSet function.
#'
#' @param fb flowBunch
#' @param files strings, the vector of file names. NULL by default, which leads to load all FCS files in the given path.
#' @param path string, the unique path where the files are located.
#' @param pattern string.
#' @param which.lines integer, the event to load for the quick scan.
#' @param channel_alias strings, see read.flowSet
#' @param ... parameters passed to read.flowSet
#' @param verbose integer, level of verbosity
#' @param with_date_time logical, extract the date and time from FCS header
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
  verbose = 0,
  with_date_time = FALSE
) {
  assertClass(fb, "flowBunch")
  assertLogical(with_date_time, len = 1)
  assertIntegerish(verbose, lower = 0, len = 1)
  # clear expressions
  fb@exprs <- NULL
  # read all FCS files through read.flowSet which does consistency checks
  if (verbose) message("Scanning a bunch of FCS files...")
  flowSet <- do.call(
    "read.flowSet",
    c(
      files = files, path = path, pattern = pattern,
      which.lines = which.lines,
      channel_alias = channel_alias,
      fb@options$read_fcs, # TODO: manage options overlap
      ...
    )
  )
  if (verbose) message("Preparing panel and pheno")
  # create panel
  info <- pData(parameters(flowSet[[1]]))
  info[is.na(info$desc), "desc"] <- info[is.na(info$desc), "name"]
  panel <- data.frame(
    fcs_colname = info$name,
    antigen = info$desc,
    guess_antigen = guess_antigen(info$desc),
    comment = ""
  )
  fb@panel <- panel
  # create metadata
  pheno <- fsApply(flowSet, function(ff) {
    c(file_name = keyword(ff, "FILENAME")[[1]],
      sample_id = identifier(ff),
      TOT = keyword(ff, "$TOT")[[1]],
      PAR = keyword(ff, "$PAR")[[1]],
      FIL = keyword(ff, "$FIL")[[1]],
      if (with_date_time) c(
        DATE = keyword(ff, "$DATE")[[1]],
        BTIM = keyword(ff, "$BTIM")[[1]],
        ETIM = keyword(ff, "$ETIM")[[1]])
    )
  })
  pheno <- data.frame(pheno, stringsAsFactors = FALSE)
  pheno$TOT <- as.integer(pheno$TOT)
  if (!with_date_time) {
    pheno
  }
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
