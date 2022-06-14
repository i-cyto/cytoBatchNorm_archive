#' @title fb_scan_files
#'
#' @description Scan files and check flowBunch compliance.
#'
#' @param files strings, the vector of file names. NULL by default, which leads
#'   to load all FCS files in the given path.
#' @param path string, the unique path where the files are located.
#' @param pattern string.
#' @param keywords string.
#' @param outfile string.
#' @param transformation FALSE,
#' @param truncate_max_range FALSE,
#' @param which.lines integer, the events to load for the quick scan.
#' @param channel_alias strings, see read.flowSet
#' @param ... parameters passed to read.flowSet
#'
#' @importFrom writexl write_xlsx
#' @export

fb_scan_files <- function(
    files = NULL,
    path = ".",
    pattern = "\\.fcs$",
    keywords = c("FILENAME", "$TOT", "$PAR", "$CYT", "$CYTSN",
                 "$DATE", "$BTIM", "$ETIM"),
    outfile = "scan_files.xlsx",
    transformation = FALSE,
    truncate_max_range = FALSE,
    which.lines = 1:50,
    channel_alias = NULL,
    ...
) {
  if (is.null(files)) {
    files <- dir(path, pattern, ignore.case = TRUE, full.names = TRUE)
    if (length(files) < 1)
      stop("No file matching '", pattern, "' found in ", path)
  } else {
    if (!is.character(files))
      stop("'files' must be a character vector.")
    if (path != ".")
      files <- file.path(path, files)
    if (!all(file.exists(files)))
      stop("Not all given files could be found in", path)
  }
  # TODO: check duplicated basenames
  # fn <- make.unique(basename(files))

  message("Scanning FCS files...")
  # scan files and report information and parameters
  res <- list()
  for (i in 1:length(files)) {
    fn <- files[i]
    if (!file.exists(fn)) {
      report <- c(fn, "-1")
    } else {
      ff <- read.FCS(fn, transformation = transformation,
                     which.lines = which.lines,
                     truncate_max_range = truncate_max_range, ...)
      # get keywords replacing unfound aka NULL by NA
      report <- keyword(ff, keywords)
      report <- c(fn, sapply(report, function(x) if (is.null(x)) NA else x))
      # append name and description of channels
      report <- c(report, c(t(
        as.matrix(pData(parameters(ff))[, c("name", "desc")]))))
    }
    res[[i]] <- report
  }

  message("Extracting panel and pheno")
  # one string per file, filling up with NA
  nc <- max(sapply(res, length))
  res <- lapply(res, function(x)
    paste0(c(x, rep(NA, nc-length(x))), collapse = "\t"))
  # convert strings as data.frame
  res2 <- read.table(text = paste0(res, collapse = "\n"), sep = "\t")
  klen <- length(keywords)
  idx <- seq((ncol(res2)-klen)/2)
  colnames(res2) <- c("file_fullname", gsub("\\$", "", keywords))
  colnames(res2)[klen+idx*2] <- sprintf("P%02dN", idx)
  colnames(res2)[klen+idx*2+1] <- sprintf("P%02dS", idx)

  # compliance test
  is_compliant <- all(
    sapply(res2[, (klen+1):ncol(res2)], function(x) length(unique(x)) == 1))

  # write report
  if (!is.null(outfile)) {
    outpath <- dirname(outfile)
    if (!dir.exists(outpath)) {
      message("Report directory ", outpath, " does not exist")
    } else {
      if (dir.exists(outfile))  # outfile is a directory
        outfile <- file.path(outfile, "scan_files.xlsx")
      writexl::write_xlsx(res2, path = outfile)
      message("Report written ", outfile)
    }
  }

  # return
  structure(res2, outfile = outfile, is_compliant = is_compliant)
}
