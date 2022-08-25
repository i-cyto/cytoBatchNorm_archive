#' @title fb_read_fcs
#'
#' @description ...
#'
#' @param fb a flowBunch.
#' @param ret string, specify the format of data imported in the flowBunch.
#' @param file_ids integers, file descriptors from file_no column in pheno
#'   table.
#' @param fcs_colnames .
#' @param sampling string, method to sample events from the FCS files.
#' @param n_cells integer, number of events to retrieve in each FCS, which
#'   depends on the sampling method.
#' @param seed integer, random seed.
#' @param verbose integer, verbosity level.
#'
#' @importFrom flowCore read.FCS write.FCS
#' @importClassesFrom flowCore flowFrame
#' @importFrom checkmate assertClass assertIntegerish assertString
#' @export

fb_read_fcs <- function(
  fb,
  ret = c("flowFrame", "matrix", "data.table"),
  file_ids,
  fcs_colnames,
  sampling = c("ceil", "none"),
  n_cells = 10000,
  seed,
  verbose = 1
) {
  assertClass(fb, "flowBunch")
  assertIntegerish(n_cells, null.ok = TRUE, len = 1, lower = 0)
  assertIntegerish(verbose)
  # check arguments
  ret <- match.arg(ret)
  assertString(ret)
  sampling <- match.arg(sampling)
  assertString(sampling)
  if (verbose) message("Loading Bunch")
  # set defaults
  if (missing(fcs_colnames)) fcs_colnames <- fb@panel$fcs_colname
  if (missing(seed)) seed <- sample.int(2^15) else assertIntegerish(seed)
  # check
  # If no file_no column then create one
  fb <- fb_freeze_file_no(fb)
  # TODO: pheno is not updated to disk
  # TODO: allow file names?
  # TODO: allow duplicated?
  if (missing(file_ids)) {
    file_nos <- fb@pheno$file_no
  } else {
    assertIntegerish(file_ids, lower = 1, upper = max(fb@pheno$file_no))
    matched <- match(file_ids, fb@pheno$file_no)
    if (any(is.na(matched)))
      stop("File ids ", file_ids[is.na(matched)], "not found!")
    file_nos <- file_ids
  }
  # a random seed for each file
  set.seed(seed)
  seeds <- sample.int(2^15, length(file_nos))
  # import data
  dta = NULL
  for (i in match(file_nos, fb@pheno$file_no)) {
    # read file
    if (verbose) message(sprintf("reading FCS %3d/%d", i, length(file_nos)))
    file_path <- fb@pheno$file_name[i]
    if (!file.exists(file_path)) {
      file_path <- file.path(fb@input$dirn, fb@pheno$file_name[i])
      if (!file.exists(file_path)) {
        stop("file not found: ", fb@pheno$file_name[i])
      }
    }
    ff <- do.call("read.FCS", c(file_path, fb@options$read_fcs))
    chn_idx <- get_channel_idx(fcs_colnames, ff)
    if (any(is.na(chn_idx))) {
      warning(sprintf(
        "skipping file %s as some channels are not found: %s",
        basename(fb@pheno$file_name[i]),
        paste0(fcs_colnames[is.na(chn_idx)], collapse = ",")))
      next
    }
    # compensate
    # TODO
    # if (!is.null(fb@options$do_compensate))
    #     fb@options$compensated <- TRUE
    # downsample
    if (sampling == "ceil" && nrow(ff) > n_cells) {
      set.seed(seeds[i])
      cell_idx <- sample.int(nrow(ff), n_cells)
      cell_idx <- sort(cell_idx)  # sort to keep acquisition order
      xprs <- cbind.data.frame(exprs(ff)[cell_idx, chn_idx],
                               file_no = fb@pheno$file_no[i],
                               cell_no = cell_idx)
    } else {
      xprs <- cbind.data.frame(exprs(ff)[, chn_idx],
                               file_no = fb@pheno$file_no[i],
                               cell_no = seq(nrow(ff)))
    }
    # append data
    dta = rbind(dta, xprs)
  }
  colnames(dta)[seq(fcs_colnames)] <- fcs_colnames
  #dta$file = factor(dta$file, labels = basename(files))
  # transform
  if (isTRUE(fb@options$do_transform)) {
    if (is.null(fb@options$transforms))
      stop("Please define direct transformations.")
    for (j in colnames(dta)) {  # reverse transformation is needed
      fun_id <- match(j, names(fb@options$transforms))
      if (!is.na(fun_id))
        dta[,j] <- (fb@options$transforms[[fun_id]])(dta[,j])
    }
    fb@options$transformed <- TRUE
  }
  fb@exprs <- dta
  if (verbose)
    message(sprintf("exprs is %d cells x %d channels", nrow(dta), ncol(dta)))
  # update history
  fb@histo <- c(fb@histo, list(
    "read_fcs",
    fb@pheno$sample_id[file_nos],
    sampling,
    n_cells,
    seeds,
    fcs_colnames
  ))
  # done
  fb
}
