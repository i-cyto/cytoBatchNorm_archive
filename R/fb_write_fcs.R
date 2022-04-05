#' @title fb_write_fcs
#'
#' @description ...
#'
#' @param fb a flowBunch.
#' @param file_ids integers, file descriptors from file_no column in pheno
#'   table.
#' @param fcs_colnames .
#' @param out_dir string, path
#' @param verbose integer, verbosity level.
#'
#' @importFrom flowCore read.FCS write.FCS
#' @importClassesFrom flowCore flowFrame
#' @importMethodsFrom flowCore exprs exprs<-
#' @importFrom checkmate assertClass assertIntegerish assertString
#' @export

fb_write_fcs <- function(
  fb,
  file_ids,
  fcs_colnames,
  out_dir,
  verbose = 1
) {
  assertClass(fb, "flowBunch")
  assertIntegerish(verbose)
  # check arguments
  if (verbose) message("Writing FCS from Bunch")
  # set defaults
  if (missing(fcs_colnames)) fcs_colnames <- fb@panel$fcs_colname
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
  # import data
  for (i in match(file_nos, fb@pheno$file_no)) {
    if (verbose > 1)
      message(sprintf("writing FCS %3d/%d", i, length(file_nos)))
    ff <- do.call("read.FCS", c(fb@pheno$file_name[i], fb@options$read_fcs))
    # chn_idx <- get_channel_idx(fcs_colnames, ff)
    # if (any(is.na(chn_idx))) {
    #   warning(sprintf(
    #     "skipping file %s as channels %s are not found.",
    #     basename(fb@pheno$file_name[i]),
    #     paste0(fcs_colnames[is.na(chn_idx)], collapse = ",")))
    #   next
    # }
    # downsample
    # replace exprs
    # TODO: check column count and names are the same OR replace only some columns OR write only matched columns
    # TODO: detransform
    # TODO: decompensate
    # if (!is.null(fb@options$do_compensate))
    #     fb@options$compensated <- TRUE
    # remove file_no and cell_no
    exprs(ff) <- fb@exprs[,1:(ncol(fb@exprs)-2)]
    file_name <- file.path(out_dir, basename(fb@pheno$file_name[i]))
    write.FCS(ff, file_name)
  }
  # colnames(dta)[seq(fcs_colnames)] <- fcs_colnames
  #dta$file = factor(dta$file, labels = basename(files))
  # transform
  # if (isTRUE(fb@options$do_transform)) {
  #   if (is.null(fb@options$transforms))
  #     stop("Please define direct transformations.")
  #   for (j in colnames(dta)) {  # reverse transformation is needed
  #     fun_id <- match(j, names(fb@options$transforms))
  #     if (!is.na(fun_id))
  #       dta[,j] <- (fb@options$transforms[[fun_id]])(dta[,j])
  #   }
  #   fb@options$transformed <- TRUE
  # }
  # update history
  # fb@histo <- c(fb@histo, list(
  #   "read_fcs",
  #   fb@pheno$sample_id[file_nos],
  #   sampling,
  #   n_cells,
  #   seeds,
  #   fcs_colnames
  # ))
  # done
}