#' @title fb_export
#'
#' @description Extract pheno and panel files from the given flowBunch and copy
#'   them to the new location. The new location is the fcs directory within the
#'   project directory of the given flowBunch. The export copies the panel and
#'   phenotype files, removing unneeded columns of information. There is 1-to-1
#'   matching between files and pheno.
#'
#' @param fb flowBunch, the flowBunch from which the pheno and panel files are
#'   extracted and in which the fcs directory is located.
#' @param to_dir string, the directory where to copy the pheno and panel files.
#'
#' @return a flowBunch object.
#'
#' @importFrom checkmate assertClass testDirectoryExists
#' @export

fb_export <- function(
  fb,
  to_dir = "fcs"
) {
  assertClass(fb, "flowBunch")
  # Empty init
  to_fb <- flowBunch()
  # Set input params
  # the pseudo project is located in the output "fcs" directory
  # the pheno and panel files will be located there
  to_fb@storage$dirn <- fb_file_name(fb)
  to_fb@storage$basen <- to_dir
  # the FCS files are located there
  fcs_dir <- fb_file_name(to_fb)
  if (!testDirectoryExists(fcs_dir))
    warning("to_dir does not exist \'", fcs_dir, "\'")
  # if (!dir.exists(fcs_dir)) dir.create(fcs_dir)
  to_fb@input$dirn <- fcs_dir
  to_fb@input$pattern <- "\\.[fF][cC][sS]$"
  # update and write pheno
  pheno <- fb@pheno
  pheno$file_name <- unname(sapply(pheno$file_name, basename))
  to_fb@pheno <- pheno
  fb_write_pheno(to_fb)
  # update and write panel
  panel <- fb@panel
  batch_cols <- grep("^batchnorm_", colnames(panel))
  if (length(batch_cols))
    panel <- panel[, -batch_cols]
  to_fb@panel <- panel
  fb_write_panel(to_fb)
  to_fb
}
