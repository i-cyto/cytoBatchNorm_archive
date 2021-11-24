#' @title cbn_create_templates
#'
#' @description This function allows you to prepare templates for the MetaData
#'   file and the Panel file.
#'
#' @param files Optional character vector with filenames passed to
#'   \code{\link[flowCore]{read.FCS}} for details.
#' @param path Directory where to look for the files passed to
#'   \code{\link[flowCore]{read.FCS}} for details..
#' @param pattern This argument is passed to \code{\link[flowCore]{read.FCS}}
#'   for details.
#' @param output_dir String.
#' @param batch_pattern String pattern to recover the batch id from the FCS
#'   filename. For example "_Batch(\\d+)_" extracts the numeric value after the
#'   "_Batch" keyword and puts it in the batch_id column.
#' @param ref_sample_pattern String pattern to identify the reference sample from the FCS
#'   filename within each batch. For example "_c20\\.fcs$" puts "Y" in the sample_is_ref column.
#' @param which.lines Limits the number of events read from FCS files. See
#'   \code{\link[flowCore]{read.FCS}} for details.
#' @param emptyValue See \code{\link[flowCore]{read.FCS}} for details.
#' @param channel_alias see \code{\link[flowCore]{read.FCS}} for details.
#' @param \dots Further arguments that get passed on to
#'   \code{\link[flowCore]{read.flowSet}}.
#'
#' @examples
#' \dontrun{
#' fcs.loc <- system.file("extdata",package="flowCore")
#' samp <- read.flowSet(dir = fcs.loc, pattern = "0+")
#' }
#'
#' @keywords prepare
#'
#' @importFrom flowCore read.flowSet parameters pData fsApply keyword identifier
#' @importFrom writexl write_xlsx
#' @importFrom utils write.csv
#' @export

cbn_create_templates <- function(
  files = NULL, path = ".", pattern = NULL,
  output_dir  =  ".",
  batch_pattern = NULL,
  ref_sample_pattern = NULL,
  # alter.names = FALSE,
  which.lines = 1:50,
  emptyValue = TRUE,
  channel_alias  =  NULL,
  ...)
{
  assertString(batch_pattern, null.ok = TRUE, fixed = "(")
  assertString(ref_sample_pattern, null.ok = TRUE)
  # read all FCS files through read.flowSet which does consistency checks
  flowSet <- read.flowSet(
    files = files, path = path, pattern = pattern,
    which.lines = which.lines, emptyValue = emptyValue,
    channel_alias = channel_alias,
    ...
  )
  # create panel
  info <- pData(parameters(flowSet[[1]]))
  info[is.na(info$desc), "desc"] <- info[is.na(info$desc), "name"]
  panel <- data.frame(
    fcs_colname = info$name,
    antigen = info$desc,
    batchnorm_method = "percentile_hi",
    batchnorm_param = 0.95,
    guess_antigen = guess_antigen(info$desc),
    comment = ""
  )
  # clear param
  panel[ panel$guess_antigen == panel$antigen, "batchnorm_param"] <- NA
  # create metadata
  info <- fsApply(flowSet, function(ff) {
    c(file_name = keyword(ff, "FILENAME")[[1]],
      sample_id = identifier(ff),
      kwd_FIL = keyword(ff, "$FIL")[[1]]
    )
  })
  info <- as.data.frame(info)
  mdta <- data.frame(
    info[,1:2],
    batch_id = "",
    sample_is_ref = "",
    batch_is_ref = "",
    info[,-(1:2), drop = FALSE],
    stringsAsFactors = FALSE
  )
  # guess batch_id & sample_is_ref
  # browser()
  if (!testNull(batch_pattern)) {
    mdta$batch_id <- gsub(batch_pattern, "\\1", mdta$sample_id)
    if (any(mdta$batch_id != "")) {
      oo <- order(mdta$batch_id)
      batch_ref_id <- oo[mdta$batch_id[oo]!=""][1]
      mdta$batch_is_ref[batch_ref_id] <- "Y"
    }
  }
  if (!testNull(ref_sample_pattern)) {
    mdta$sample_is_ref <- ifelse(grepl(ref_sample_pattern, mdta$sample_id), "Y", NA)
  }
  # write files
  if (is_package_installed("writexl")) {
    writexl::write_xlsx(panel, "template_panel.xlsx")
    writexl::write_xlsx(mdta, "template_mdta.xlsx")
  } else {
    message("You should install 'writexl' package to create XLSX file.\n",
            "Templates are save as CSV files.")
    write.csv(panel, "template_panel.csv")
    write.csv(mdta, "template_mdta.csv")
  }
}


#' @title is_package_installed
#'
#' @description Check if the the given package is installed.
#'
#' @param pkg string, the package that should be installed.

is_package_installed <- function(pkg) {
  if (missing(pkg)) stop()
  length(find.package(pkg, quiet = TRUE, verbose = FALSE))
}

#' @title get_channel_idx
#'
#' @description Get the indices of channel names in a flowFrame. It tries to
#'   match them against names, desc of the flowFrame and guess_antigen(desc).
#'
#' @param channel_names Character vector of channel names to look for.
#' @param ff The flowFrame.
#' @param verbose Logical.
#'
#' @examples
#' \dontrun{
#' fcs.loc <- system.file("extdata",package="flowCore")
#' samp <- read.flowSet(dir = fcs.loc, pattern = "0+")
#' }
#'
#' @importFrom flowCore colnames
#' @export

get_channel_idx <- function(
  channel_names,
  ff,
  verbose = getOption("verbose")
) {
  stopifnot(!missing(channel_names))
  stopifnot(inherits(ff, "flowFrame"))
  pd <- pData(parameters(ff))
  df <- data.frame(
    coln = match(channel_names, pd$name),
    chnn = match(channel_names, pd$desc),
    antn = match(channel_names, guess_antigen(pd$desc))
  )
  apply(df, 1, function(x) {
    x <- x[!is.na(x)]
    if (length(x) == 0) return(NA)
    x <- unique(x)
    if (length(x) == 1) return(x) else return(0)
  })
}


#' @title asinhTransf
#'
#' @param transformId .
#' @param cofactor .
#'
#' @importFrom methods new
#' @importClassesFrom flowCore transform
#' @export

asinhTransf <- function(transformId, cofactor){
  t = new("transform", .Data = function(x) asinh(x / cofactor))
  t@transformationId = transformId
  t
}

#' @title sinhTransf
#'
#' @param transformId .
#' @param cofactor .
#'
#' @importFrom methods new
#' @importClassesFrom flowCore transform
#' @export

sinhTransf <- function(transformId, cofactor){
  t = new("transform", .Data = function(x) sinh(x) * cofactor)
  t@transformationId = transformId
  t
}

#' @title asinhCytof
#'
#' @export

asinhCytof <- asinhTransf("asinhCytof")

#' @title sinhCytof
#'
#' @export

sinhCytof <- sinhTransf("sinhCytof")
