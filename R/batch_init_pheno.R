#' @title batch_init_pheno
#'
#' @description ...
#'
#' @param fb a flowBunch.
#' @param batch_pattern String pattern to recover the batch id from the FCS
#'   filename. For example "_Batch(\\d+)_" extracts the numeric value after the
#'   "_Batch" keyword and puts it in the batch_id column.
#' @param ref_sample_pattern String pattern to identify the reference sample
#'   from the FCS filename within each batch. For example "_c20\\.fcs$" puts "Y"
#'   in the sample_is_ref column when the file name ends up with "_c20.fcs". \\.
#'   is used to match the specil character ".". $ is used to match the end of
#'   the string and is therefore at the end.
#'
#' @importFrom utils type.convert
#' @importFrom checkmate assertClass assertString testNull
#' @export

batch_init_pheno <- function(
  fb,
  batch_pattern = ".+?_Batch(\\d+)_.+",
  ref_sample_pattern = "_c20\\.fcs$"
) {
  assertClass(fb, "flowBunch")
  assertString(batch_pattern)
  assertString(ref_sample_pattern)
  mdta <- data.frame(
    batch_id = rep("", nrow(fb@pheno)),
    sample_is_ref = "",
    batch_is_ref = "",
    stringsAsFactors = FALSE
  )
  # guess batch_id
  if (testNull(batch_pattern) || batch_pattern == "") {
    message("batch_pattern is empty or null.")
  } else if (grepl("\\(.+?\\)", batch_pattern) == FALSE) {
    message("batch_pattern does not contains a pattern enclosed by ().")
  } else {
    mdta$batch_id <- gsub(batch_pattern, "\\1", fb@pheno$sample_id)
    if (any(mdta$batch_id == "")) {  # "" leads to NA
      message("Some batch id could not be retrieved.")
    } else {
      # convert to numeric if possible
      mdta$batch_id <- type.convert(c(mdta$batch_id), as.is = TRUE)
    }
  }
  # guess sample_is_ref
  if (testNull(ref_sample_pattern) || ref_sample_pattern == "") {
    message("ref_sample_pattern is empty or null.")
  } else {
    mdta$sample_is_ref <- ifelse(grepl(ref_sample_pattern, fb@pheno$sample_id), "Y", "")
  }
  # guess batch_is_ref
  if (any(mdta$batch_id != "") && any(mdta$sample_is_ref == "Y")) {
    # look for the smallest batch id and set it as reference
    batch_ref_id <- which(
      mdta$batch_id != "" && mdta$sample_is_ref == "Y")
    if (length(batch_ref_id) > 0)
      mdta$batch_is_ref[min(batch_ref_id)] <- "Y"
  }
  # verify every file has a batch
  if (any(mdta$batch_id == ""))
    message("One or more file has no value in the batch_id column.")
  # verify every batch has a reference
  sample_is_ref_counts <- tapply(mdta$sample_is_ref == "Y", mdta$batch_id, sum)
  if (any(sample_is_ref_counts) == 0) {
    message("A batch has no reference sample.")
  }
  if (any(sample_is_ref_counts) >= 2) {
    message("A batch has more than one reference sample.")
  }
  # verify a batch is a reference
  if (all(mdta$batch_is_ref == ""))
    message("No file has a value in the batch_is_ref column.")
  # return
  fb@pheno$batch_id <- mdta$batch_id
  fb@pheno$sample_is_ref <- mdta$sample_is_ref
  fb@pheno$batch_is_ref <- mdta$batch_is_ref
  fb
}
