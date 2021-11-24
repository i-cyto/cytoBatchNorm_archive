#' @title batch_init_pheno
#'
#' @description ...
#'
#' @param fb a flowBunch.
#' @param batch_pattern String pattern to recover the batch id from the FCS
#'   filename. For example "_Batch(\\d+)_" extracts the numeric value after the
#'   "_Batch" keyword and puts it in the batch_id column.
#' @param ref_sample_pattern String pattern to identify the reference sample from the FCS
#'   filename within each batch. For example "_c20\\.fcs$" puts "Y" in the sample_is_ref column.
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
  # guess batch_id & sample_is_ref
  # browser()
  if (!testNull(batch_pattern)) {
    mdta$batch_id <- gsub(batch_pattern, "\\1", fb@pheno$sample_id)
    if (any(mdta$batch_id == "")) {  # "" leads to NA
      message("Some batch id could not be retrieved.")
    } else {
      # convert to numeric if possible
      mdta$batch_id <- type.convert(c(mdta$batch_id), as.is = TRUE)
      # look for the smallest batch id and set it as reference
      batch_ref_id <- order(mdta$batch_id)[1]
      mdta$batch_is_ref[batch_ref_id] <- "Y"
    }
  }
  if (!testNull(ref_sample_pattern)) {
    mdta$sample_is_ref <- ifelse(grepl(ref_sample_pattern, fb@pheno$sample_id), "Y", NA)
  }
  # TODO: verify every file has a batch
  # TODO: verify every batch has a reference
  fb@pheno$batch_id <- mdta$batch_id
  fb@pheno$sample_is_ref <- mdta$sample_is_ref
  fb@pheno$batch_is_ref <- mdta$batch_is_ref
  fb
}
