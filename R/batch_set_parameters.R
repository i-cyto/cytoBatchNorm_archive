#' @title batch_set_parameters
#'
#' @description ...
#'
#' @param fb a flowBunch.
#' @param batchnorm_method String pattern to recover the batch id from the FCS
#'   filename. For example "_Batch(\\d+)_" extracts the numeric value after the
#'   "_Batch" keyword and puts it in the batch_id column.
#' @param batchnorm_params String pattern to identify the reference sample from the FCS
#'   filename within each batch. For example "_c20\\.fcs$" puts "Y" in the sample_is_ref column.
#' @param overwrite .
#' @param channels .
#'
#' @importFrom checkmate assertClass assertString assertLogical
#' @export

batch_set_parameters <- function(
  fb,
  batchnorm_method = "percentile_hi",
  batchnorm_params = 0.95,
  overwrite = FALSE,
  channels
) {
  assertClass(fb, "flowBunch")
  assertString(batchnorm_method)
  assertLogical(overwrite, len = 1)
  batchnorm_already <- "batchnorm_method" %in% colnames(fb@panel) &&
    "batchnorm_params" %in% colnames(fb@panel)
  # if channels is missing, all batchnorm_xxx are reset to the given values
  # whether batchnorm_xxx already set or not
  if (missing(channels)) {
    if (batchnorm_already)
      if (!overwrite) {
        message("Normalisation and/or Parameter already in panel. No overwrite.")
        return(fb)
      } else
        message("Normalisation and/or Parameter already in panel. Overwrite.")
    fb@panel$batchnorm_method <- NA
    fb@panel$batchnorm_params <- NA
    channels_ok <- which_is_marker(fb@panel$antigen)
    fb@panel$batchnorm_method[channels_ok] <- batchnorm_method
    fb@panel$batchnorm_params[channels_ok] <- batchnorm_params
  }
  # else, all batchnorm_xxx of the found channels are set to the given values
  # if batchnorm_xxx not present, they are initialized to NA first
  else {
    if (!batchnorm_already) {
      fb@panel$batchnorm_method <- NA
      fb@panel$batchnorm_params <- NA
    }
    id_found <- guess_match_channels(fb, channels)
    for (i in seq(channels)) {
      if (is.na(id_found[i])) {
        message(channels[i], "could not be clearly identified.")
        next
      }
      fb@panel$batchnorm_method[id_found[i]] <- batchnorm_method
      fb@panel$batchnorm_params[id_found[i]] <- batchnorm_params
    }
  }
  fb
}
