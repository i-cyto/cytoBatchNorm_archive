#' @title fb_initiate
#'
#' @description Create a flowBunch at the designed location by reading the
#'   designated FCS. A directory is also created to keep information (panel and
#'   phenotype of FCS files). The data measured on the cells are not loaded.
#'
#' @param project_name string, name of the project which is a short name for the
#'   current normalization process. The function creates a directory with the
#'   project name in the project_dir.
#' @param project_dir string, the directory in which the project resides.
#' @param fcs_dir string, the directory where all FCS files could be found.
#' @param pattern string, the pattern to identify FCS files.
#' @param cytometer string, either "mass", "flow" or "spectral". This defines
#'   the default cofactor for transforming channels.
#'
#' @return a flowBunch object.
#'
#' @importFrom checkmate assertDirectoryExists
#' @export

fb_initiate <- function(
  project_name,
  project_dir = NULL,
  fcs_dir,
  pattern = "\\.fcs$",
  cytometer = c("mass", "flow", "spectral")
) {
  assertDirectoryExists(fcs_dir)
  if (!is.null(project_dir))
    assertDirectoryExists(project_dir)
  # Empty init
  my_fb <- flowBunch()
  # Set storage params
  my_fb@storage$basen <- project_name
  my_fb@storage$dirn <- project_dir
  # Scan FCS files
  my_fb <- fb_init_from_files(
    my_fb, dirname = fcs_dir, pattern = pattern, verbose = 1)
  # info
  fb_print(my_fb)
  # set direct and reverse transformations
  method <- "asinh"
  cofactor <- switch (cytometer,
                      mass = 5,
                      flow = 250,
                      spectral = 6000
  )
  # my_fb <- fb_set_transforms(
  #   my_fb,
  #   transforms = function(x) asinh(x/cofactor),
  #   revtransforms = function(x) sinh(x)*cofactor)
  # names(my_fb@options$transforms)
  # names(my_fb@options$revtransforms)
  # add transform information to panel table, ie define markers transformation
  my_fb <- transf_set_parameters(
    my_fb, transf_method = method, transf_params = cofactor)

  # Write to disk
  fb_write(my_fb)

  my_fb
}


#' @title fb_setup_batch
#'
#' @description Set up the batch by adding 3 columns to the phenotype table:
#'   batch_id, sample_is_ref,	batch_is_ref. The phenotype table contains one row
#'   for each FCS file. The batch_id column designates the batch identifier to
#'   which the FCS file belongs. The sample_is_ref column contains "Y" (yes) if
#'   the FCS file (aka sample) is the replicated reference sample within the
#'   batch. The batch_is_ref designates the sample that is the reference to
#'   which each reference sample will be aligned.
#'
#' @param my_fb flowBunch, the flowBunch to setup.
#' @param batch_pattern string, a regular expression to extract the batch_id.
#' @param ref_sample_pattern string, the directory in which the project resides.
#' @param ... parameters passed to batch_set_parameters.
#'
#' @return a flowBunch object.
#'
#' @export

fb_setup_batch <- function(
  my_fb,
  batch_pattern = NULL,
  ref_sample_pattern = NULL,
  ...
) {
  # add batch information to pheno table, ie annotate FCS files
  my_fb <- batch_init_pheno(my_fb, batch_pattern, ref_sample_pattern)
  # reorder by batch
  my_fb <- fb_order_by(my_fb, "batch_id")
  # add batch information to panel table, ie define markers normalization
  my_fb <- batch_set_parameters(my_fb, ...)
  # Write to disk
  fb_write(my_fb)

  my_fb
}


#' @title fb_open
#'
#' @description Create a flowBunch from one previously saved to disk. This
#'   function only reads the RData file, not the panel and pheno files.
#'
#' @param project_name string, name of the project in which the flowBunch is.
#' @param project_dir string, the directory in which the project resides.
#'
#' @return a flowBunch object.
#'
#' @export

fb_open <- function(
  project_name,
  project_dir
) {
  # Empty init
  my_fb <- flowBunch()
  # Set storage params
  my_fb@storage$basen <- project_name
  my_fb@storage$dirn <- project_dir
  #
  rdata <- fb_file_name(my_fb, "%s-fcsBunch.RData")
  if (!file.exists(rdata)) {
    stop("No fcsBunch at ", dirname(rdata))
  }
  # TODO: check FCS exit or guess new location
  # Return fcsBunch
  tmp_env <- new.env()
  load(rdata, envir = tmp_env)
  get("fb", tmp_env)
}


#' @title fb_open_from_disk
#'
#' @description Create a flowBunch from one previously saved to disk. This
#'   function only reads the RData file, not the panel and pheno files.
#'
#' @param project_name string, name of the project in which the flowBunch is.
#' @param project_dir string, the directory in which the project resides.
#'
#' @return a flowBunch object.
#'
#' @export

fb_open_ <- function(
  project_name,
  project_dir
) {
  # Empty init
  my_fb <- flowBunch()
  # Set storage params
  my_fb@storage$basen <- project_name
  my_fb@storage$dirn <- project_dir
  #
  my_fb@input$dirn <- fb_file_path(my_fb)
  my_fb <- fb_read_panel(my_fb)
  my_fb <- update_transf_from_panel(my_fb)
  my_fb <- fb_read_pheno(my_fb)
  my_fb
}


#' @title fb_reload_from_disk
#'
#' @description Reload the panel and pheno files of the flowBunch.
#'
#' @param my_fb flowBunch.
#'
#' @return a flowBunch object.
#'
#' @export

# TODO: ??? load transforms from panel table
fb_reload_from_disk <- function(
  my_fb
) {
  my_fb <- fb_read_panel(my_fb)
  my_fb <- update_transf_from_panel(my_fb)
  my_fb <- fb_read_pheno(my_fb)
  my_fb
}


#' @title fb_extract_batch_references
#'
#' @description Extract reference samples in each batch and create a new
#'   flowBunch.
#'
#' @param my_fb flowBunch.
#'
#' @return a flowBunch object.
#'
#' @export

fb_extract_batch_references <- function(
  my_fb
) {
  ref_idx <- my_fb@pheno$sample_is_ref == "Y"
  ref_idx[is.na(ref_idx)] <- FALSE
  fba <- my_fb
  fba@pheno <- my_fb@pheno[ref_idx,]
  fba
}


#' @title fb_load_cells
#'
#' @description Load the cells measurements from FCS files into memory. Sampling
#'   is usually applied.
#'
#' @param my_fb flowBunch.
#' @param sampling string, sampling method.
#' @param n_cells integer, number of cells to extract per file.
#'
#' @return a flowBunch object.
#'
#' @export
# TODO:  add seed?

fb_load_cells <- function(
  my_fb,
  sampling = "ceil",
  n_cells = 2000
) {
  my_fb <- fb_read_fcs(
    my_fb,
    sampling = sampling,
    n_cells = n_cells)
  my_fb
}


#' @title fb_plot_ridgelines
#'
#' @description Prints a diagnostic graphics of densities along with
#'   percentiles. Loop across all given channels.
#'
#' @param my_fb flowBunch.
#' @param channels strings, name of channels to display.
#' @param cof numerical.
#' @param cut_lower_than numerical.
#' @param title string, title at the top of the plot.
#'
#' @return a flowBunch object.
#'
#' @export

fb_plot_ridgelines <- function(
  my_fb,
  channels,
  channel_names,
  group_by = "sample_id",
  cof = 8,
  cut_lower_than = -5,
  title = NULL
) {
  tmp_exprs <- fb_get_exprs(my_fb, transformed = TRUE, ret = "data.frame")

  if (missing(channels)) {
    if ("batchnorm_method" %in% colnames(my_fb@panel)) {
      channels_idx <- which(!is.na(my_fb@panel$batchnorm_method))
    } else if ("transf_method" %in% colnames(my_fb@panel)) {
      channels_idx <- which(!is.na(my_fb@panel$transf_method))
    } else {
      stop("There should be a column called transf_method or batchnorm_method.")
    }
    channels <- my_fb@panel$fcs_colname[channels_idx]
  }

  if (missing(channel_names)) {
    channel_names <- my_fb@panel$antigen
    names(channel_names) <- my_fb@panel$fcs_colname
  }

  # TODO: check channels are in the fcs_colname
  # TODO: check channels are in the names of channel_names

  file_no_mapping <- my_fb@pheno[, group_by]
  names(file_no_mapping) <- my_fb@pheno[, "file_no"]

  for (channel in channels)
    dt_plot_ridgelines(
      dta = tmp_exprs,
      channels = channel,
      channel_names = channel_names,
      batch_idn = file_no_mapping,
      batch_lab = group_by,
      cof = cof,
      cut_lower_than = cut_lower_than,
      title = title
    )
}


#' @title fb_split_batch_params
#'
#' @description Split the parameters of batch normalization.
#'
#' @param bn_method string.
#' @param bn_params string.
#'
#' @return a flowBunch object.
#'
#' @export

fb_split_batch_params <- function(
  bn_method, bn_params
) {
  bn_params <- strsplit(as.character(bn_params), " *, *")[[1]]
  bn_exclude_zeroes <- which("exclude_zeroes" == bn_params)
  if (length(bn_exclude_zeroes)) bn_params <- bn_params[ -bn_exclude_zeroes ]
  bn_exclude_zeroes <- length(bn_exclude_zeroes) > 0
  bn_transform <- which("transform" == bn_params)
  if (length(bn_transform)) bn_params <- bn_params[ -bn_transform ]
  bn_transform <- length(bn_transform) > 0
  list(
    method = bn_method,
    params = bn_params,
    exclude_zeroes = bn_exclude_zeroes,
    transform = bn_transform)
}


#' @title fb_model_batch
#'
#' @description Model the batch effects from reference samples.
#'
#' @param fb flowBunch.
#' @param channels strings, name of channels to display.
#' @param verbose integer, verbosity level.
#'
#' @details The model defines a function for each channel of each batch that
#'   transforms the intensity so its percentiles are aligned to the ones of the
#'   reference batch. The model retrieves the method and its parameters for each
#'   channel.
#'
#'   Current implemented methods are:
#'
#'   - "none": f(x) = x
#'
#'   - "percentile_hi": f(x) = x
#'
#'   - "percentile_hi": f(x) = (x)/(batHi)*(refHi)
#'
#'   - "percentile_lohi": f(x) = (x-batLo)/(batHi-batLo)*(refHi-refLo)+refLo
#'
#'   - "percentile_lohi_pos": f(x) =
#'   (x-batLo)/(batHi-batLo)*(refHi-refLo)+refLo, and if f(x) < 0 then f(x) = 0
#'
#'   - "quantiles": f(x) = spline_interpolation(x) that each quantile of the
#'   current batch into the corresponding quantiles of the reference batch. By
#'   default quantiles are c(0.01, .2, .4, .6, .8, .9, .99). If a single
#'   positive integer is set, than quantiles will cut the 0..1 percentiles. If a
#'   set of quantiles is defined, they will be used  for the spline.
#'
#' @importFrom stats splinefun
#' @export

fb_model_batch <- function(
  fb,
  channels,
  verbose = 1
) {
  if (verbose) message("Modeling batch effects...")

  # batchnorm params
  batchnorm_method <- fb@panel$batchnorm_method
  batchnorm_params <- fb@panel$batchnorm_params
  names(batchnorm_method) <- names(batchnorm_params) <- fb@panel$fcs_colname

  # get batch id from file no
  fno_to_bid <- factor(fb@pheno$batch_id)
  names(fno_to_bid) <- fb@pheno$file_no
  exprs_bid <- fno_to_bid[as.character(fb@exprs[, "file_no"])]
  rm(fno_to_bid)

  # get file_no for reference file
  ref_numid <- which(fb@pheno$batch_is_ref != "")
  ref_bid <- fb@pheno$batch_id[ref_numid]
  rm(ref_numid)

  # channels
  if (missing(channels)) {
    channels <- which(!is.na(batchnorm_method))
    channels <- fb@panel$fcs_colname[channels]
  }

  # options_keep_source <- options("keep.source")
  # options(keep.source=FALSE)

  models <- list()
  for (chn in channels) {
    models[[chn]] <- list()
    if (verbose) message("  ", chn, appendLF = FALSE)

    # parse batchnorm params
    bnp <- fb_split_batch_params(batchnorm_method[chn], batchnorm_params[chn])

    # get data from a single channel, so a vector
    all_exprs <- fb_get_exprs(fb, "matrix", transformed = bnp[["transform"]])
    # TODO: get one channel at a time because of transformation cost
    all_exprs_chn <- all_exprs[, chn]
    all_exprs_fid <- exprs_bid
    rm(all_exprs)

    # exclude zeroes
    if (bnp[["exclude_zeroes"]]) {
      all_exprs_fid <- all_exprs_fid[all_exprs_chn > 0]
      all_exprs_chn <- all_exprs_chn[all_exprs_chn > 0]
    }

    # build the model
    mod_env <- new.env(parent = baseenv())
    mod_env$ref_bid <- ref_bid
    mod_env$chn <- chn
    mod_env$method <- method <- bnp[["method"]]

    if (method == "none") {

      params <- as.character(unique(all_exprs_fid))
      mod_env$params <- params
      funs <- local({ sapply(params, function(y) {
        function(x) x
      })}, envir = mod_env)

    } else if (method == "percentile_hi") {

      qlo <- 0
      qhi <- max(as.numeric(bnp[["params"]]))
      params <- tapply(all_exprs_chn, all_exprs_fid,
                       quantile, probs = c(qlo, qhi))
      mod_env$params <- params
      funs <- local({ sapply(names(params), function(y) {
        refHi <- params[[ref_bid]][2]
        batHi <- params[[y]][2]
        if (batHi == 0) {
          warning("Infinite scaling for batch ", y, ", channel ", chn,
                  ". No scaling.", call. = FALSE)
          function(x) x
        } else {
          function(x) (x/batHi*refHi)
        }
      })}, envir = mod_env)

    } else if (method == "quantiles") {

      bnp_params <- as.numeric(bnp[["params"]])
      if (length(bnp_params) == 0 || is.na(bnp_params)) {  # default quantiles
        quantileValues <- c(0.01, .2, .4, .6, .8, .9, .99)
      } else if (length(bnp_params) == 1 && bnp_params > 1) {
        nQ <- min(bnp_params, 101)
        quantileValues <- c(0, (1:(nQ-1))/(nQ-1))
      } else {
        quantileValues <- bnp_params
      }
      params <- tapply(all_exprs_chn, all_exprs_fid,
                       quantile, probs = quantileValues)
      # params <- tapply(all_exprs_chn, all_exprs_fid,
      #                  function(x) {
      #                    x <- x[x>0]
      #                    quantile(x, probs = quantileValues)
      #                  })
      mod_env$params <- params
      funs <- local({ sapply(names(params), function(y) {
        refQ <- params[[ref_bid]]
        batQ <- params[[y]]
        suppressWarnings(
          spl <- stats::splinefun(batQ, refQ, method = "monoH.FC"))
        spl
      })}, envir = mod_env)

    } else if (method %in%
               c("percentile_lohi", "percentile_lohi_pos")) {

      bnp_params <- as.numeric(bnp[["params"]])
      if (length(bnp_params) == 1) qlo <- 0.40 else
        qlo <- max(min(bnp_params), 0)
      qhi <- min(max(bnp_params), 1)
      params <- tapply(all_exprs_chn, all_exprs_fid,
                       quantile, probs = c(qlo, qhi))
      mod_env$params <- params
      funs <- local({ sapply(names(params), function(y) {
        refLo <- params[[ref_bid]][1]
        batLo <- params[[y]][1]
        refHi <- params[[ref_bid]][2]
        batHi <- params[[y]][2]
        if (batHi == batLo) {
          warning("Infinite scaling for batch ", y, ", channel ", chn,
                  ". No scaling.", call. = FALSE)
          function(x) x
        } else if (method == "percentile_lohi") {
          function(x) (x-batLo)/(batHi-batLo)*(refHi-refLo)+refLo
        } else {
          function(x) pmax((x-batLo)/(batHi-batLo)*(refHi-refLo)+refLo, 0)
        }
      })}, envir = mod_env)

    } else {

      stop("Unknown batchnorm method ", method, " for channel ", chn)

    }

    models[[chn]] <- funs

  }
  # options(options_keep_source)
  if (verbose) message("\nDone")
  temp <- fb@procs$batchnorm_funs
  for (chn in names(models)) {
    temp[[chn]] <- models[[chn]]
  }
  fb@procs$batchnorm_funs <- temp
  fb
}



#' @title fb_correct_batch
#'
#' @description Adjust batch effects using the modelling of reference samples.
#'
#' @param fb flowBunch.
#' @param channels strings, name of channels to display.
#' @param verbose integer.
#'
#' @export

# TODO: apply model to the matrix
# TODO: manage transform
fb_correct_batch <- function(
  fb,
  channels,
  verbose = 1
) {
  if (verbose) message("Correcting batch effects...")

  # batchnorm params
  batchnorm_method <- fb@panel$batchnorm_method
  batchnorm_params <- fb@panel$batchnorm_params
  names(batchnorm_method) <- names(batchnorm_params) <- fb@panel$fcs_colname

  # get batch id from file no
  fno_to_bid <- factor(fb@pheno$batch_id)
  names(fno_to_bid) <- fb@pheno$file_no
  exprs_bid <- fno_to_bid[as.character(fb@exprs[, "file_no"])]
  rm(fno_to_bid)

  # channels
  if (missing(channels)) {
    channels <- which(!is.na(batchnorm_method))
    channels <- fb@panel$fcs_colname[channels]
  }

  models <- fb@procs$batchnorm_funs

  for (chn in channels) {
    if (verbose) message("  ", chn, appendLF = FALSE)
    # parse batchnorm params
    bnp <- fb_split_batch_params(batchnorm_method[chn], batchnorm_params[chn])

    # get data
    all_exprs <- fb_get_exprs(fb, "matrix", transformed = bnp[["transform"]])
    # all_exprs_fid <- all_exprs[, "file_no"]
    all_exprs_fid <- exprs_bid

    for (batch in unique(all_exprs_fid)) {
      row_ids <- batch == all_exprs_fid
      # table(row_ids)
      fb@exprs[row_ids, chn] <-
        models[[chn]][[batch]](all_exprs[row_ids, chn])
    }

    # back to original data scale
    if (fb@options$transformed != bnp[["transform"]]) {
      # reverse transformation is needed
      fun_id <- match(chn, names(fb@options$transforms))
      if (!is.na(fun_id))
        fb@exprs[, chn] <- (fb@options$revtransforms[[fun_id]])(fb@exprs[, chn])
    }

  }
  if (verbose) message("\nDone")
  fb
}


#' @title fb_correct_batch_fcs
#'
#' @description Adjust batch effects using the modelling of reference samples.
#'   Apply to FCS files
#'
#' @param fb flowBunch.
#' @param file_ids ???.
#' @param channels strings, name of channels to display.
#' @param verbose integer, verbosity level.
#'
#' @export

fb_correct_batch_fcs <- function(
  fb,
  file_ids,
  channels,
  verbose = 1
) {
  if (verbose) message("Correcting batch effects (fcs)...")

  # batchnorm params
  batchnorm_method <- fb@panel$batchnorm_method
  batchnorm_params <- fb@panel$batchnorm_params
  names(batchnorm_method) <- names(batchnorm_params) <- fb@panel$fcs_colname

  # channels
  if (missing(channels)) {
    channels <- which(!is.na(batchnorm_method))
    channels <- fb@panel$fcs_colname[channels]
  }

  # file_ids
  if (missing(file_ids)) {
    file_nos <- fb@pheno$file_no
  } else {
    assertIntegerish(file_ids, lower = 1, upper = max(fb@pheno$file_no))
    matched <- match(file_ids, fb@pheno$file_no)
    if (any(is.na(matched)))
      stop("File ids ", file_ids[is.na(matched)], "not found!")
    file_nos <- file_ids
  }

  models <- fb@procs$batchnorm_funs

  # finally apply batch adjustment
  for (file_no in file_nos) {
    i <- match(file_no, fb@pheno$file_no)
    batch_id <- fb@pheno$batch_id[i]
    if (verbose)
      message(sprintf("%d: %s of %s", i, fb@pheno$sample_id[i], batch_id))
    # read FCS
    my_fb_single <- fb_read_fcs(fb, ret = "flowFrame",
                                file_ids = file_no, sampling = "none")
    # process channels
    for (chn in channels) {
      #chn = "Cd116Di"
      if (verbose > 1) message("  ", chn, appendLF = FALSE)
      #browser()
      # parse batchnorm params
      bnp <- fb_split_batch_params(batchnorm_method[chn], batchnorm_params[chn])
      # get data
      all_exprs <- fb_get_exprs(my_fb_single, "matrix", transformed = bnp[["transform"]])
      # TODO: manage back transform
      # TODO: manage zero
      all_exprs[, chn] <- models[[chn]][[batch_id]](all_exprs[, chn])
      # TODO: build a setter function to push expression in default mode
      # if (fb@options$transformed != bnp[["transform"]]) {
      #   # reverse transformation is needed
      #   fun_id <- match(chn, names(fb@options$transforms))
      #   if (!is.na(fun_id))
      #     fb@exprs[, chn] <- (fb@options$revtransforms[[fun_id]])(fb@exprs[, chn])
      # }
      my_fb_single@exprs <- all_exprs
    }
    if (verbose > 1) message()  # append LF
    # write.FCS
    fb_write_fcs(my_fb_single, file_no)
  }

  if (verbose) message("\nDone")
  fb
}

# TODO: save/load model

# TODO: save flowBunch to FCS files

# TODO: fb_setup_batch_from_previous: reload pattern, panel norm & transform
