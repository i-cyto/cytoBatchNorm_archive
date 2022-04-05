server <- function(input, output, session) {

  # Get variables passed through the environment
  if(!exists('roots', envir = parent.env(environment()), inherits = FALSE)) {
    roots <- c(Data="C:/")
  }
  if(!exists('debug', envir = parent.env(environment()), inherits = FALSE)) {
    debug <- FALSE
  }

  # Not reactive values


  # Reactive values for flowbunch
  mem <- reactiveValues(
    my_fb = NULL,
    my_fb_ref = NULL
  )

  # Reactive values to store user preferences
  prefs <- reactiveValues(
    ui_plot_height = 400
  )


  # ========== DEBUG

  # display debugging UI in debug mode
  output$debug_ui <- renderUI({
    req(debug)
    tags$div(
      class="form-group shiny-input-container",
      actionButton(
        "debug_button",
        "Debug"
      ),
      checkboxInput(
        "debug_flag",
        "Debugging mode"
      ))
  })

  # activate debugging in some functions
  debugging <- reactive({
    if (debug) input$debug_flag else FALSE
  })

  # interrupt the code to view values using isolate()
  observeEvent(input$debug_button, {
    browser()
  })


  # ========== CREATE

  shinyDirChoose(
    input, 'create_fcs_dir', roots = roots)
  shinyDirChoose(
    input, 'create_proj_dir', roots = roots)

  # create_create_button <- eventReactive(input$create_create_button, {})
  # TODO: validate are not output
  observeEvent(input$create_create_button, {
    validate(
      need(input$create_proj_name, 'Check project name!'),
      need(input$create_proj_dir, 'Check project directory!'),
      need(input$create_fcs_dir, 'Check directory of FCS!')
    )
    proj_name <- input$create_proj_name
    proj_dir <- parseDirPath(roots = roots, input$create_proj_dir)
    fcs_dir <- parseDirPath(roots = roots, input$create_fcs_dir)
    cytometer <- input$create_cytometer
    # check FCS files present
    ok <- length(dir(path = fcs_dir, "\\.[Ff][Cc][Ss]$")) > 0
    if (!ok) {
      showNotification("No FCS in selected dir?", type = "error")
      req(ok)
    }
    showNotification("Scan started", type = "message")
    withCallingHandlers({
      shinyjs::html("create_log", "\n")
      my_fb <- fb_initiate(proj_name, proj_dir, fcs_dir, cytometer)
    },
    message = function(m) {
      shinyjs::html(id = "create_log", html = m$message, add = TRUE)
    })
    showNotification("Scan finished", type = "message")
    # TODO: update UI for default processing parameters
    mem$my_fb <- my_fb
  })

  output$create_set <- renderText({
    req(mem$my_fb)
    fb_info(mem$my_fb)
  })

  output$create_pheno_table <- renderDataTable(
    options = list(pageLength = 20),
    {
      req(mem$my_fb, mem$my_fb@pheno)
      mem$my_fb@pheno
    }
  )

  output$create_panel_table <- renderDataTable(
    options = list(pageLength = 20),
    {
      req(mem$my_fb, mem$my_fb@panel)
      mem$my_fb@panel
    }
  )

  # ========== SETUP

  # during the setup, the user fill batch_id, anchor and reference columns of
  # the pheno slot.

  # TODO: test button for regular expressions

  # Finalize the setup and store pheno and panel as files that the user will
  # edit and check
  observeEvent(input$setup_setup_button, {
    batch_pattern <- input$setup_batch_pattern
    ref_sample_pattern <- input$setup_ref_sample_pattern
    validate(
      need(batch_pattern, 'Batch pattern!'),
      need(ref_sample_pattern, 'Sample pattern!')
    )
    req(batch_pattern, ref_sample_pattern)
    my_fb <- isolate(mem$my_fb)
    req(my_fb)

    withCallingHandlers({
      shinyjs::html("setup_log", "\n")
      my_fb <- fb_setup_batch(
        my_fb,
        batch_pattern,
        ref_sample_pattern)
      message("Done!")
    },
    message = function(m) {
      shinyjs::html(id = "setup_log", html = m$message, add = TRUE)
    })
    showNotification("Pheno and panel files written to disk", type = "message")
    showNotification("Edit and verify pheno and panel files", type = "warning")
    mem$my_fb <- my_fb
  })

  # This opens an explorer to ease the editing of pheno and panel files
  observeEvent(input$setup_explorer, {
    my_fb <- mem$my_fb
    req(my_fb)
    fb_path <- file.path(my_fb@output$path, my_fb@output$name)
    if (.Platform[['OS.type']] == "windows") {
      shell.exec(fb_path)
      showNotification("A file explorer is opened", type = "message")
    } else {
      message(fb_path)
      showNotification("A file explorer is not available on this OS", type = "message")
    }
  })

  # Once edited, pheno and panel are loaded back
  observeEvent(input$setup_reload_button, {
    my_fb <- mem$my_fb
    req(my_fb)
    # <<< DEBUGGING
    if (debug) {
      f_backup <- fb_file_name(my_fb, "../%s-pheno.xlsx")
      if (file.exists(f_backup))
        file.copy(f_backup, overwrite = TRUE,
                  fb_file_name(my_fb, "%s-pheno.xlsx"))
      f_backup <- fb_file_name(my_fb, "../%s-panel.xlsx")
      if (file.exists(f_backup))
        file.copy(f_backup, overwrite = TRUE,
                  fb_file_name(my_fb, "%s-panel.xlsx"))
    }
    # DEBUGGING >>>
    my_fb <- fb_reload_from_disk(my_fb)
    mem$my_fb <- my_fb
    # Erase any ref and transformation
    mem$my_fb_ref <- NULL
    showNotification("Pheno and panel files reloaded from disk", type = "message")
    # Check batch information in pheno is correct
    errors <- fb_check_pheno(my_fb)
    # output errors
    msg <- paste0(tags$br(), "===== ", date(), tags$br(), tags$br())
    if (length(errors)) {
      msg <- paste0(msg, errors, collapse = "<br/>")
      showNotification("Pheno file is incorrect. Check Log.", type = "error")
    } else {
      msg <- paste0(msg, "Pheno is correct.")
      showNotification("Pheno file is correct.", type = "message")
    }
    shinyjs::html(id = "setup_log", html = msg, add = TRUE)
  })

  output$setup_set <- renderText({
    req(mem$my_fb)
    tags$pre(fb_info(mem$my_fb))
  })

  output$setup_pheno_table <- renderDataTable(
    options = list(pageLength = 20),
    {
      req(mem$my_fb, mem$my_fb@pheno)
      pheno <- mem$my_fb@pheno
      pheno[, colnames(pheno) %in%
              c("sample_id", "batch_id", "sample_is_ref", "batch_is_ref")]
    }
  )

  output$setup_panel_table <- renderDataTable(
    options = list(pageLength = 20),
    {
      req(mem$my_fb, mem$my_fb@panel)
      mem$my_fb@panel
    }
  )

  # # Check batch information in pheno is correct
  # observeEvent(input$setup_reload_button, {
  #   my_fb <- mem$my_fb
  #   req(my_fb)
  #
  #   errors <- fb_check_pheno(my_fb)
  #   # output errors
  #   msg <- paste0(tags$br(), "===== ", date(), tags$br(), tags$br())
  #   if (length(errors)) {
  #     msg <- paste0(msg, errors, collapse = "<br/>")
  #     showNotification("Pheno file is incorrect. Check Log.", type = "error")
  #   } else {
  #     msg <- paste0(msg, "Pheno is correct.")
  #     showNotification("Pheno file is correct.", type = "message")
  #   }
  #   shinyjs::html(id = "setup_log", html = msg, add = TRUE)
  # })


  # ========== TUNE

  # When the sample button is pushed,
  # Then sample cells
  observeEvent(input$tune_sample_button, {
    warning("Update UI sample")
    if (debugging()) browser()
    # load data to assess density plot
    tune_load_ncells <- as.integer(input$tune_load_ncells)
    req(tune_load_ncells)
    err <- tune_load_ncells < 1000
    if (err) {
      showNotification("Not enough cells.", type = "error")
      req(err)  #, cancelOutput = TRUE)
    }
    my_fb <- mem$my_fb
    req(my_fb)

    # extract the bunch of reference FCS
    my_fb_ref <- fb_extract_batch_references(
      my_fb
    )

    # load data to assess density plot
    showNotification("Sampling started", type = "message")
    my_fb_ref <- fb_load_cells(
      my_fb_ref, n_cells = tune_load_ncells
    )
    showNotification("Sampling finished", type = "message")

    # set or update my_fb_ref
    if (is.null(mem$my_fb_ref)) {
      mem$my_fb_ref <- my_fb_ref
    } else {
      # Copy expressions
      mem$my_fb_ref@exprs <- my_fb_ref@exprs
    }

    # Update the UI channels
    # should be done previously
    # TODO: trigger on my_fb instead of my_fb_ref
    warning("Update UI channel")
    ok <- !is.na((my_fb_ref@panel$batchnorm_method))
    channels <- my_fb_ref@panel$antigen[ok]
    names(channels) <- channels
    # TODO: use row_no instead of name in case of duplicated marker name
    updateSelectizeInput(
      session, "tune_channel",
      choices = channels)
    channel <- input$tune_channel
    if (isFALSE(channel == "")) channel <- channels[1]
    updateSelectizeInput(
      session, "tune_channel",
      selected = channel)
    updateSelectizeInput(
      session, "revtran_channel",
      choices = channels,
      selected = input$revtran_channel)
    updateSelectizeInput(
      session, "revcoef_channel",
      choices = channels,
      selected = input$revcoef_channel)
    updateSelectizeInput(
      session, "revbipl_channel_x",
      choices = channels,
      selected = input$revbipl_channel_x)
    updateSelectizeInput(
      session, "revbipl_channel_y",
      choices = channels,
      selected = input$revbipl_channel_y)

    warning("Update UI batch")
    batches <- my_fb_ref@pheno$file_no
    names(batches) <- my_fb_ref@pheno$batch_id
    updateSelectizeInput(
      session, "revtran_batch",
      choices = batches,
      selected = input$revtran_batch)
    updateSelectizeInput(
      session, "revcoef_batch",
      choices = batches,
      selected = input$revcoef_batch)
    updateSelectizeInput(
      session, "revbipl_batch",
      choices = batches,
      selected = input$revbipl_batch)
  })

  tune_channel <- reactive({
    channel <- input$tune_channel
    req(channel)
    validate(need(nchar(as.character(channel)) > 2,
                  "Select a channel in the menu."))
    if (debugging()) browser()
    my_fb_ref <- isolate(mem$my_fb_ref)
    req(my_fb_ref)
    idx <- guess_match_channels(my_fb_ref, channel)
    channel <- my_fb_ref@panel$fcs_colname[idx]

    warning("Update UI batch")
    bnp <- fb_split_batch_params(
      my_fb_ref@panel$batchnorm_method[idx],
      my_fb_ref@panel$batchnorm_params[idx]
    )
    freezeReactiveValue(input, "tune_batch_method")
    freezeReactiveValue(input, "tune_batch_params")
    freezeReactiveValue(input, "tune_batch_zero")
    freezeReactiveValue(input, "tune_batch_transf")
    updateSelectInput(
      session, "tune_batch_method",
      selected = bnp[["method"]])
    updateTextInput(
      session, "tune_batch_params",
      value = bnp[["params"]])
    updateCheckboxInput(
      session, "tune_batch_zero",
      value = bnp[["exclude_zeroes"]])
    updateCheckboxInput(
      session, "tune_batch_transf",
      value = bnp[["transform"]])

    warning("Update UI transf")
    method <- my_fb_ref@panel$transf_method[idx]
    params <- my_fb_ref@panel$transf_params[idx]
    freezeReactiveValue(input, "tune_transf_method")
    freezeReactiveValue(input, "tune_transf_params")
    updateSelectInput(
      session, "tune_transf_method",
      selected = method
    )
    updateTextInput(
      session, "tune_transf_params",
      value = params
    )

    warning("Update UI transf")
    # freezeReactiveValue(input, "revcoef_channel")
    # freezeReactiveValue(input, "revtran_channel")
    # freezeReactiveValue(input, "revbipl_channel_x")
    updateSelectizeInput(session, "revcoef_channel", selected = input$tune_channel)
    updateSelectizeInput(session, "revtran_channel", selected = input$tune_channel)
    updateSelectizeInput(session, "revbipl_channel_x", selected = input$tune_channel)

    list(idx = idx, channel = channel)
  })

  # Set the UI for the plots
  output$tune_ui_plots <- renderUI({
    warning("Update UI plots")
    height <- input$tune_plot_height
    req(prefs)
    tagList(
      plotOutput(
        "tune_main_plot", width = "100%", height = height
      ),
      plotOutput(
        "tune_main_plot_raw", width = "100%", height = height
      )
    )
  })

  # If method or parameters of batch modeling change,
  # Then update fb_ref
  my_fb_ref <- reactive({
    # retrieve channel, transformation batch
    tune_channel <- (tune_channel())$channel
    req(tune_channel)
    transf_method <- input$tune_transf_method
    transf_params <- input$tune_transf_params
    req(transf_method)
    req(transf_params)
    message(transf_method, "-", transf_params)
    batch_method <- input$tune_batch_method
    batch_params <- input$tune_batch_params
    # check
    bp <- as.numeric(strsplit(batch_params, ",\\s*")[[1]])
    tune_load_ncells <- as.integer(isolate(input$tune_load_ncells))
    req(tune_load_ncells)
    err <- min(c(bp, 1-bp) * tune_load_ncells) < 10
    if (err) {
      showNotification("Too extreme percentiles.", type = "error")
      req(err, cancelOutput = TRUE)
    }
    if (input$tune_batch_zero)
      batch_params <- paste0(batch_params, ",exclude_zeroes")
    if (input$tune_batch_transf)
      batch_params <- paste0(batch_params, ",transform")
    req(batch_method)
    req(batch_params)
    message(batch_method, "-", batch_params)
    # retrieve FB and apply parameters
    my_fb_ref <- isolate(mem$my_fb_ref)
    req(my_fb_ref)
    my_fb_ref <- transf_set_parameters(
      my_fb_ref,
      transf_method = transf_method,
      transf_params = transf_params,
      overwrite = TRUE,
      channels = tune_channel
    )
    my_fb_ref <- batch_set_parameters(
      my_fb_ref,
      batchnorm_method = batch_method,
      batchnorm_params = batch_params,
      overwrite = TRUE,
      channels = tune_channel
    )
    # fit model to correct batch effect and store model
    my_fb_ref_mod <- fb_model_batch(
      my_fb_ref,
      channels = tune_channel
    )
    mem$my_fb_ref <- my_fb_ref_mod
    # return updated FB
    my_fb_ref_mod
  })

  output$tune_main_plot <- renderPlot({
    if (debugging()) browser()
    warning("Plot")
    channel <- (tune_channel())$channel
    req(channel)
    my_fb_ref <- my_fb_ref()
    req(my_fb_ref)

    # correct batch effect and plot
    my_fb_ref_adj <- fb_correct_batch(
      my_fb_ref,
      channels = channel
    )
    fb_plot_ridgelines(
      my_fb_ref_adj,
      channel,
      group_by = "batch_id",
      cof = 8,
      cut_lower_than = -5,
      title = "Normalized"
    )
  })

  output$tune_main_plot_raw <- renderPlot({
    if (debugging()) browser()
    warning("Plot Raw")
    channel <- (tune_channel())$channel
    req(channel)
    my_fb_ref <- my_fb_ref() ###
    req(my_fb_ref)

    # plot
    fb_plot_ridgelines(
      my_fb_ref,
      channel,
      group_by = "batch_id",
      cof = 8,
      cut_lower_than = -5,
      title = "Raw"
    )
  })


  # ========== REVIEW SCALING COEFF

  # Set the UI for the plots
  output$revcoef_ui_plots <- renderUI({
    height <- input$revcoef_plot_height
    req(height)
    tagList(
      # tags$legend("Transformation"),
      plotOutput(
        "revcoef_main_plot", width = "100%", height = height
      )
    )
  })

  revcoef_gdata <- reactive({
    warning("revcoef_main_plot")
    if (isolate(debugging())) browser()
    channel <- input$revcoef_channel
    req(channel)

    my_fb_ref <- my_fb_ref()
    req(my_fb_ref)
    idx <- guess_match_channels(my_fb_ref, channel)
    channel <- my_fb_ref@panel$fcs_colname[idx]
    channel_name <- my_fb_ref@panel$antigen[idx]

    df_raw <- rev_df_raw()
    req(df_raw)
    df_adj <- rev_df_adj()
    req(df_adj)

    # get correction
    batch_params <- my_fb_ref@panel$batchnorm_params[idx]
    # check
    # parse parameters
    percentiles <- as.numeric(strsplit(batch_params, ",\\s+")[[1]])
    req(percentiles)
    # get percentiles
    # get raw intensities of percentiles
    # get transformed intensities of percentiles
    # get corrected transformed intensities of percentiles
    # compute difference
    perc_raw <- tapply(
      df_raw[,channel], df_raw[, "file_no"],
      quantile, probs = percentiles, names = FALSE)
    perc_adj <- tapply(
      df_adj[,channel], df_adj[, "file_no"],
      quantile, probs = percentiles)
    gg_perc <- data.frame(
      file_no = rep(names(perc_raw), each = length(percentiles)),
      percentiles = percentiles,
      raw = unlist(perc_raw),
      adj = unlist(perc_adj)
    )
    gg_perc$percentiles <- percentiles
    gg_perc$coeff <- gg_perc$adj - gg_perc$raw
    # TODO: do a mapping instead of merge
    gg_perc <- merge(gg_perc, my_fb_ref@pheno[, c("file_no", "batch_id")], sort = FALSE)
    list(gg_perc = gg_perc, channel_name = channel_name)
  })

  output$revcoef_main_plot <- renderPlot({
    warning("revcoef_main_plot")
    gdata <- revcoef_gdata()
    req(gdata)
    gg_perc <- gdata$gg_perc
    channel_name <- gdata$channel_name
    file_no <- as.integer(input$revcoef_batch)
    if (length(file_no) == 0)
      file_no <- gdata$gg_perc$file_no
    req(file_no)
    file_nos <- c(file_no)

    # plot difference
    llim <- range(gg_perc$coeff)
    gg <- ggplot(gg_perc, aes_string("batch_id", "coeff")) +
      geom_col(width = 0.2) + #geom_point() +
      ylim(min(-1, llim[1]), max(1, llim[2])) +
      facet_wrap(~percentiles) +
      labs(x = "batch", y = paste0("adjustment of ",  channel_name)) +
      theme_minimal()
    print(gg)
  })


  # ========== REVIEW TRANSFORM

  # Set the UI for the plots
  output$revtran_ui_plots <- renderUI({
    height <- input$revtran_plot_height
    req(height)
    tagList(
      # tags$legend("Transformation"),
      plotOutput(
        "revtran_main_plot", width = "100%", height = height
      )
    )
  })

  rev_df_raw <- reactive({
    fb <- my_fb_ref()
    req(fb)
    fb_get_exprs(fb, "data.frame", transformed = TRUE)
  })

  rev_df_adj <- reactive({
    fb <- my_fb_ref()
    req(fb)
    fb_adj <- fb_model_batch(fb)
    fb_adj <- fb_correct_batch(fb_adj)
    fb_get_exprs(fb_adj, "data.frame", transformed = TRUE)
  })

  output$revtran_main_plot <- renderPlot({
    warning("revtran_main_plot")
    channel <- input$revtran_channel
    req(channel)
    file_no <- as.integer(input$revtran_batch)
    req(file_no)
    jitter <- as.numeric(input$revtran_jitter)
    req(jitter)
    point <- input$revtran_point
    req(point)
    gg_ncol <- as.numeric(input$revtran_ncol)
    req(gg_ncol)

    my_fb_ref <- my_fb_ref()
    req(my_fb_ref)
    idx <- guess_match_channels(my_fb_ref, channel)
    channel <- my_fb_ref@panel$fcs_colname[idx]
    channel_name <- my_fb_ref@panel$antigen[idx]

    if (!is.na(as.integer(point))) point <- as.integer(point)

    df_raw <- rev_df_raw()
    req(df_raw)
    df_adj <- rev_df_adj()
    req(df_raw)

    df <- cbind(df_raw[ , seq(ncol(df_raw)-2)], df_adj)
    colnames(df)[seq(2*(ncol(df_raw)-2))] <- paste0(
      colnames(df)[seq(2*(ncol(df_raw)-2))],
      rep(c("_raw", "_adj"), each = ncol(df_raw)-2))
    mapping <- factor(
      my_fb_ref@pheno$batch_id, levels = my_fb_ref@pheno$batch_id)
    names(mapping) <- my_fb_ref@pheno$file_no
    df$batch_id <- mapping[as.character(df$file_no)]

    if (debugging()) browser()
    file_nos <- c(file_no)
    gg <- ggplot(subset(df, file_no %in% file_nos),
                 aes_string(x = colnames(df)[idx],
                            y = colnames(df)[idx+ncol(df_raw)-2])) +
      geom_jitter(width = jitter, height = jitter,
                  pch = point, cex = 2, col = "#11222222") +
      geom_abline(slope = 1, intercept = 0, col = grey(.3)) +
      theme_minimal() + theme(aspect.ratio = 1) +
      # facet_wrap(~file_no, ncol = gg_ncol) +
      facet_wrap(~batch_id, ncol = gg_ncol) +
      labs(x = paste0(channel_name, " raw"),
           y = paste0(channel_name, " normd"))
    print(gg)
  })


  # ========== REVIEW BI-PARAMETRIC PLOT

  # Set the UI for the plots
  output$revbipl_ui_plots <- renderUI({
    height <- input$revbipl_plot_height
    req(height)
    tagList(
      # tags$legend("Transformation"),
      plotOutput(
        "revbipl_main_plot", width = "100%", height = height
      )
    )
  })

  output$revbipl_main_plot <- renderPlot({
    warning("revbipl_main_plot")
    if (debugging()) browser()
    channel_x <- input$revbipl_channel_x
    req(channel_x)
    channel_y <- input$revbipl_channel_y
    req(channel_y)
    file_no <- as.integer(input$revbipl_batch)
    req(file_no)
    hexbin <- as.numeric(input$revbipl_hexbin)
    req(hexbin)
    aspect <- as.numeric(input$revbipl_aspect)
    req(aspect)

    my_fb_ref <- my_fb_ref()
    req(my_fb_ref)
    idx <- guess_match_channels(my_fb_ref, channel_x)
    channel_x <- my_fb_ref@panel$fcs_colname[idx]
    channel_name_x <- my_fb_ref@panel$antigen[idx]
    idx <- guess_match_channels(my_fb_ref, channel_y)
    channel_y <- my_fb_ref@panel$fcs_colname[idx]
    channel_name_y <- my_fb_ref@panel$antigen[idx]

    df_raw <- rev_df_raw()
    req(df_raw)
    df_adj <- rev_df_adj()
    req(df_raw)

    df_all <- rbind(cbind(df_raw, normed = "raw"),
                    cbind(df_adj, normed = "normd"))
    df_all$normed <- factor(df_all$normed, levels = c("raw", "normd"))
    mapping <- factor(
      my_fb_ref@pheno$batch_id, levels = my_fb_ref@pheno$batch_id)
    names(mapping) <- my_fb_ref@pheno$file_no
    df_all$batch_id <- mapping[as.character(df_all$file_no)]

    file_nos <- c(file_no)
    gg <- ggplot(subset(df_all, file_no %in% file_nos),
                 aes_string(x = channel_x, y = channel_y)) +
      geom_hex(bins = hexbin) +
      scale_fill_gradientn(colours = rev(RColorBrewer::brewer.pal(11, "Spectral")), trans = "sqrt") +
      theme_minimal() + theme(aspect.ratio = aspect) +
      facet_grid(batch_id ~ normed) +
      labs(x = channel_name_x, y = channel_name_y)
    print(gg)
  })

  # TODO: print PDF button
  observeEvent(input$revbipl_print_pdf, {
    # TODO
  })


  # ========== PREVIEW NORMALISATION

  observeEvent(input$proc_preview_button, {
    validate(
      need(input$create_proj_name, 'Check project name!'),
      need(input$create_proj_dir, 'Check project directory!')
    )
    if (debugging()) browser()
    my_fb_ref <- mem$my_fb_ref
    req(my_fb_ref)

    # plot raw, ie before
    showNotification("Preview RAW started")
    pdf(fb_file_name(my_fb_ref, "-refs_raw.pdf"), width = 15, height = 6)
    fb_plot_ridgelines(my_fb_ref, title = "Raw")
    dev.off()
    showNotification("Preview RAW finished")

    # apply models
    showNotification("Normalisation started")
    my_fb_ref_mod <- fb_model_batch(
      my_fb_ref
    )
    my_fb_ref_adj <- fb_correct_batch(
      my_fb_ref_mod
    )
    showNotification("Normalisation finished")

    # plot normed, ie after
    showNotification("Preview NORMD started")
    pdf(fb_file_name(my_fb_ref, "-refs_normed.pdf"), width = 15, height = 6)
    fb_plot_ridgelines(my_fb_ref_adj, group_by = "batch_id", title = "Normd")
    dev.off()
    showNotification("Preview NORMD finished")
  })


  # ========== PROCESS

  observeEvent(input$proc_apply_button, {
    validate(
      need(input$create_proj_name, 'Check project name!'),
      need(input$create_proj_dir, 'Check project directory!'),
      need(input$create_fcs_dir, 'Check directory of FCS!')
    )
    if (debugging()) browser()
    my_fb_ref <- mem$my_fb_ref
    req(my_fb_ref)
    my_fb <- mem$my_fb
    req(my_fb)
    # model and copy transformations
    showNotification("Normalisation started")
    my_fb_ref <- fb_model_batch(my_fb_ref)
    my_fb@procs <- my_fb_ref@procs
    my_fb@panel <- my_fb_ref@panel
    my_fb <- fb_freeze_file_no(my_fb)
    # store before processing
    fb_write(my_fb)
    # apply models
    showNotification("File processing started")
    withCallingHandlers({
      shinyjs::html("proc_log", "\n")
      message(format(Sys.time(), "%a %b %d %Y %X TZ(%z)"), appendLF = TRUE)
      for (i in my_fb@pheno$file_no) {
        fb_correct_batch_fcs(my_fb, file_ids = i)
        showNotification(sprintf("Processing %s", my_fb@pheno$sample_id[i]))
      }
      message(format(Sys.time(), "%a %b %d %Y %X TZ(%z)"), appendLF = TRUE)
    },
    message = function(m) {
      shinyjs::html(id = "proc_log", html = m$message, add = TRUE)
    })
    showNotification("Normalisation finished")
    # create minimal information to build a flowBunch in fcs
    fb_export(my_fb)
  })


  # ========== REVIEW NORMALISATION

  observeEvent(input$proc_review_button, {
    validate(
      need(input$create_proj_name, 'Check project name!'),
      need(input$create_proj_dir, 'Check project directory!')
    )
    if (debugging()) browser()
    my_fb <- mem$my_fb
    req(my_fb)

    # plot raw, ie before
    showNotification("Review RAW started")
    my_fb_raw <- fb_reload(
      my_fb
    )
    # load data to assess density plot
    my_fb_raw <- fb_load_cells(
      my_fb_raw, n_cells = 9000
    )
    # plot raw, ie before normalization
    pdf(fb_file_name(my_fb_raw, "-raw.pdf"), width = 15, height = 8)
    fb_plot_ridgelines(my_fb_raw, title = "Raw")
    dev.off()
    showNotification("Review RAW finished")

    # check process succeeded
    fcs_dir <- file.path(fb_file_name(my_fb), "fcs")
    if (!testDirectoryExists(fcs_dir)) {
      showNotification("Normalized files not found")
      return()
    }

    # plot normed, ie after
    showNotification("Review NORMD started")
    my_fb_adj <- fb_open_(
      project_name = "fcs",
      project_dir = fb_file_name(my_fb)
    )
    # load data to assess density plot
    my_fb_adj <- fb_load_cells(
      my_fb_adj, n_cells = 9000
    )
    # plot after, ie after, in the same project
    pdf(fb_file_name(my_fb, "-normd.pdf"), width = 15, height = 8)
    fb_plot_ridgelines(my_fb_adj, title = "Normd")
    dev.off()
    showNotification("Review NORMD finished")
  })


  # ========== OPTIONS

  # Apply
  observeEvent(input$opt_apply_button, {
    opt_ui_plot_height <- as.integer(input$opt_ui_plot_height)
    validate(
      need(opt_ui_plot_height > 200, 'Integer > 200!')
    )
    opt_ui_plot_height <- max(200, as.integer(opt_ui_plot_height))
    prefs$ui_plot_height <- opt_ui_plot_height
  })

}
