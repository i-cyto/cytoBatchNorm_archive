server <- function(input, output, session) {

  #
  if(!exists('roots', envir = parent.env(environment()), inherits = FALSE)) {
    roots <- c(Data="C:/")
  }

  # Not reactive values


  # Reactive values
  mem <- reactiveValues(
    my_fb = NULL,
    my_fb_ref = NULL
  )

  prefs <- reactiveValues(
    ui_plot_height = 400
  )

  ui <- reactiveValues(
    set_ui_channels = 0,
    sample_changed = 0,
    transf_changed = 0,
    batchf_changed = 0
  )

  debugging <- reactive({
    input$debug_flag
  })
  observeEvent(input$debug_button, {
    browser()
  })

  # ========== CREATE

  shinyDirChoose(
    input, 'create_fcs_dir', roots = roots)
  shinyDirChoose(
    input, 'create_proj_dir', roots = roots)

  # create_create_button <- eventReactive(input$create_create_button, {})
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
    withCallingHandlers({
      shinyjs::html("create_log", "\n")
      my_fb <- fb_initiate(proj_name, proj_dir, fcs_dir, cytometer)
    },
    message = function(m) {
      shinyjs::html(id = "create_log", html = m$message, add = TRUE)
    })
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

  # TODO: add this part to define default values
  output$setup_ui_batch <- renderUI({
    my_fb <- isolate(mem$my_fb)
    req(my_fb)
    cytometer <- my_fb@options$cytometer
    req(cytometer)
    batchnorm_defaults <- get_batchnorm_defaults(cytometer)
    tagList(
      selectInput(
        "setup_batch_method",
        "Set the method to adjust batch effect",
        choices = c("percentile_hi", "percentile_lohi"),
        selected = batchnorm_defaults[["method"]]
      ),
      textInput(
        "setup_batch_params",
        "Set the percentile to adjust batch effect",
        batchnorm_defaults[["params"]]
      ),
      checkboxInput(
        "setup_batch_zero",
        "Exclude the zeroes in percentiles",
        batchnorm_defaults[["exclude_zeroes"]]),
      checkboxInput(
        "setup_batch_transf",
        "Apply to transformed intensities",
        batchnorm_defaults[["transform"]])
    )
  })

  #
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
    mem$my_fb <- my_fb
  })

  observeEvent(input$setup_explorer, {
    my_fb <- mem$my_fb
    req(my_fb)
    fb_path <- file.path(my_fb@output$path, my_fb@output$name)
    if (.Platform[['OS.type']] == "windows") {
      shell.exec(fb_path)
    } else {
      message(fb_path)
    }
  })

  observeEvent(input$setup_reload_button, {
    my_fb <- mem$my_fb
    req(my_fb)
    # <<< DEBUGGING
    if (my_fb@output$name == "run3") {
      file.copy("C:/data/active/201107-OM_cytofbatchadjust/run3-pheno.xlsx",
                "C:/data/active/201107-OM_cytofbatchadjust/run3/",
                overwrite = TRUE)
      file.copy("C:/data/active/201107-OM_cytofbatchadjust/run3-panel.xlsx",
                "C:/data/active/201107-OM_cytofbatchadjust/run3/",
                overwrite = TRUE)
      file.copy("C:/data/cyto_datasets/spectre/Alignment_workflow/run3/run3-pheno - Copie.xlsx",
                "C:/data/cyto_datasets/spectre/Alignment_workflow/run3/run3-pheno.xlsx",
                overwrite = TRUE)
      file.copy("C:/data/cyto_datasets/spectre/Alignment_workflow/run3/run3-panel - Copie.xlsx",
                "C:/data/cyto_datasets/spectre/Alignment_workflow/run3/run3-panel.xlsx",
                overwrite = TRUE)
    }
    # DEBUGGING >>>
    my_fb <- fb_reload_from_disk(my_fb)
    mem$my_fb <- my_fb
    # Erase any ref and transformation
    mem$my_fb_ref <- NULL
  })

  output$setup_set <- renderText({
    req(mem$my_fb)
    fb_info(mem$my_fb)
  })

  output$setup_pheno_table <- renderDataTable(
    options = list(pageLength = 20),
    {
      req(mem$my_fb, mem$my_fb@pheno)
      mem$my_fb@pheno
    }
  )

  output$setup_panel_table <- renderDataTable(
    options = list(pageLength = 20),
    {
      req(mem$my_fb, mem$my_fb@panel)
      mem$my_fb@panel
    }
  )

  # ========== TUNE

  # When the sample button is pushed,
  # Then sample cells
  observeEvent(input$tune_sample_button, {
    warning("Update UI sample")
    if (debugging()) browser()
    # load data to assess density plot
    tune_load_ncells <- as.integer(input$tune_load_ncells)
    validate(
      need(tune_load_ncells > 1000, 'Number of cells > 1000!'))
    my_fb <- mem$my_fb
    req(my_fb)

    # extract the bunch of reference FCS
    my_fb_ref <- fb_extract_batch_references(
      my_fb
    )

    # load data to assess density plot
    my_fb_ref <- fb_read_fcs(
      my_fb_ref, sampling = "ceil",
      n_cells = tune_load_ncells)

    # set or update my_fb_ref
    if (is.null(mem$my_fb_ref)) {
      mem$my_fb_ref <- my_fb_ref
      ui$set_ui_channels <- ui$set_ui_channels + 1
    } else {
      # Copy transformations
      mem$my_fb_ref@exprs <- my_fb_ref@exprs
      #>> ui$sample_changed <- ui$sample_changed + 1
    }
    #mem$panel_flag <- runif(1)

    # Update the UI channels
    # should be done previously
    warning("Update UI channel")
    ok <- !is.na((my_fb_ref@panel$batchnorm_method))
    channels <- my_fb_ref@panel$antigen[ok]
    names(channels) <- channels
    # TODO: use row_no instead of name in case of duplicated marker name
    updateSelectizeInput(
      session, "tune_channel",
      choices = channels)

    # trigger update plots
    ui$sample_changed <- ui$sample_changed + 1
  })

  # Fill the UI of channel selection with channels to be correct
  output$tune_ui_channel <- renderUI({
    warning("Update UI channel")
    ui$set_ui_channels
    my_fb_ref <- isolate(mem$my_fb_ref) ###
    req(my_fb_ref)
    if (debugging()) browser()
    ok <- !is.na((my_fb_ref@panel$batchnorm_method))
    channels <- my_fb_ref@panel$antigen[ok]
    #names(channels) <- my_fb_ref@panel$antigen[ok]
    names(channels) <- channels
    # TODO: use row_no instead of name in case of duplicated marker name
    selectizeInput(
      "tune_channel",
      "Select the channel to process",
      choices = channels)
  })

  tune_ui_channel <- reactive({
    channel <- input$tune_channel
    req(channel)
    validate(need(nchar(as.character(channel)) > 3,
                  "Select a channel in the menu."))
    if (debugging()) browser()
    my_fb_ref <- isolate(mem$my_fb_ref)
    req(my_fb_ref)
    idx <- guess_match_channels(my_fb_ref, channel)
    channel <- my_fb_ref@panel$fcs_colname[idx]
    list(idx = idx, channel = channel)
  })


  #
  observeEvent(
    tune_ui_channel(), {
      channel <- tune_ui_channel()
      req(channel)
      if (debugging()) browser()
      my_fb_ref <- mem$my_fb_ref
      req(my_fb_ref)
      idx <- channel$idx
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
    }
  )

  # Set the UI for the plots
  output$tune_ui_plots <- renderUI({
    warning("Update UI plots")
    req(prefs)
    tagList(
      plotOutput(
        "tune_main_plot", width = "100%",
        height = prefs$ui_plot_height
      ),
      plotOutput(
        "tune_main_plot_raw", width = "100%",
        height = prefs$ui_plot_height
      )
    )
  })

  # If method or parameter of transformation change,
  # Then update fb_ref
  observeEvent({
    c(input$tune_transf_method, input$tune_transf_params)
  }, {
    warning("Change of tune_transf_X")
    if (debugging()) browser()
    transf_method <- input$tune_transf_method
    transf_params <- input$tune_transf_params
    req(transf_method)
    req(transf_params)
    message(transf_method, "-", transf_params)
    tune_channel <- (isolate(tune_ui_channel()))$channel
    req(tune_channel)
    my_fb_ref <- isolate(mem$my_fb_ref)
    req(my_fb_ref)
    debug_param("tune_transf")
    my_fb_ref <- transf_set_parameters(
      my_fb_ref,
      transf_method = transf_method,
      transf_params = transf_params,
      overwrite = TRUE,
      channels = tune_channel
    )
    mem$my_fb_ref <- my_fb_ref
    ui$transf_changed <- ui$transf_changed + 1
  })

  debug_param <- function(msg) {
    isolate({
      warning(paste(
        msg,
        input$tune_batch_method,
        input$tune_batch_params,
        input$tune_transf_method,
        input$tune_transf_params,
        (tune_ui_channel())$channel))
    })
  }

  # If method or parameters of batch modeling change,
  # Then update fb_ref
  observeEvent({
    c(input$tune_batch_method, input$tune_batch_params,
      input$tune_batch_zero, input$tune_batch_transf)
  }, {
    warning("Change of tune_batch_X")
    if (debugging()) browser()
    batch_method <- input$tune_batch_method
    batch_params <- input$tune_batch_params
    if (input$tune_batch_zero)
      batch_params <- paste0(batch_params, ",exclude_zeroes")
    if (input$tune_batch_transf)
      batch_params <- paste0(batch_params, ",transform")
    req(batch_method)
    req(batch_params)
    message(batch_method, "-", batch_params)
    tune_channel <- (tune_ui_channel())$channel
    req(tune_channel)
    my_fb_ref <- isolate(mem$my_fb_ref)
    req(my_fb_ref)
    debug_param("tune_batch")
    my_fb_ref <- batch_set_parameters(
      my_fb_ref,
      batchnorm_method = batch_method,
      batchnorm_params = batch_params,
      overwrite = TRUE,
      channels = tune_channel
    )
    mem$my_fb_ref <- my_fb_ref
    ui$batchf_changed <- ui$batchf_changed + 1
  })

  output$tune_main_plot <- renderPlot({
    if (debugging()) browser()
    warning("Plot")
    c(ui$sample_changed, ui$transf_changed, ui$batchf_changed)
    channel <- (tune_ui_channel())$channel
    req(channel)
    my_fb_ref <- isolate(mem$my_fb_ref) ###
    req(my_fb_ref)
    # validate(need(my_fb_ref@exprs,
    #               "Click Sample button to retrieve cells from each file."))
    #req(my_fb_ref@exprs)
    warning(paste(ui$sample_changed, ui$transf_changed, ui$batchf_changed, channel))

    debug_param("Plot")

    my_fb_ref <- fb_model_batch(
      my_fb_ref,
      channels = channel
    )
    my_fb_ref <- fb_correct_batch(
      my_fb_ref,
      channels = channel
    )
    fb_plot_ridgelines(
      my_fb_ref,
      channel,
      cof = 8,
      cut_lower_than = -5
    )
  })

  output$tune_main_plot_raw <- renderPlot({
    if (debugging()) browser()
    warning("Plot Raw")
    c(ui$sample_changed, ui$transf_changed)
    channel <- (tune_ui_channel())$channel
    req(channel)
    my_fb_ref <- isolate(mem$my_fb_ref) ###
    req(my_fb_ref)
    validate(need(my_fb_ref@exprs,
                  "Click Sample button to retrieve cells from each file."))
    #req(my_fb_ref@exprs)
    warning(paste(ui$sample_changed, ui$transf_changed, ui$batchf_changed, channel))
    debug_param("Plot Raw")

    fb_plot_ridgelines(
      my_fb_ref,
      channel,
      cof = 8,
      cut_lower_than = -5
    )
  })

  output$tune_main_plot2 <- renderPlot({pairs(iris[,-5])})

  # ========== PROCESS

  # TODO: store PDF before/after: histograms, bi-param?
  # TODO: define the output directories
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
    my_fb_ref <- fb_model_batch(my_fb_ref)
    my_fb@procs <- my_fb_ref@procs
    my_fb <- fb_freeze_file_no(my_fb)
    # apply models
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
    # store
    fb_write(my_fb)
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
