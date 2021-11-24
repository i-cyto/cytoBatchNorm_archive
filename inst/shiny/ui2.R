ui <- dashboardPage(
  dashboardHeader(title = "cytoBatchNorm"),
  dashboardSidebar(
    sidebarMenu(
      menuItem("Create Bunch", tabName = "Create", icon = icon("dashboard")),
      # menuItem("Load",  tabName = "Load", icon = icon("th")),
      menuItem("Setup Batch", tabName = "Setup", icon = icon("dashboard")),
      menuItem("Tune Params", tabName = "Tune", icon = icon("dashboard")),
      menuItem("Process", tabName = "Process", icon = icon("dashboard")),
      menuItem("Options", tabName = "Options", icon = icon("dashboard")),
      # menuItem("Save", tabName = "Save", icon = icon("dashboard")),
      NULL
    ),
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
  ),
  dashboardBody(
    useShinyjs(),
    tabItems(
      tabItem(
        tabName = "Create",
        box(
          title = "Create", width = 12,
          sidebarPanel(
            width = 4,
            textInput(
              "create_proj_name",
              "Set the name of the project",
              "run3"
            ),
            tags$div(
              class="form-group shiny-input-container",
              tags$label("Select a directory to store the project"),
              tags$br(),
              shinyDirButton(
                "create_proj_dir",
                "Browse",
                "Select a directory to store the project"
              )),
            selectInput(
              "create_cytometer",
              "Select the cytometry technology",
              choices = c("mass", "flow", "spectral")
            ),
            tags$div(
              class="form-group shiny-input-container",
              tags$label("Select the directory of all the FCS files"),
              tags$br(),
              shinyDirButton(
                "create_fcs_dir",
                "Browse",
                "Select the directory of all the FCS files"
              )),
            tags$div(
              class="form-group shiny-input-container",
              tags$label("Once done, click below"), tags$br(),
              actionButton(
                "create_create_button",
                "Create"
              ))
          ),

          mainPanel(
            width = 8,
            tabsetPanel(
              id = "create_tabset",
              type = "pills",
              tabPanel("Log", verbatimTextOutput("create_log")),
              tabPanel("Set", verbatimTextOutput("create_set")),
              tabPanel("Pheno", dataTableOutput("create_pheno_table")),
              tabPanel("Panel", dataTableOutput("create_panel_table"))
            )
          )
        )
      ),

      tabItem(
        tabName = "Setup",
        box(
          title = "Setup", width = 12,
          sidebarPanel(
            width = 4,
            tags$fieldset(
              tags$legend("Batch adjust defaults"),
              textInput(
                "setup_batch_pattern",
                "Set the pattern to determine batch identifiers",
                ".+?_Batch(\\d+)_.+"
              ),
              textInput(
                "setup_ref_sample_pattern",
                "Set the pattern to identify reference FCS",
                "_c20\\.fcs$"
              ),
              # ),
              # TODO: test button
              # tags$fieldset(
              # tags$legend("Batch adjust defaults"),
              NULL # uiOutput("setup_ui_batch")
            ),
            tags$div(
              class="form-group shiny-input-container",
              tags$label("Once done, click below"), tags$br(),
              actionButton(
                "setup_setup_button",
                "Finalize"
              )
            ),
            tags$fieldset(
              tags$legend("Edit and Reload from disk"),
              class="form-group shiny-input-container",
              tags$text("Now edit the panel file to set batch model parameters. You could also edit the pheno file."), tags$br(),
              actionButton(
                "setup_explorer",
                "Open project dir"
              ), tags$br(), tags$br(),
              tags$label("Once done, click below"), tags$br(),
              actionButton(
                "setup_reload_button",
                "Reload"
              )
            ),
          ),
          mainPanel(
            width = 8,
            tabsetPanel(
              id = "setup_tabset",
              type = "pills",
              tabPanel("Log", verbatimTextOutput("setup_log")),
              tabPanel("Pheno", dataTableOutput("setup_pheno_table")),
              tabPanel("Panel", dataTableOutput("setup_panel_table"))
            )
          )
        )
      ),

      tabItem(
        tabName = "Tune",
        tabBox(
          id = "Tune_tabset", width = 12,
          title = "Tune",
          tabPanel(
            "Tune",
            fluidRow(
              sidebarPanel(
                width = 4,
                textInput(
                  "tune_load_ncells",
                  "Define the amount of cells per FCS file",
                  5000
                ),
                tags$div(
                  class="form-group shiny-input-container",
                  tags$label("Extract a sample of cells"), tags$br(),
                  actionButton(
                    "tune_sample_button",
                    "Sample"
                  )),
                uiOutput("tune_ui_channel"),
                tags$fieldset(
                  tags$legend("Batch adjust"),
                  uiOutput("tune_ui_batch")
                ),
                tags$fieldset(
                  tags$legend("Transform"),
                  uiOutput("tune_ui_transf")
                )
              ),
              mainPanel(
                width = 8,
                uiOutput("tune_ui_plots")
                # plotOutput("tune_main_plot", width = "100%"),
                # plotOutput("tune_main_plot_raw", width = "100%"),
              )
            )
          ),
          tabPanel(
            "Plot scaling",
            plotOutput("tune_main_plot2")
          )
        )
      ),

      tabItem(
        tabName = "Process",
        box(
          title = "Process", width = 12,
          sidebarPanel(
            width = 4,
            tags$div(
              class="form-group shiny-input-container",
              tags$label("Once you reviewed all channels, click below"), tags$br(),
              actionButton(
                "proc_apply_button",
                "Apply"
              ))
          ),

          mainPanel(
            width = 8,
            tabsetPanel(
              id = "proc_tabset",
              type = "pills",
              tabPanel("Log", verbatimTextOutput("proc_log"))
            )
          )
        )
      ),

      tabItem(
        tabName = "Options",
        box(
          title = "Options", width = 12,
          tags$div(
            sidebarPanel(
              width = 4,
              tags$div(
                class="form-group shiny-input-container",
                tags$label("Graphical options"), tags$br(),
                textInput("opt_ui_plot_height", "Plot height", "600")
              )
            ),
            sidebarPanel(
              width = 4,
              tags$div(
                class="form-group shiny-input-container",
                tags$br()
              )
            ),
            sidebarPanel(
              width = 4,
              tags$div(
                class="form-group shiny-input-container",
                tags$br()
              )
            )
          ),
          tags$div(
            column(
              width = 12,
              actionButton(
                "opt_apply_button",
                "Apply"
              )
            )
          ),
        )
      ),

      tabItem(
        tabName = "Save",
        NULL
      )
    )
  )
)
