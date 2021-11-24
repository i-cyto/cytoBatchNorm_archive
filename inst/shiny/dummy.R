ui <- fluidPage(
  {
    cols <- 1:4
    names(cols) <- colnames(iris)[cols]
    selectInput("col", "Column to transform", cols)
  },
  textInput("scaling", "Linear scaling", character(0)),
  plotOutput("plot"),
  plotOutput("plot_raw")
)

server <- function(input, output, session) {

  ui <- list(
    scalings = 1:4
  )

  observeEvent(input$col, {
    col <- as.integer(input$col)
    freezeReactiveValue(input, "scaling")
    updateTextInput(session, "scaling", value = ui$scalings[col])
    warning(input$col)
  })

  output$plot <- renderPlot({
    col <- as.integer(input$col)
    sca <- as.numeric(input$scaling)
    req(col)
    req(sca)
    hist(iris[,col] * sca)
    ui$scalings[col] <- sca
    warning("Plot", col, sca)
  })
  output$plot_raw <- renderPlot({
    col <- as.integer(input$col)
    req(col)
    hist(iris[,col])
    warning("Plot Raw", col)
  })

}

server0 <- function(input, output, session) {

  ui <- reactiveValues(
    scalings = 1:4
  )

  observeEvent(input$col, {
    col <- as.integer(input$col)
    updateTextInput(session, "scaling", value = ui$scalings[col])
    freezeReactiveValue(input, "scaling")
    warning(input$col)
  })

  observeEvent(input$scaling, {
    col <- as.integer(input$col)
    sca <- as.numeric(input$scaling)
    req(col)
    req(sca)
    ui$scalings[col] <- sca
    warning("Scaling changed", input$col)
  })

  output$plot <- renderPlot({
    col <- as.integer(input$col)
    sca <- as.numeric(input$scaling)
    req(col)
    req(sca)
    hist(iris[,col] * sca)
    warning("Plot", col, sca)
  })

}

shinyApp(ui, server)
