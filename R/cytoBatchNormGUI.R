#' @title cytoBatchNormGUI
#'
#' @description Run the Graphical User Interface.
#'
#' @param roots string, paths to use for loading FCS files and storing the
#'   resulting FCS files.
#' @param ... parameters passed to batch_set_parameters.
#'
#' @importFrom shiny shinyApp runApp
#' @export

cytoBatchNormGUI <- function(roots = c(data = getwd()), debug = FALSE, ...) {
  source(system.file('shiny/globals.R', package = "cytoBatchNorm"))
  # TODO: check the env of globals is shared
  shiny_env <- new.env()
  assign('roots', roots, shiny_env)
  assign('debug', debug, shiny_env)
  source(system.file('shiny/ui.R', package = "cytoBatchNorm"))
  source(system.file('shiny/server.R', package = "cytoBatchNorm"))
  environment(ui) <- shiny_env
  environment(server) <- shiny_env
  app <- shinyApp(ui = ui, server = server)
  runApp(app, ...)
}
