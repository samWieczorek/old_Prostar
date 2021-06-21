library(shinyBS)

source(file.path("../server","mod_LegendColoredExprs.R"), local=TRUE)$value

ui <- fluidPage( mod_LegendColoredExprs_ui('test'))



server <- function(input, output, session) {
  mod_LegendColoredExprs_server(id = 'test')
}


shinyApp(ui, server)


