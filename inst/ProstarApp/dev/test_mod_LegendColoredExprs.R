library(shinyBS)

source(file.path("../server","mod_LegendColoredExprs.R"), local=TRUE)$value

ui <- fluidPage( mod_LegendColoredExprs_ui('test'))



server <- function(input, output, session) {
  
  utils::data(Exp1_R25_prot, package='DAPARdata')
  obj <- Exp1_R25_prot
  
  mod_LegendColoredExprs_server(id = 'test',
                                obj = reactive({obj}))
}


shinyApp(ui, server)


