library(DAPAR)
library(DT)
library(shinyBS)


source(file.path("../modules/Plots","mod_MSnSetExplorer.R"), local=TRUE)$value
source(file.path("../server","mod_LegendColoredExprs.R"), local=TRUE)$value
source(file.path("../server","mod_download_btns.R"), local=TRUE)$value


ui <- fluidPage(mod_MSnSetExplorer_ui('test'))



server <- function(input, output, session) {
  utils::data(Exp1_R25_prot, package = 'DAPARdata')
  obj <- Exp1_R25_prot
  
  mod_MSnSetExplorer_server(id = 'test',
                           data = reactive({obj}),
                           digits = reactive({3}),
                           palette.conds = reactive({DAPAR::ExtendPalette(2)})
                           )
  
}


shinyApp(ui, server)


