library(DAPAR)
library(shinyjs)
library(DT)

source(file.path("../server","mod_staticDT.R"), local=TRUE)$value
source(file.path("../server","mod_download_btns.R"), local=TRUE)$value


ui <- fluidPage(
   mod_staticDT_ui('test')
)



server <- function(input, output, session) {
  utils::data(Exp1_R25_prot, package='DAPARdata')
  obj <- Exp1_R25_prot
  
  
  
  mod_staticDT_server(id = 'test',
                      data = reactive({head(exprs(obj))}), 
                      withDLBtns = TRUE
                      )
}


shinyApp(ui, server)


