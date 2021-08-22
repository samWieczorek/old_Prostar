library(DAPAR)

source(file.path("../server","mod_download_btns.R"), local=TRUE)$value

ui <- fluidPage(
  tagList(
    mod_download_btns_ui('test')
    
  )
)



server <- function(input, output, session) {
  utils::data(Exp1_R25_prot, package='DAPARdata')
  obj <- Exp1_R25_prot
  
  mc <- metacell.def(GetTypeofData(obj))
  colors <- as.list(setNames(mc$color, mc$node))

  
  mod_download_btns_server(id = 'test',
                           df.data = reactive({Biobase::exprs(obj)}), 
                           name = reactive({'toto'}), 
                           colors = reactive({colors}),
                           df.tags = reactive({GetMetacell(obj)})
                           )
}


shinyApp(ui, server)


