source(file.path(".", "modules/moduleStaticDataTable.R"),  local = TRUE)$value
source(file.path(".", "modules/DataManager/moduleConvertData.R"),  local = TRUE)$value
source(file.path(".", "modules/DataManager/moduleOpenMSnSet.R"),  local = TRUE)$value
source(file.path(".", "modules/DataManager/moduleOpenDemoDataset.R"),  local = TRUE)$value
source(file.path(".", "modules/DataManager/moduleInfoDataset.R"),  local = TRUE)$value



moduleOpenDatasetUI  <- function(id){
  ns <- NS(id)
  
  tagList(
    shinyjs::disabled(actionButton(ns('btn_launch'), "Launch pipeline",class = actionBtnClass)),
    tabsetPanel(
    
              tabPanel("Open MSnset",
                      moduleOpenMSnSetUI(ns("moduleOpenMSnSet"))
             ),
             tabPanel("Convert",
                      value = "convertTab",
                      moduleConvertDataUI(ns("moduleProcess_Convert"))
             ),
             tabPanel("Demo data", 
                      moduleOpenDemoDatasetUI(ns("moduleOpenDemoDataset"))
             )
  )
  
  )
}




moduleOpenDataset  <- function(input, output, session, selectedPanel){
  ns <- session$ns
  
  rv.opendataset <- reactiveValues(
    tmp.convert = NULL,
    tmp.demo = NULL,
    tmp.file = NULL,
    obj = NULL,
    dataOut = NULL
  )
  
  
  rv.opendataset$tmp.convert <- callModule(module=moduleConvertData, 'moduleProcess_Convert')
  rv.opendataset$tmp.demo <- callModule(module=moduleOpenDemoDataset, 'moduleOpenDemoDataset')
  rv.opendataset$tmp.file <- callModule(module=moduleOpenMSnSet, 'moduleOpenMSnSet')
  
  
  observe({
    req(rv.opendataset$tmp.file())
    rv.opendataset$obj <- rv.opendataset$tmp.file()
  })
  
  observe({
    req(rv.opendataset$tmp.convert())
    rv.opendataset$obj <- rv.opendataset$tmp.convert()
  })
  
  
  observe({
    req(rv.opendataset$tmp.demo())
    rv.opendataset$obj <- rv.opendataset$tmp.demo()
    print("demo dataset loaded")
  })
  
  
  
  observeEvent(req(rv.opendataset$obj),{
    print("PASS dedans")
    rv.opendataset$obj <- ConfigureData(rv.opendataset$obj)
    if (length(rv.opendataset$obj@AdjacencyMat)==0 && (rv.opendataset$obj@pipeline == 'peptide')){
      rv.opendataset$obj@AdjacencyMat <- ComputeAdjacencyMatrices(rv.opendataset$obj@datasets[[1]])
      pipeline$current.obj@ConnexComp <- ComputeConnexComposants(pipeline$current.obj@AdjacencyMat)
    }
    shinyjs::enable('btn_launch')
  })
  
  
  observeEvent(input$btn_launch, {
    rv.opendataset$dataOut <- rv.opendataset$obj
  })
  
  return(reactive({rv.opendataset$dataOut}))
}