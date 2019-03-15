rm(list=ls())
options(shiny.maxRequestSize=300*1024^2) 

require(compiler)
enableJIT(3)




onStart = function() {
  cat("Doing application setup\n")
  
  onStop(function() {
    cat("Doing application cleanup\n")
    graphics.off()
    unlink(sessionID, recursive = TRUE)
    unlink(paste(tempdir(), "*", sep="/"),recursive = TRUE)
  })
}


sourceFiles <- function(){
  
}


library(shinyBS)

#' Title
#'
#' @param input 
#' @param output 
#' @param session 
#'
#' @return
#' @export
#'
#' @examples
server <- function(input, output, session){
  
  sourceFiles()
  source(file.path(".", "modules/Plots/modulePlots.R"),  local = TRUE)$value
  source(file.path(".", "modules/moduleBugReport.R"),  local = TRUE)$value
  
  source(file.path(".", "modules/DataManager/moduleDataManager.R"),  local = TRUE)$value
  source(file.path(".", "modules/moduleInsertMarkdown.R"),  local = TRUE)$value
  
  source(file.path(".", "modules/Export/moduleExport.R"),  local = TRUE)$value
  
  # source(file.path(".", "modules/pipelines/modulePipelinePep.R"),  local = TRUE)$value
  # source(file.path(".", "modules/pipelines/modulePipelineProt.R"),  local = TRUE)$value
  # source(file.path(".", "modules/pipelines/modulePipelineP2p.R"),  local = TRUE)$value
   source(file.path(".", "modules/pipelines/moduleGenericPipeline.R"),  local = TRUE)$value
  
  
  loadLibraries()
  
  
  rv <- reactiveValues(
    
    # name of the current dataset in the widget chooseDataset
   
    # current working data from current pipeline
    current.pipeline.data = NULL,
    current.pipeline.indice = NULL,
    current.pipeline = NULL,
    ll.process = c('original'),
    #current indice (rank of current dataset in pipeline)
    indice = 1,
    init.obj = NULL,
    
    #model for the structure of dataset for peptide pipeline
    
    pipeline.gen = callModule(module = moduleGenericPipeline, 'pepe', 
                              initData = reactive({rv$init.obj}), 
                              navPage =reactive({input$navPage}),
                              indice = reactive({rv$current.pipeline.indice}),
                              ll.process = reactive({rv$ll.process}))
     
  )
    
 #####
 ## Launch modules
  obj <- callModule(module = moduleDataManager, 'datamanager')
  
 observeEvent(req(rv$current.pipeline.data,rv$current.pipeline.indice), {
   callModule(module = modulePlots, 'showPlots', 
              dataIn=reactive({rv$current.pipeline.data[[rv$current.pipeline.indice]]}), 
              llPlots=reactive({1:6}))
 })
 
 callModule(module = moduleBugReport, 'bugreport', logfile=reactive({logfilename}))
 callModule(moduleInsertMarkdown, "links_MD",URL_links)
 callModule(moduleInsertMarkdown, "FAQ_MD",URL_FAQ)
 
 
 
 #Set up writing file for log
 logfilename <- tempfile(fileext=".log")
 print(logfilename)
 con <- file(logfilename,open="wt")
 if(!interactive()){
   sink(con, append=TRUE)
   sink(con, append=TRUE, type="message")
 }
 

 
 RemoveAllPipelineTabs <- function(){
   removeTab(inputId = "navPage", target = "Pipeline peptide")
   removeTab(inputId = "navPage", target = "Pipeline protein")
   removeTab(inputId = "navPage", target = "Pipeline p2p")
 }
 
 ## New value for obj
 observeEvent(req(obj()$initialData),{
    print('EVENT ON : obj()')
    print(paste0("Obj() = ", obj()$initialData))
    print(paste0("pipeline = ", obj()$pipeline))
    
    rv$current.pipeline.indice <- 1
    
    ## which processes will be part of the pipeline
    rv$ll.process <- c('original','processA','processB','processC')
    rv$init.obj <- NULL
    rv$init.obj[rv$ll.process] <- list(NULL)
    rv$init.obj[['original']] <- obj()$initialData
    
    
    rv$current.pipeline <- rv$pipeline.gen()
    RemoveAllPipelineTabs()
    insertTab(inputId = "navPage",moduleGenericPipelineUI('pepe'), target="Data manager", position="after")
    

  })
  
 
  
  ## manual change of current dataset
 observeEvent(input$currentDataset,{
    n <- which(names(rv$current.pipeline.data)==input$currentDataset)
    if (length(n)==0){
      rv$current.pipeline.indice <- 1
    } else {
      rv$current.pipeline.indice <- n
    }
  })


 
 observeEvent(rv$pipeline.gen(),{
   req(rv$current.pipeline)
   if (rv$current.pipeline$name != 'peptide'){return(NULL)}
   print('### EVENT ON : rv$pipeline.gen')
   rv$current.pipeline <- rv$pipeline.gen()
   rv$current.pipeline.data <- rv$pipeline.gen()$dataset
   rv$current.pipeline.indice <- rv$pipeline.gen()$indice
   print("new value for rv$current.pipeline")
   print(rv$current.pipeline)
   
 })
 
  
  
  output$chooseDataset <- renderUI({

    req(rv$current.pipeline.data)
    req(rv$current.pipeline.indice)
    absolutePanel(
      id  = "#AbsolutePanel",
      top = -10, right = 50, width = "500px",height = "50px",
      draggable = FALSE,fixed = TRUE,
      cursor = "default",
    tagList(
      div(
        div(
          style="display:inline-block; vertical-align: center; margin:0px",
          p('Current dataset')
        ),
        div(
        style="display:inline-block; vertical-align: center; margin:0px",
        selectInput('currentDataset', '',
                    choices = names(rv$current.pipeline.data[!sapply(rv$current.pipeline.data,is.null)]),
                    selected = names(rv$current.pipeline.data)[rv$current.pipeline.indice],
                    width='150px')
        )
      )
    )
    )
  })


   }
