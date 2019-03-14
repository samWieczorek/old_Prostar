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
  
  source(file.path(".", "modules/pipelines/modulePipelinePep.R"),  local = TRUE)$value
  source(file.path(".", "modules/pipelines/modulePipelineProt.R"),  local = TRUE)$value
  source(file.path(".", "modules/pipelines/modulePipelineP2p.R"),  local = TRUE)$value
  
  
  loadLibraries()
  
  
  rv <- reactiveValues(
    
    # name of the current dataset in the widget chooseDataset
   
    # current working data from current pipeline
    current.pipeline.data = NULL,
    current.pipeline.indice = NULL,
    current.pipeline = NULL,
    
    #current indice (rank of current dataset in pipeline)
    indice = 1,
    init.obj = NULL,
    
    #model for the structure of dataset for peptide pipeline
    
    pipeline.pep = callModule(module = modulePipelinePep, 'test', 
                              initData = reactive({rv$init.obj}), 
                              navPage =reactive({input$navPage}),
                              indice = reactive({rv$current.pipeline.indice}))
    # pipeline.prot = callModule(module = modulePipelineProt, 'testprot', 
    #                           initData = reactive({rv$init.obj}), 
    #                           navPage =reactive({input$navPage}),
    #                           indice = reactive({rv$indice}))
    
  )
    
 #####
 ## Launch modules
  obj <- callModule(module = moduleDataManager, 'datamanager')
  
 callModule(module = modulePlots, 'showPlots', 
              dataIn=reactive({rv$current.pipeline.data[[rv$current.pipeline.indice]]}), 
              llPlots=reactive({1:6}))
 
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
 

 observeEvent(req(obj()$initialData),{
    print('EVENT ON : obj()')
    print(paste0("Obj() = ", obj()$initialData))
    print(paste0("pipeline = ", obj()$pipeline))
    
    switch(obj()$pipeline,
           Peptide= {
             rv$current.pipeline.indice <- 1
             rv$init.obj <- list(original=obj()$initialData,
                                 A_processed = NULL,
                                 B_processed = NULL,
                                 C_processed = NULL
                                )
              rv$current.pipeline <- rv$pipeline.pep()
             print(rv$current.pipeline)
             insertTab(inputId = "navPage",modulePipelinePepUI('test'), target="Data manager", position="after")
           },
           Protein = {
             rv$current.pipeline.indice <- 1
             rv$init.obj <- list(original=obj()$initialData,
                                 D_processed = NULL,
                                 E_processed = NULL,
                                 F_processed = NULL,
                                 G_processed = NULL
                                  )
             insertTab(inputId = "navPage",modulePipelineProtUI('testprot'), target="Data manager", position="after")
           },
           P2p = {
             rv$current.pipeline.indice <- 1
             insertTab(inputId = "navPage",modulePipelineP2pUI('testp2p'), target="Data manager", position="after")
           }
    )

  })
  
 
  
  observeEvent(input$currentDataset,{
    print("observeEvent(input$currentDataset,")

    print(str(rv$current.pipeline.data))
    rv$current.pipeline.indice <- which(names(rv$current.pipeline.data)==input$currentDataset)
    if (length(rv$current.pipeline.indice)==0) rv$current.pipeline.indice <- 1
    print(paste0("New value of indice : ", rv$current.pipeline.indice))
  })


  
 
 observe({
   rv$current.pipeline.indice
   print(paste0("Watch rv$current.pipeline.indice = ", rv$current.pipeline.indice))
 })
 
 
  observeEvent(rv$pipeline.pep(),{
    
    print('### EVENT ON : rv$pipeline.pep')
    rv$current.pipeline <- rv$pipeline.pep()
    rv$current.pipeline.data <- rv$pipeline.pep()$dataset
    rv$current.pipeline.indice <- rv$pipeline.pep()$indice
  })
  
  
    # rv$current.pipeline.indice <- rv$current.pipeline()$indice
    # print(paste0("current.pipeline.indice :",rv$current.pipeline.indice))
    # print(rv$current.pipeline.data)
    # updateSelectInput(session, "currentDataset", 
    #                   choices = names(rv$current.pipeline.data[!sapply(rv$current.pipeline.data,is.null)]),
    #                   selected = names(rv$current.pipeline.data)[rv$current.pipeline.indice])
    # 
    # #GetIndex()
    # })
  
#  
# GetNonNullNames <- reactive({
#   rv$current.pipeline.data
#   n <-names(rv$current.pipeline.data[!sapply(rv$current.pipeline.data,is.null)])
#   n
# })
#   
  output$chooseDataset <- renderUI({

    rv$current.pipeline.data
    rv$current.pipeline.indice
    tagList(
      div(
        style="display:inline-block; vertical-align: center; margin:0px",
        selectInput('currentDataset', '',
                    choices = names(rv$current.pipeline.data[!sapply(rv$current.pipeline.data,is.null)]),
                    selected = names(rv$current.pipeline.data)[rv$current.pipeline.indice],
                    width='150px')
      )
    )
  })


   }
