source(file.path(".", "modules/DataManager/funcs.R"),  local = TRUE)$value


moduleOpenDemoDatasetUI  <- function(id){
  ns <- NS(id)
  
  
  tagList(
    tags$div(
      tags$div( style="display:inline-block; vertical-align: middle; padding-right: 20px;",
                uiOutput(ns("chooseDemoDataset"))
      ),
      
      tags$div( style="display:inline-block; vertical-align: middle; padding-right: 20px;",
                p(""),
                actionButton(ns("loadDemoDataset"), "Load demo dataset",class = actionBtnClass)
      ),
      tags$div( style="display:inline-block; vertical-align: middle;",
                p(""),
                uiOutput(ns("linktoDemoPdf"))
      )
    ),
    
    moduleInfoDatasetUI(ns("infoAboutMSnset")),
    div( style="display:inline-block; vertical-align: top;",
         moduleStaticDataTableUI(ns("overview_DemoMode"))
    )
  )
}



moduleOpenDemoDataset  <- function(input, output, session, selectedPanel){
  ns <- session$ns
  
  
  rv.openDemo <- reactiveValues(
    current.obj = NULL,
    dataOut = NULL
  )
  
  
  callModule(moduleStaticDataTable,"overview_DemoMode", 
             table2show=reactive({req(rv.openDemo$current.obj)
                                GetDatasetOverview2(rv.openDemo$current.obj@datasets[[1]])}))
  callModule(moduleInfoDataset, "infoAboutMSnset",
              obj = reactive({req(rv.openDemo$current.obj)
                rv.openDemo$current.obj@datasets[[1]]}))
    


  ### function for demo mode
  output$chooseDemoDataset <- renderUI({
    
    if(require("DAPARdata", lib.loc=DAPARdata.loc)){
      print("DAPARdata is loaded correctly")
      selectInput(ns("demoDataset"),
                  "Demo dataset",
                  choices = utils::data(package="DAPARdata")$results[,"Item"],
                  width='200px')
    } else {
      print("Trying to install DAPARdata")
      BiocManager::install("DAPARdata")
      if(require(DAPARdata)){
        print("DAPARdata installed and loaded")
        selectInput(ns("demoDataset"),
                    "Demo dataset",
                    choices = utils::data(package='DAPARdata')$results[,"Item"],
                    width='200px'   )
      } else {
        stop("Could not install the package DAPARdata")
      }
    }
    
    
  })
  
  
  
  
  
  
  observeEvent(input$loadDemoDataset, {
    
    nSteps <- length(def.progress.loadDataset)
    print(paste0("nSteps = ", nSteps))
    withProgress(message = '',detail = '', value = 0, {
      #ClearMemory()
      #ClearUI()
      incProgress(1/nSteps, detail = def.progress.loadDataset[1])
      utils::data(list = input$demoDataset)
      print(input$demoDataset)
      data <- get(input$demoDataset)
      print("#################################")
      print(paste0("class(data) = ", class(data)[1]))
      if(!(class(data)[1] %in% c("pepPipeline", "protPipeline", "p2pPipeline"))) {
        
        if (class(data)[1]=="MSnSet") {
          typeOfData <- data@experimentData@other$typeOfData
          ll.process <- type <- NULL
          switch(typeOfData,
                 peptide = {
                   rv.openDemo$current.obj <- pepPipeline()
                   ll.process <- peptide.def
                   type <- 'peptide'
                   },
                 protein = {
                   rv.openDemo$current.obj <- protPipeline()
                   ll.process <- protein.def
                   type <- 'protein'
                   
                 }, 
                 p2p = {
                   rv.openDemo$current.obj <- p2pPipeline()
                   ll.process <- p2p.def
                   type <- 'p2p'
                   
                 }
          )
          
          rv.openDemo$current.obj <- initialize(rv.openDemo$current.obj, ll.process, data,input$demoDataset, type )
          
          
        } else {
          shinyjs::info("Warning : this file is not a MSnset file ! 
                      Please choose another one.")
          return(NULL)
        }
      } else {
        
        ## The dataset is already in the new format 
        rv.openDemo$current.obj <- data
      }
      
      print(class( rv.openDemo$current.obj))
      incProgress(1/nSteps, detail = def.progress.loadDataset[2])
      l.params <- list(filename = input$demoDataset)
      incProgress(1/nSteps, detail = def.progress.loadDataset[3])
      
      #loadObjectInMemory()
      incProgress(1/nSteps, detail = def.progress.loadDataset[4])
      #tmp <- getMSnSet(rv.openDemo$current.obj, 1)
      #print(paste0("reutrn of  getMSnSet : ", class(tmp)))
      rv.openDemo$current.obj <- ConfigureData(rv.openDemo$current.obj)
      print("new demo dataset prepared")
      #rv.openDemo$dataOut <- rv.openDemo$current.obj
      
    })
    rv.openDemo$dataOut <- rv.openDemo$current.obj
    
  })
  
  
  
  
  
  output$linktoDemoPdf <- renderUI({
    req(input$demoDataset)
    
    file<- paste(system.file(package = "DAPARdata"),"/doc/",
                 input$demoDataset,".pdf", sep="")
    cmd <- paste("cp ",file," www/.", sep="")
    system(cmd)
    filename <-paste0(input$demoDataset,".pdf", sep="")
    p("Dataset documentation ",a(href=filename, target='_blank', "(pdf)"))
    
  })
  
  
  
  
  return(reactive({rv.openDemo$dataOut}))
  
}