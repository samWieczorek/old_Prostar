

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
    
    uiOutput(ns("infoAboutMSnset")),
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
             table2show=reactive({GetDatasetOverview2(GetMSnSet.Reactive())}))
  
  
  
  GetMSnSet.Reactive <- reactive({
    req(rv.openDemo$current.obj)
    
    tmp <- NULL
    if( class(rv.openDemo$current.obj)[1]=='MSnSet') {
     tmp <- rv.openDemo$current.obj 
    } else if ( class(rv.openDemo$current.obj)[1]=='genericPipeline') {
    tmp <- getMSnSet(rv.openDemo$current.obj, 1)
    }
    tmp
  })


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
      if(!(class(data)[1] %in% c("pepPipeline", "protPipeline", "p2pPipeline"))) {
        
        if (class(data)[1]=="MSnSet") {
          typeOfData <- data@experimentData@other$typeOfData
          switch(typeOfData,
                 peptide = rv.openDemo$current.obj <- pepPipeline(),
                 protein = rv.openDemo$current.obj <- protPipeline(), 
                 p2p = rv.openDemo$current.obj <- p2pPipeline()
          )
          rv.openDemo$current.obj <- initialize(rv.openDemo$current.obj, ll.process, data,input$file$name, type )
          
          
        } else {
          shinyjs::info("Warning : this file is not a MSnset file ! 
                      Please choose another one.")
          return(NULL)
        }
      } else {
        
        ## The dataset is already in the new format 
        rv.openDemo$current.obj <- data
      }
      
      
      incProgress(1/nSteps, detail = def.progress.loadDataset[2])
      l.params <- list(filename = input$demoDataset)
      incProgress(1/nSteps, detail = def.progress.loadDataset[3])
      
      #loadObjectInMemory()
      incProgress(1/nSteps, detail = def.progress.loadDataset[4])
      ConfigureDataset()
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
  
  
  
  
  
  output$infoAboutMSnset <- renderUI({
    req(GetMSnSet.Reactive())
    
    #print(str(rv$current.obj))
    data <- GetMSnSet.Reactive()
    #print(str(data))
    typeOfDataset <- data@experimentData@other$typeOfData
    
    if (NeedsUpdate())
    {    
      tags$div(
        tags$div(style="display:inline-block; vertical-align: top;",
                 tags$img(src = "images/Problem.png", height=25)),
        tags$div(style="display:inline-block; vertical-align: top;",
                 HTML("The dataset was created with a former version of ProStaR, which experimental design is not compliant with the current
                      software functionalities. Please update the design below"))
                 )
    } else{
      
      NA.count <- length(which(is.na(Biobase::exprs(data))))
      nb.empty.lines <- sum(apply(is.na(as.matrix(exprs(data))), 1, all))
      tagList(
        tags$h3("Info"),
        if (typeOfDataset == "protein"){
          tags$p("Note: the aggregation tool
                 has been disabled because the dataset contains 
                 protein quantitative data.")
        },
        
        if (NA.count > 0){
          tags$p("As your dataset contains missing values, you should 
                 impute them prior to proceed",br()," 
                 to the differential analysis.")
        },
        if (nb.empty.lines > 0){
          tags$p("As your dataset contains lines with no values, you 
                 should remove them with the filter",br()," tool
                 prior to proceed to the analysis of the data.")
        }
        
          )
      
        }
        })
  
  
  
  
}