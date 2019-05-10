source(file.path(".", "modules/moduleStaticDataTable.R"),  local = TRUE)$value



moduleOpenMSnSetUI  <- function(id){
  ns <- NS(id)
  tabPanel("Open MSnset file",
           value = "openMSnsetTab",
           tagList(
             fileInput(ns("file"), "Open a MSnset file", multiple = FALSE),
             uiOutput(ns("infoAboutMSnsetOpen")),
             div( style="display:inline-block; vertical-align: top;",
                  moduleStaticDataTableUI(ns("overview_openMSnset"))
             )
             
           )
  )
}




moduleOpenMSnSet  <- function(input, output, session){
  ns <- session$ns
  
  
  callModule(moduleStaticDataTable,"overview_openMSnset", 
             table2show=reactive({GetDatasetOverview2(GetMSnSet.Reactive())}))
 
  def.progress.openMSnset <- c('Step 1', 'Step 2', 'Step 3', 'Step 4')
  
  rv.openMSnSet <- reactiveValues(
    current.obj = NULL,
    dataOut = NULL
  )
  
  
  GetMSnSet.Reactive <- reactive({
    req(rv.openMSnSet$current.obj)
    
    tmp <- NULL
    if( class(rv.openMSnSet$current.obj)[1]=='MSnSet') {
      tmp <- rv.openMSnSet$current.obj 
    } else if ( class(rv.openMSnSet$current.obj)[1]=='genericPipeline') {
      tmp <- getMSnSet(rv.openMSnSet$current.obj, 1)
    }
    tmp
  })
  
  
  
  
  output$infoAboutMSnsetOpen <- renderUI({
    req(GetMSnSet.Reactive())
    
    data <- GetMSnSet.Reactive()
    #print(str(data))
    typeOfDataset <- data@experimentData@other$typeOfData
    
    retroCompatibility()
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
  
  
  
  ##-- Open a MSnset File --------------------------------------------
  observeEvent(input$file,ignoreInit =TRUE,{ 
    nSteps <- length(def.progress.openMSnset)
    
    withProgress(message = '',detail = '', value = 0, {
      
      incProgress(1/nSteps, detail = def.progress.openMSnset[1])
      #ClearMemory()
      #ClearUI()
      data <- readRDS(input$file$datapath)
      
      
      if(!(class(data)[1] %IN% c("pepPipeline", "protPipeline", "p2pPipeline"))) {
        
        if (class(data)[1]=="MSnSet") {
          typeOfData <- data@experimentData@other$typeOfData
          switch(typeOfData,
                  peptide = rv.openMSnSet$current.obj <- pepPipeline(),
                  protein = rv.openMSnSet$current.obj <- protPipeline(), 
                  p2p = rv.openMSnSet$current.obj <- p2pPipeline()
                 )
          rv.openMSnSet$current.obj <- initialize(rv.openMSnSet$current.obj, ll.process, data,input$file$name, type )
          
          
        } else {
              shinyjs::info("Warning : this file is not a MSnset file ! 
                      Please choose another one.")
        return(NULL)
        }
      } else {
          
          ## The dataset is already in the new format 
          rv.openMSnSet$current.obj <- data
          }
        
      incProgress(1/nSteps, detail = def.progress.openMSnset[2])
      l.params <- list(filename = input$file$name)
      
      incProgress(1/nSteps, detail = def.progress.openMSnset[3])
      
      loadObjectInMemory()
      incProgress(1/nSteps, detail = def.progress.openMSnset[4])
    })
    
    rv.openMSnSet$dataOut <- rv.openMSnSet$current.obj
    
  })
  
  
}
