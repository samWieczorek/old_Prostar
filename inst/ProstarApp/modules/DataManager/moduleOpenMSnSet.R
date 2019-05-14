source(file.path(".", "modules/DataManager/funcs.R"),  local = TRUE)$value



moduleOpenMSnSetUI  <- function(id){
  ns <- NS(id)
  tabPanel("Open MSnset file",
           value = "openMSnsetTab",
           tagList(
             fileInput(ns("file"), "Open a MSnset file", multiple = FALSE),
             moduleInfoDatasetUI(ns("infoAboutMSnset")),
             div( style="display:inline-block; vertical-align: top;",
                  moduleStaticDataTableUI(ns("overview_openMSnset"))
             )
             
           )
  )
}




moduleOpenMSnSet  <- function(input, output, session){
  ns <- session$ns
  
  
  callModule(moduleStaticDataTable,"overview_openMSnset", 
             table2show=reactive({
               req(rv.openMSnSet$dataOut)
               GetDatasetOverview2(rv.openMSnSet$dataOut@datasets[[1]])
               }))
 
  callModule(moduleInfoDataset, "infoAboutMSnset",
             obj = reactive({
               req(rv.openMSnSet$dataOut)
               print("test")
               print(rv.openMSnSet$dataOut)
               GetDatasetOverview2(rv.openMSnSet$dataOut@datasets[[1]])
             }))
  
  def.progress.openMSnset <- c('Step 1', 'Step 2', 'Step 3', 'Step 4')
  
  rv.openMSnSet <- reactiveValues(
    current.obj = NULL,
    dataOut = NULL
  )
  
  
  
  
  ##-- Open a MSnset File --------------------------------------------
  observeEvent(input$file,ignoreInit =TRUE,{ 
    nSteps <- length(def.progress.openMSnset)
    
    withProgress(message = '',detail = '', value = 0, {
      
      incProgress(1/nSteps, detail = def.progress.openMSnset[1])
      #ClearMemory()
      #ClearUI()
      data <- readRDS(input$file$datapath)
      
      
      if(!(class(data)[1] %in% c("pepPipeline", "protPipeline", "p2pPipeline"))) {
        
        if (class(data)[1]=="MSnSet") {
          typeOfData <- data@experimentData@other$typeOfData
          ll.process <- type <- NULL
          switch(typeOfData,
                 peptide = {
                   rv.openMSnSet$current.obj <- pepPipeline()
                   ll.process <- peptide.def
                   type <- 'peptide'
                 },
                 protein = {
                   rv.openMSnSet$current.obj <- protPipeline()
                   ll.process <- protein.def
                   type <- 'protein'
                   
                 }, 
                 p2p = {
                   rv.openMSnSet$current.obj <- p2pPipeline()
                   ll.process <- p2p.def
                   type <- 'p2p'
                   
                 }
          )
          print(paste0("class of data : ", class(data)))
          rv.openMSnSet$current.obj <- initialize(rv.openMSnSet$current.obj, 
                                                  c('original',ll.process), 
                                                  data,
                                                  input$file$name, 
                                                  type )
          
          
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
      
      #rv.openMSnSet$current.obj <- ConfigureData(rv.openMSnSet$current.obj)
      
      incProgress(1/nSteps, detail = def.progress.openMSnset[4])
    })
    
    rv.openMSnSet$dataOut <- rv.openMSnSet$current.obj
    
  })
  
  
  return(reactive({rv.openMSnSet$dataOut }))
  
}
