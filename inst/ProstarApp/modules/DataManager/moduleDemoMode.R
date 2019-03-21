source(file.path(".", "modules/moduleStaticDataTable.R"),  local = TRUE)$value



moduleDemoModeUI  <- function(id){
  ns <- NS(id)
 
  tabPanel("Demo data",
           value = "demoTab",
           #tagList(
           # selectInput(ns("selectPipeline"), "Select pipeline",
           #             choices=c("None"="","Peptide"="Peptide", "Protein"="Protein", "P2p" = "P2p"),
           #             selected=character(0)),
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
           
           hr(),
           uiOutput(ns("infoAboutDemoDataset")),
           div( style="display:inline-block; vertical-align: top;",
                moduleStaticDataTableUI(ns("overview_DemoMode"))
           )
           #)
 )


}




moduleDemoMode  <- function(input, output, session){
  ns <- session$ns
  
 
  callModule(moduleStaticDataTable,"overview_DemoMode", 
             table2show=reactive({GetDatasetOverview2(rv$current.obj$datasets[[1]])}))
  
  
  rv <- reactiveValues(
    current.obj = NULL,
    res = NULL,
    name ="demomode",
    current.obj.name =NULL,
    indexNA = NULL
  )
  
  
  
  output$chooseDemoDataset <- renderUI({
    
    if(require("DAPARdata", lib.loc=DAPARdata.loc)){
      print("DAPARdata is loaded correctly")
      selectInput(ns("demoDataset"),
                  "Demo dataset",
                  choices = utils::data(package="DAPARdata")$results[,"Item"],
                  width='200px'    )
    } else {
      print("Trying to install DAPARdata")
      BiocManager::install("DAPARdata")
      if(require(DAPARdata)){
        print("DAPARdata installed and loaded")
        selectInput("demoDataset",
                    "Demo dataset",
                    choices = utils::data(package='DAPARdata')$results[,"Item"],
                    width='200px'   )
      } else {
        stop("Could not install the package DAPARdata")
      }
    }
    
    
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
  
  
  output$infoAboutDemoDataset <- renderUI({
    req(rv$current.obj$datasets[[1]])
    
    data <- rv$current.obj$datasets[[1]]
    print(data)
    
    
    isolate({ NA.count <- length(which(is.na(Biobase::exprs(data))))
    
    nb.empty.lines <- sum(apply(is.na(as.matrix(exprs(data))), 1, all))
    
    tagList(
      h3("Info"),
      # if (rv$typeOfDataset == "protein"){
      #   p("Note: the aggregation tool
      #          has been disabled because the dataset contains 
      #          protein quantitative data.")
      # },
      
      if (NA.count > 0){
        p("As your dataset contains missing values, you should 
               impute them prior to proceed to the differential analysis.")
      },
      if (nb.empty.lines > 0){
        p("As your dataset contains lines with no values, you 
               should remove them with the filter tool
               prior to proceed to the analysis of the data.")
      }
      
        

        )
      })
    
    
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
      print(paste0("class(data)[1] : ", class(data)[1]))
      if((class(data)[1] == "MSnSet") && !is.list(data)){
        
        
        rv$current.obj <- list(datasets = list(original=data),
                               name.dataset = input$demoDataset, 
                               pipeline = "Peptide")
      
        print("cas 1")
      } else {
        rv$current.obj <- data
        print("cas 2")
      }
      
      incProgress(1/nSteps, detail = def.progress.loadDataset[2])
      l.params <- list(filename = input$demoDataset)
      incProgress(1/nSteps, detail = def.progress.loadDataset[3])
        
      loadObjectInMemory()
      incProgress(1/nSteps, detail = def.progress.loadDataset[4])
      })
      
   
    rv$res <- rv$current.obj
    
  })
  
  
  return(reactive({rv$res}))
}



loadObjectInMemory <- function(obj){
  
  # print(rv$current.obj)
  # req(rv$current.obj)
  # #rv$typeOfDataset <- rv$current.obj@experimentData@other$typeOfData
  # rv$proteinId <- rv$current.obj@experimentData@other$proteinId
  # if (is.null(rv$typeOfDataset)) {rv$typeOfDataset <- ""}
  # rv$current.obj$indexNA <- which(is.na(rv$current.obj$MSnset))
  # rv$current.obj$MSnset <- DAPAR::addOriginOfValue(rv$current.obj$MSnset)
  # 
  # colnames(fData(rv$current.obj$MSnset)) <- gsub(".", "_", colnames(fData(rv$current.obj$MSnset)), fixed=TRUE)
  # names(rv$current.obj$MSnset@experimentData@other) 
  #             <- gsub(".", "_", names(rv$current.obj$MSnset@experimentData@other), fixed=TRUE)
  # 
  
  #If there are already pVal values, then do no compute them 
  # if (G_logFC_Column %in% names(Biobase::fData(rv$current.obj) )){
  #   rv$resAnaDiff <- list(logFC = Biobase::fData(rv$current.obj)$logFC,
  #                         P_Value = Biobase::fData(rv$current.obj)$P_Value)
  #   rv$widgets$hypothesisTest$th_logFC <- rv$current.obj@experimentData@other$threshold_logFC
  # }
  
  # if (is.null(rv$current.obj$MSnset@experimentData@other$RawPValues ))
  #   rv$current.obj$MSnset@experimentData@other$RawPValues <- FALSE
  # 
  # rv$PlotParams$paletteConditions <- GetExamplePalette()
  # 
  # if (rv$typeOfDataset == "peptide"  && !is.null(rv$proteinId)){ ComputeAdjacencyMatrices()}
  # 
  # rv$res.pca <- wrapper.pca(rv$current.obj, rv$PCA_varScale, ncp=Compute_PCA_nbDimensions())
  # 
  # name <- paste0("Original", ".", rv$typeOfDataset)
  # 
  # if (is.null(rv$current.obj@experimentData@other$Params))
  #   rv$current.obj <- saveParameters(rv$current.obj, name,"-")
  # else {
  #   names(rv$current.obj@experimentData@other$Params) <- paste0('prev.',names(rv$current.obj@experimentData@other$Params))
  # }
  
  #rv$dataset[[name]] <- rv$current.obj
  #updateSelectInput(session, "datasets", 
                    #label = paste("Dataset versions of", rv$current.obj.name, sep=" "),
  #                  choices = names(rv$dataset),
  #                 selected = name)
  #ClearNavbarPage()
  #BuildNavbarPage()
  
  #return(obj)
}
