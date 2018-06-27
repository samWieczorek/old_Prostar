callModule(moduleDatasetOverview,"overview_openMSnset")

output$openMSnsetScreen <- renderUI({
  
  tagList(
    fileInput("file", "Open a MSnset file", multiple = FALSE),
    moduleDatasetOverviewUI("overview_openMSnset"),
    uiOutput("infoAboutAggregationTool")
  )
})






output$infoAboutAggregationTool <- renderUI({
  rv$typeOfDataset
  req(rv$current.obj)
  retroCompatibility()
  if (NeedsUpdate())
  {    
    tags$div(
      tags$div(style="display:inline-block; vertical-align: top;",
               tags$img(src = "images/Problem.png", height=25)),
      tags$div(style="display:inline-block; vertical-align: top;",
               tags$p("The dataset was created with a former version of ProStaR, which experimental design is not compliant with the current
                      software functionalities. Please go to \"Update design\" in the \"Dataset manager\" menu tu update it."))
    )
  } else{
    
    
    NA.count <- length(which(is.na(Biobase::exprs(rv$current.obj))))
    
    nb.empty.lines <- sum(apply(is.na(as.matrix(exprs(rv$current.obj))), 1, all))
    
    tagList(
      tags$h3("Info"),
      if (rv$typeOfDataset == "protein"){
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
    
    exts <- c("MSnset","MSnSet")
    if( is.na(match(GetExtension(input$file$name), exts))) {
        shinyjs::info("Warning : this file is not a MSnset file ! 
                      Please choose another one.")
    }
    else {
        ClearMemory()
        ClearUI()
        rv$current.obj <- readRDS(input$file$datapath)
        rv$current.obj.name <- DeleteFileExtension(input$file$name)
        rv$typeOfDataset <- rv$current.obj@experimentData@other$typeOfData
        rv$indexNA <- which(is.na(exprs(rv$current.obj)))
        rv$updateDesign_designChecked <- check.design(Biobase::pData(rv$current.obj))
        colnames(fData(rv$current.obj)) <- gsub(".", "_", colnames(fData(rv$current.obj)), fixed=TRUE)
        names(rv$current.obj@experimentData@other) <- gsub(".", "_", names(rv$current.obj@experimentData@other), fixed=TRUE)
        
        if (is.null(rv$current.obj@experimentData@other$RawPValues ))
            rv$current.obj@experimentData@other$RawPValues <- FALSE
        
        rv$current.obj <- addOriginOfValue(rv$current.obj)
        l.params <- list(filename = rv$current.obj.name)
        UpdateLog("Original",l.params)
        
        
        
        retroCompatibility()
        
        
        #loadObjectInMemoryFromConverter()
        loadObjectInMemoryFromConverter_2(rv$current.obj)
        
    }
    
    })

