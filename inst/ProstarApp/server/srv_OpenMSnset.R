callModule(moduleDatasetOverview,"overview_openMSnset")

output$openMSnsetScreen <- renderUI({
  
  tagList(
    fileInput("file", "Open a MSnset file", multiple = FALSE),
    moduleDatasetOverviewUI("overview_openMSnset"),
    uiOutput("infoAboutAggregationTool")
  )
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
        
        ### check if the design has to be updated
        # if (NeedsUpdate()) {
        #     hideTab(inputId ="navPage", target = "FilterDataTab")
        #     hideTab(inputId ="navPage", target = "Normalization")
        #     hideTab(inputId ="navPage", target = "AggregationTab")
        #     hideTab(inputId ="navPage", target = "imputationTabs")
        #     hideTab(inputId ="navPage", target = "diffAnalysisTab")
        #     hideTab(inputId ="navPage", target = "GOAnalysisTab")
        #     hideTab(inputId ="navPage", target = "convertTab")
        #     hideTab(inputId ="navPage", target = "demoTab")
        #     hideTab(inputId ="navPage", target = "SessionLogsTab")
        #     
        #     showTab(inputId ="navPage", target = "updateDesignTab")
        # } else {
        #     hideTab(inputId ="navPage", target = "updateDesignTab")
        #     showTab(inputId ="navPage", target = "FilterDataTab")
        #     showTab(inputId ="navPage", target = "Normalization")
        #     showTab(inputId ="navPage", target = "AggregationTab")
        #     showTab(inputId ="navPage", target = "imputationTabs")
        #     showTab(inputId ="navPage", target = "diffAnalysisTab")
        #     showTab(inputId ="navPage", target = "GOAnalysisTab")
        #     showTab(inputId ="navPage", target = "convertTab")
        #     showTab(inputId ="navPage", target = "demoTab")
        #     showTab(inputId ="navPage", target = "SessionLogsTab")
        # }
        #if (input$showCommandLog){
        writeToCommandLogFile(
            paste("current.obj <- readRDS('",input$file$name,"')", sep="")
        )
        #}
        
        #loadObjectInMemoryFromConverter()
        loadObjectInMemoryFromConverter_2(rv$current.obj)
        
    }
    
    })


