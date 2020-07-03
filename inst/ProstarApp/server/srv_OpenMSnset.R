callModule(moduleStaticDataTable,"overview_openMSnset", table2show=reactive({GetDatasetOverview()}),
           filename='openMSnset_View')







output$updateDesign <- renderUI({
  rv$current.obj
  
  if(!NeedsUpdate()){return(NULL)}
  #source(file.path("server", "srv_UpdateDesign.R"),  local = TRUE)$value
  tagList(
    fluidRow(
      column(width=6,tags$b("1 - Fill the \"Condition\" column to identify the conditions to compare.")),
      column(width=6,uiOutput("updateDesign_UI_checkConditions")  )
    ),
    fluidRow(
      column(width=6,uiOutput("updateDesign_UI_hierarchicalExp")),
      column(width=6,uiOutput("updateDesign_checkDesign") )
    ),
    uiOutput("updateDesign_SaveDesign"),
    uiOutput("designUpdated"),
    
    hr(),
    tags$div(
      
      tags$div(style="display:inline-block; vertical-align: top;",
               uiOutput("viewNewDesign",width="100%")
      ),
      tags$div(style="display:inline-block; vertical-align: top;",
               shinyjs::hidden(
                 div(id = "updateDesign_exLevels",uiOutput("updateDesign_designExamples")))
      )
      
    )
    
  )
})




output$openMSnsetScreen <- renderUI({
  
  tagList(
    fileInput("file", "Open a MSnset file", multiple = FALSE),
    moduleStaticDataTableUI("overview_openMSnset"),
    uiOutput("infoAboutAggregationTool")
  )
})







# observeEvent(input$LinkToupdateDesignTab, {
#   updateTabsetPanel(session, 'navPage', "updateDesignTab")
# })


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
               HTML("The dataset was created with a former version of ProStaR, which experimental design is not compliant with the current
                      software functionalities. Please update the design below"))
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
observeEvent(input$loadMSnset,ignoreInit =TRUE,{ 
  input$file
  ClearMemory()
  ClearUI()
  
  authorizedExtension <- "msnset"
  
  tryCatch({
    
    if (length(grep(GetExtension(input$file$datapath),authorizedExtension,ignore.case=TRUE) )==0)
    {
      # shinyjs::info("Warning : this file is not a MSnset file ! Please choose another one.")
      warning("Warning : this file is not a MSnset file ! Please choose another one.")
    }
    rv$current.obj <- readRDS(input$file$datapath)
    
    if( class(rv$current.obj)[1] != "MSnSet") {
      # shinyjs::info("Warning : this file is not a MSnset file !  Please choose another one.")
      warning("Warning : this file is not a MSnset file ! Please choose another one.")
    }
    
    
    
    
    rv$current.obj.name <- DeleteFileExtension(input$file$name)
    rv$typeOfDataset <- rv$current.obj@experimentData@other$typeOfData
    rv$indexNA <- which(is.na(exprs(rv$current.obj)))
    rv$updateDesign_designChecked <- check.design(Biobase::pData(rv$current.obj))
    colnames(fData(rv$current.obj)) <- gsub(".", "_", colnames(fData(rv$current.obj)), fixed=TRUE)
    names(rv$current.obj@experimentData@other) <- gsub(".", "_", names(rv$current.obj@experimentData@other), fixed=TRUE)
    
    rv$widgets$aggregation$proteinId <- rv$current.obj@experimentData@other$proteinId
    rv$proteinId <- rv$current.obj@experimentData@other$proteinId
    if (is.null(rv$current.obj@experimentData@other$RawPValues ))
    {
      rv$current.obj@experimentData@other$RawPValues <- FALSE
    } else if(isTRUE(rv$current.obj@experimentData@other$RawPValues )){
      
      #   nn <- names(rv$current.obj@experimentData@other$Params)
      #    ind <-  grep("HypothesisTest",nn)
      # names.logFC <- rv$current.obj@experimentData@other$Params[[nn[ind]]][['HypothesisTest']]$AllPairwiseCompNames$logFC
      # names.P_Value <- rv$current.obj@experimentData@other$Params[[nn[ind]]][['HypothesisTest']]$AllPairwiseCompNames$P_Value
      # 
      # .logFC <- as.data.frame(Biobase::fData(rv$current.obj)[,names.logFC])
      # .P_Value <- as.data.frame(Biobase::fData(rv$current.obj)[,names.P_Value])
      # names(.logFC) <- names.logFC
      # names(.P_Value) <- names.P_Value
      # 
      
      #rv$widgets$hypothesisTest$th_logFC <- rv$current.obj@experimentData@other$Params[[nn[ind]]][['HypothesisTest']]$th_logFC
      rv$method <- rv$current.obj@experimentData@other$Params[["HypothesisTest"]]$method
    }
    nn <- names(rv$current.obj@experimentData@other$Params)
    ind <-  grep("HypothesisTest",nn)
    #params.tmp <- rv$current.obj@experimentData@other$Params[["HypothesisTest"]]
    if (length(ind)>0){
      # rv$res_AllPairwiseComparisons <- list(logFC = setNames(data.frame(Biobase::fData(rv$current.obj)[,params.tmp$AllPairwiseCompNames$logFC]),
      #                                                      params.tmp$AllPairwiseCompNames$logFC),
      #                                     P_Value = setNames(data.frame(Biobase::fData(rv$current.obj)[,params.tmp$AllPairwiseCompNames$P_Value]),
      #                                                        params.tmp$AllPairwiseCompNames$P_Value
      #                                     ))
      rv$res_AllPairwiseComparisons <- Get_AllComparisons(rv$current.obj)
      rv$listNomsComparaison <- colnames(rv$res_AllPairwiseComparisons$logFC)
    }
    
    
    rv$current.obj <- addOriginOfValue(rv$current.obj)
    
    
    l.params <- list(filename = rv$current.obj.name)
    retroCompatibility()
    loadObjectInMemoryFromConverter()
  }
  , warning = function(w) {
    shinyjs::info( conditionMessage(w))
    return(NULL)
  }, error = function(e) {
    shinyjs::info(conditionMessage(e))
    return(NULL)
  }, finally = {
    #cleanup-code 
  })
  
})
