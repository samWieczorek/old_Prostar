source(file.path(".", "modules/moduleStaticDataTable.R"),  local = TRUE)$value
source(file.path(".", "modules/DataManager/moduleConvertData.R"),  local = TRUE)$value



moduleDemoModeUI  <- function(id){
  ns <- NS(id)
 
  # tagList(
  #   uiOutput(ns('openUI'))
  # )
  # 
  
   
  
  
  navbarMenu("Data manager" ,
             tabPanel("Open MSnset",
                      tagList(
                        fileInput(ns("file"), "Open a MSnset file", multiple = FALSE),
                        #uiOutput(ns("updateDesign")),
                        
                        uiOutput(ns("infoAboutMSnsetOpen")),
                        div( style="display:inline-block; vertical-align: top;",
                             moduleStaticDataTableUI(ns("overview_OpenMode"))
                        )
                        
                      )),
             tabPanel("Convert",
                      value = "convertTab",
                      moduleConvertDataUI(ns("moduleProcess_Convert"))
                ),
             tabPanel("Demo data", 
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
                      ))
  )


}




moduleDemoMode  <- function(input, output, session, selectedPanel){
  ns <- session$ns
  
  rv.demomode <- reactiveValues(
    tmp.convert = NULL,
    current.obj = NULL,
    res = NULL,
    name ="demomode",
    current.obj.name =NULL,
    indexNA = NULL
  )
  
  # observeEvent(selectedPanel(),{
  #   
  #   print("Reset of reactive values : rv.demomode")
  #   #rv.demomode$current.obj <- NULL
  # 
  #   
  # })
  # 
  # 
  
  callModule(moduleStaticDataTable,"overview_DemoMode", 
             table2show=reactive({GetDatasetOverview2(rv.demomode$current.obj$datasets[[1]])}))
  callModule(moduleStaticDataTable,"overview_OpenMode", 
             table2show=reactive({GetDatasetOverview2(rv.demomode$current.obj$datasets[[1]])}))
  
 
  
  rv.demomode$tmp.convert <- callModule(module=moduleConvertData, 'moduleProcess_Convert')
  
  
  observe({
    req(rv.demomode$tmp.convert())

    rv.demomode$res <- rv.demomode$tmp.convert()
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
  
  
  
  
  
  
  ###### specific for file open
  
  
  
  output$infoAboutMSnset <- renderUI({
    req(rv.demomode$current.obj$datasets[[1]])
    
    #print(str(rv$current.obj))
    data <- rv.demomode$current.obj$datasets[[1]]
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
  
  
  
  
  output$infoAboutMSnsetOpen <- renderUI({
    req(rv.demomode$current.obj$datasets[[1]])
    
    #print(str(rv$current.obj))
    data <- rv.demomode$current.obj$datasets[[1]]
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
    print(paste0("nSteps = ", nSteps))
    withProgress(message = '',detail = '', value = 0, {
      
      incProgress(1/nSteps, detail = def.progress.openMSnset[1])
      #ClearMemory()
      #ClearUI()
      data <- readRDS(input$file$datapath)
      if(class(data)[1] != "MSnSet") {
        shinyjs::info("Warning : this file is not a MSnset file ! 
                      Please choose another one.")
        return(NULL)
      } else {
        if (!is.list(data)){
        
        
        rv.demomode$current.obj <- list(datasets = list(original=data),
                                        name.dataset = input$file$name, 
                                        pipeline = "Peptide")
        print("cas 1")
        
         } else {
          rv.demomode$current.obj <- data
          print("cas 2")
        }
        
      }
      incProgress(1/nSteps, detail = def.progress.openMSnset[2])
      l.params <- list(filename = input$file$name)
      
      incProgress(1/nSteps, detail = def.progress.openMSnset[3])
      
      #loadObjectInMemoryFromConverter()
      loadObjectInMemory()
      incProgress(1/nSteps, detail = def.progress.openMSnset[4])
    })
    
    rv.demomode$res <- rv.demomode$current.obj
    
  })
  
  
  
  ####### Common functions
 
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
        
        rv.demomode$current.obj <- list(datasets = list(original=data),
                                        name.dataset = input$demoDataset, 
                                        pipeline = data@experimentData@other$typeOfData)
      } else {
        rv.demomode$current.obj <- data
      }
      
      incProgress(1/nSteps, detail = def.progress.loadDataset[2])
      l.params <- list(filename = input$demoDataset)
      incProgress(1/nSteps, detail = def.progress.loadDataset[3])
        
      loadObjectInMemory()
      incProgress(1/nSteps, detail = def.progress.loadDataset[4])
      })
      
   
   rv.demomode$res <- rv.demomode$current.obj
    
  })


loadObjectInMemory <- function(obj){
  
  obj <- GetCurrentMSnSet()
  
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
  # rv.PlotParams$paletteConditions <- GetExamplePalette()
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
  




#   
#   rv$current.obj.name <- DeleteFileExtension(input$file$name)
#   rv$typeOfDataset <- rv$current.obj@experimentData@other$typeOfData
#   rv$indexNA <- which(is.na(exprs(rv$current.obj)))
#   rv$updateDesign_designChecked <- check.design(Biobase::pData(rv$current.obj))
#   colnames(fData(rv$current.obj)) <- gsub(".", "_", colnames(fData(rv$current.obj)), fixed=TRUE)
#   names(rv$current.obj@experimentData@other) <- gsub(".", "_", names(rv$current.obj@experimentData@other), fixed=TRUE)
# 
#   if (is.null(rv$current.obj@experimentData@other$RawPValues ))
#   {
#   rv$current.obj@experimentData@other$RawPValues <- FALSE
#   } else if(rv$current.obj@experimentData@other$RawPValues ){
#   
#     nn <- names(rv$current.obj@experimentData@other$Params)
#     ind <-  grep("HypothesisTest",nn)
#     names.logFC <- rv$current.obj@experimentData@other$Params[[nn[ind]]][['HypothesisTest']]$AllPairwiseCompNames$logFC
#     names.P_Value <- rv$current.obj@experimentData@other$Params[[nn[ind]]][['HypothesisTest']]$AllPairwiseCompNames$P_Value
#   
#     .logFC <- as.data.frame(Biobase::fData(rv$current.obj)[,names.logFC])
#     .P_Value <- as.data.frame(Biobase::fData(rv$current.obj)[,names.P_Value])
#     names(.logFC) <- names.logFC
#     names(.P_Value) <- names.P_Value
#   
#     rv$res_AllPairwiseComparisons <- list(logFC= .logFC,
#                                         P_Value = .P_Value)
#   
#     rv$widgets$hypothesisTest$th_logFC <- rv$current.obj@experimentData@other$Params[[nn[ind]]][['HypothesisTest']]$th_logFC
#     rv$method <- rv$current.obj@experimentData@other$Params[["HypothesisTest"]]$method
#   }
# 
# params.tmp <- rv$current.obj@experimentData@other$Params[["HypothesisTest"]]
# if (!is.null(params.tmp)){
#   rv$res_AllPairwiseComparisons <- list(logFC = setNames(data.frame(Biobase::fData(rv$current.obj)[,params.tmp$AllPairwiseCompNames$logFC]),
#                                                          params.tmp$AllPairwiseCompNames$logFC),
#                                         P_Value = setNames(data.frame(Biobase::fData(rv$current.obj)[,params.tmp$AllPairwiseCompNames$P_Value]),
#                                                            params.tmp$AllPairwiseCompNames$P_Value
#                                         ))
#   
#   rv$listNomsComparaison <- colnames(params.tmp$AllPairwiseCompNames$logFC)
# }
# 
# 
# rv$current.obj <- addOriginOfValue(rv$current.obj)
  
  
  ########################################################### 
  NeedsUpdate <- reactive({
    req(rv.demomode$current.obj$datasets[[1]])
    
    PROSTAR.version <- rv.demomode$current.obj$datasets[[1]]@experimentData@other$Prostar_Version
    
    if (!is.null(PROSTAR.version) && (compareVersion(PROSTAR.version,"1.12.9") != -1)
        && (DAPAR::check.design(Biobase::pData(rv.demomode$current.obj$datasets[[1]]))$valid))
    {return (FALSE)}
    
    else {
      return(TRUE)
    }
  })
  
  
  ######################################################  
  retroCompatibility <- reactive({
    req(rv.demomode$current.obj$datasets[[1]])
    data <- rv.demomode$current.obj$datasets[[1]]
    if ("FC" %in% colnames(Biobase::fData(data))){
      idx <- which(colnames(Biobase::fData(data)) == "FC")
      names(Biobase::fData(data))[idx] <-"logFC"
    }
    
    if ("Experiment" %in% colnames(Biobase::pData(data))){
      idx <- which(colnames(Biobase::pData(data)) == "Experiment")
      names(Biobase::pData(data))[idx] <-"Sample.name"
    }
    
    if ("Label" %in% colnames(Biobase::pData(data))){
      idx <- which(colnames(Biobase::pData(data)) == "Label")
      names(Biobase::pData(data))[idx] <-"Condition"
    }
  })



return(reactive({rv.demomode$res}))
}


