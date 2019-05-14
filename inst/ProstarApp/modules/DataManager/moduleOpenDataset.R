source(file.path(".", "modules/moduleStaticDataTable.R"),  local = TRUE)$value
source(file.path(".", "modules/DataManager/moduleConvertData.R"),  local = TRUE)$value
source(file.path(".", "modules/DataManager/moduleOpenMSnSet.R"),  local = TRUE)$value
source(file.path(".", "modules/DataManager/moduleOpenDemoDataset.R"),  local = TRUE)$value
source(file.path(".", "modules/DataManager/moduleInfoDataset.R"),  local = TRUE)$value



moduleOpenDatasetUI  <- function(id){
  ns <- NS(id)
 
  # tagList(
  #   uiOutput(ns('openUI'))
  # )
  # 
  
   
  
  
  navbarMenu("Data manager" ,
             tabPanel("Open MSnset",
                      moduleOpenMSnSetUI(ns("moduleOpenMSnSet"))
                      ),
             tabPanel("Convert",
                      value = "convertTab",
                      moduleConvertDataUI(ns("moduleProcess_Convert"))
                ),
             tabPanel("Demo data", 
                      moduleOpenDemoDatasetUI(ns("moduleOpenDemoDataset"))
                      )
  )


}




moduleOpenDataset  <- function(input, output, session, selectedPanel){
  ns <- session$ns
  
  rv.opendataset <- reactiveValues(
    tmp.convert = NULL,
    tmp.demo = NULL,
    tmp.file = NULL,
    obj = NULL,
    dataOut = NULL
  )
  
  
  rv.opendataset$tmp.convert <- callModule(module=moduleConvertData, 'moduleProcess_Convert')
  rv.opendataset$tmp.demo <- callModule(module=moduleOpenDemoDataset, 'moduleOpenDemoDataset')
  rv.opendataset$tmp.file <- callModule(module=moduleOpenMSnSet, 'moduleOpenMSnSet')
  
  
  observe({
    req(rv.opendataset$tmp.file())
    rv.opendataset$obj <- rv.opendataset$tmp.file()
  })
  
  observe({
    req(rv.opendataset$tmp.convert())
    rv.opendataset$obj <- rv.opendataset$tmp.convert()
  })

  
  observe({
    req(rv.opendataset$tmp.demo())
    rv.opendataset$obj <- rv.opendataset$tmp.demo()
    print("demo dataset loaded")
  })
  
  

  observe({
    req(rv.opendataset$obj)
    print(">>>ConfigureData")
    rv.opendataset$obj <- ConfigureData(rv.opendataset$obj)
    print(">>> test si calcul de la matrice d'adjacence")
    if (length(rv.opendataset$obj@AdjacencyMat)==0 && (rv.opendataset$obj@pipeline == 'peptide')){
    print("Calcul de la matrice d'adjacence")
      #rv.opendataset$obj@AdjacencyMat <- ComputeAdjacencyMatrices(rv.opendataset$obj@datasets[[1]])
      #pipeline$current.obj@ConnexComp <- ComputeConnexComposants(pipeline$current.obj@AdjacencyMat)
    }
    print("demo dataset loaded")
    print(rv.opendataset$obj)
    rv.opendataset$dataOut <- rv.opendataset$obj
  })
  
  
  
  
 
  
  
  #If there are already pVal values, then do no compute them 
  # if (G_logFC_Column %in% names(Biobase::fData(rv$current.obj) )){
  #   rv$resAnaDiff <- list(logFC = Biobase::fData(rv$current.obj)$logFC,
  #                         P_Value = Biobase::fData(rv$current.obj)$P_Value)
  #   rv$widgets$hypothesisTest$th_logFC <- rv$current.obj@experimentData@other$threshold_logFC
  # }
  
  # if (is.null(rv$current.obj$MSnset@experimentData@other$RawPValues ))
  #   rv$current.obj$MSnset@experimentData@other$RawPValues <- FALSE
  # 
  # rv.settings$paletteConditions <- GetExamplePalette()
  # 
  # 
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
  
  
 

return(reactive({rv.opendataset$dataOut}))
}


