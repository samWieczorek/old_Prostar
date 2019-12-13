
shinyOutput <- function(FUN,id,num,...) {
  inputs <- character(num)
  for (i in seq_len(num)) {
    inputs[i] <- as.character(FUN(paste0(id,i),label=NULL,...))
  }
  inputs
}


# function for dynamic inputs in DT
shinyInput <- function(FUN,id,num,...) {
  inputs <- character(num)
  for (i in seq_len(num)) {
    inputs[i] <- as.character(FUN(paste0(id,i),label=NULL,...))
  }
  inputs
}


# function to read DT inputs
shinyValue <- function(id,num) {
  unlist(lapply(seq_len(num),function(i) {
    value <- input[[paste0(id,i)]]
    if (is.null(value)) NA else value
  }))
}



GetCurrentDatasetName <- reactive({
  name <- last(names(rv$dataset))
  name
})





getDataForExprs <- function(obj){
 
  
  test.table <- as.data.frame(round(Biobase::exprs(obj),digits=rv$settings_nDigits))
 # print(paste0("tutu:",obj@experimentData@other$OriginOfValues))
  if (!is.null(obj@experimentData@other$OriginOfValues)){ #agregated dataset
   test.table <- cbind(test.table, 
                        Biobase::fData(obj)[,obj@experimentData@other$OriginOfValues])
  # print(paste0("tutu:",head(test.table)))
   
  } else {
    test.table <- cbind(test.table, 
                        as.data.frame(matrix(rep(NA,ncol(test.table)*nrow(test.table)), nrow=nrow(test.table))))
    #print(paste0("tata:",head(test.table)))
    }
  return(test.table)

}


getData <- reactive({
  req(rv$settings_nDigits)
  rv$current$obj
  
  test.table <- round(Biobase::exprs(rv$current.obj),digits=rv$settings_nDigits)
  test.table
})





GetDatasetOverview <- reactive({
  req(rv$current.obj)
  
  
    columns <- c("Number of samples","Number of conditions",
                 "Number of lines", "Number of missing values", "% of missing values", 
                 "Number of empty lines")
    
    do <- data.frame(Definition= columns,
                     Value=rep(0,length(columns)))
    
    NA.count<- length(which(is.na(Biobase::exprs(rv$current.obj))==TRUE))
    pourcentage <- 100 * round(NA.count/(ncol(rv$current.obj)*nrow(rv$current.obj)), digits=4)
    nb.empty.lines <- sum(apply(
      is.na(as.matrix(Biobase::exprs(rv$current.obj))), 1, all))
    
    
    val <- c(ncol((Biobase::exprs(rv$current.obj))),
             length(unique(Biobase::pData(rv$current.obj)$Condition)),
             nrow((Biobase::exprs(rv$current.obj))),
             NA.count,
             pourcentage,
             nb.empty.lines)
    do$Value <- val
  
  do
})

BuildParamDataProcessingDT <- reactive({
  req(rv$current.obj)
  req(input$datasets)
  tmp.params <- rv$current.obj@experimentData@other$Params
  #ind <- which(input$datasets == names(tmp.params))
  df <- data.frame(Dataset = names(tmp.params),
                   Process = rep("",length(names(tmp.params))),
                   Parameters = rep("",length(names(tmp.params))),
                   stringsAsFactors = FALSE)
  
  for (iData in 1:length(names(tmp.params))) {
    p <- tmp.params[[iData]]
    processName <- ifelse(is.null(names(tmp.params[[iData]])), "-",names(tmp.params[[iData]]))
    # if (processName=='Imputation'){
    #   processName <- paste0(processName,rv$typeOfData)
    # }
    df[iData, "Process"] <- processName
    if (length(tmp.params[[iData]][[processName]])==0){
      df[iData,"Parameters"]<- '-'
    } else {
      
      
      df[iData,"Parameters"]<- do.call(paste0("getTextFor",processName), 
                                       list(l.params=tmp.params[[iData]][[processName]]))
    }
  }
  
  df
})



BuildParamDataMiningDT <- reactive({
  req(rv$current.obj)
  
    nbLines <- sum((as.character(input$selectComparison) != "None"), !is.null(rv$params.GO))
  if (nbLines ==0) {
    df <- NULL
  } else {
  df <- data.frame(Dataset = rep(input$datasets,length(names(nbLines))),
                   Process = rep("",length(names(nbLines))),
                   Parameters = rep("",length(names(nbLines))),
                   stringsAsFactors = FALSE)
 
  if (!is.null(as.character(input$selectComparison))){
    df[1,"Dataset"]<- input$datasets
    df[1,"Process"]<- "Differential analysis"
    #ll <- setNames(split(rv$widgets$anaDiff[,2], seq(nrow(rv$widgets$anaDiff))), rv$widgets$anaDiff[,1])
    df[1,"Parameters"]<- getTextForAnaDiff(rv$widgets$anaDiff)
    } else {}
  
    }
  df
})




data <- eventReactive(rv$current$obj, {
  rv$settings_nDigits
  rv$current$obj
  
  test.table <- round(Biobase::exprs(rv$current.obj),digits=rv$settings_nDigits)
  test.table
}, ignoreNULL = FALSE)










callModule(modulePopover,"modulePopover_dataset", 
           data = reactive(list(title = p(if(is.null(rv$current.obj.name)) "No dataset" else paste0(rv$current.obj.name)),

                                content="Before each processing step, a backup of the current dataset is stored. It is possible to reload one of them at any time.",
                                color = 'white')))


observeEvent(input$navbar,{
    if (input$navbar=="stop")
        stopApp()
})

getDatasetName <- reactive({
    req(rv$current.obj.name)
    rv$current.obj.name
})


##' Get back to a previous object ---------------------------------------
##' @author Samuel Wieczorek
# observeEvent( req(input$datasets),ignoreInit = TRUE,{ 
# 
#     isolate({
#       rv$current.obj <- rv$dataset[[input$datasets]]
# 
#         # if (rv$typeOfDataset != rv$current.obj@experimentData@other$typeOfData){
#         #       BuildNavbarPage()
#         #     }
#             
#        if (!is.null( rv$current.obj)){
#             rv$typeOfDataset <- rv$current.obj@experimentData@other$typeOfData
#         }
# 
#     })
#     
# })





output$datasetAbsPanel <- renderUI({
    req(rv$current.obj.name)
     div(
         div(
             style="display:inline-block; vertical-align: middle;",
             modulePopoverUI("modulePopover_dataset")
             ),
         div(
             style="display:inline-block; vertical-align: middle;",
             selectInput("datasets", "", choices = list("None"="None"),width = '200px')
         )
     )
})

###-------------------------------------------------------------------
onStop(function() cat("Session stopped.\n"))



session$onSessionEnded(function() {
    #setwd(tempdir())
    graphics.off()
    unlink(sessionID, recursive = TRUE)
    unlink(paste(tempdir(), sessionID, commandLogFile, sep="/"),recursive = TRUE)
    unlink(paste(tempdir(), sessionID, sep="/"),recursive = TRUE)
    unlink(paste(tempdir(), "*Rmd", sep="/"),recursive = TRUE)
    unlink(paste(tempdir(), "*html", sep="/"))
    unlink(paste(tempdir(), "*log", sep="/"))
    unlink("www/*pdf")
    
    #unlink( normalizePath(paste(tempdir(), 'report.Rmd',sep="/")))
    #do.call(file.remove, list(list.files(tempdir(), full.names = TRUE)))
    #rm(rv$current.obj, rv$matAdj) 
    gc()
    cat("Session stopped. Temporary files cleaned up\n")
  
    
    #rm(list= list(myListOfThings))
    #stopApp()
})


###-------------------------------------------------------------------
ClearUI <- reactive({
        
        updateSelectInput(session, 
                          "datasets",  
                          choices = G_noneStr)
        updateRadioButtons(session,"typeOfData",selected = typePeptide )
        updateRadioButtons(session, "checkDataLogged", selected="no")
        
        updateSelectInput(session, "idBox", selected = NULL)
        
        updateSelectizeInput(session,"eData.box",choices = NULL, selected=NULL)
        updateTextInput(session,"filenameToCreate",value= "")
        updateTextInput(session,"nameExport",value= "")
        
        updateCheckboxInput(session, "replaceAllZeros",value = TRUE)
        updateRadioButtons(session,
                           inputId = "ChooseFilters", 
                           selected = gFilterNone)
        
    })



#


ComputeAdjacencyMatrices <- reactive({
  rv$matAdj <- NULL
  matSharedPeptides <- BuildAdjacencyMatrix(rv$current.obj, rv$proteinId, FALSE)
  print("mat adj 1 done")
  matUniquePeptides <- BuildAdjacencyMatrix(rv$current.obj, rv$proteinId, TRUE)
  print("mat adj 2 done")
  
  rv$matAdj <- list(matWithSharedPeptides=matSharedPeptides, matWithUniquePeptides=matUniquePeptides)
  rv$matAdj
})

ComputeConnexComposants <- reactive({
  req(rv$matAdj)
  print(dim(rv$matAdj$matWithSharedPeptides))
  ll1 <- get.pep.prot.cc(rv$matAdj$matWithSharedPeptides)
  ll2 <- get.pep.prot.cc(rv$matAdj$matWithUniquePeptides)
    
  rv$CC <- list(allPep = ll1,
                onlyUniquePep = ll2)
  print("end ComputeConnexComposants")
  
  rv$CC
})


###-------------------------------------

Compute_PCA_nbDimensions <- reactive({
  # ncp should not be greater than...
  nmax <- 12  
  # pour info, ncp = nombre de composantes ou de dimensions dans les r?sultats de l'ACP
  
  y <- exprs(rv$current.obj)
  nprot <- dim(y)[1]
  # If too big, take the number of conditions.
  n <- dim(y)[2] 
  
  if (n > nmax){
    n <- length(unique(Biobase::pData(rv$current.obj)$Condition))
  }
  
  
  ncp <- min(n, nmax)
  ncp
})




######################################
loadObjectInMemoryFromConverter <- function(){
  # req(rv$current.obj)
   
  rv$proteinId <-rv$current.obj@experimentData@other$proteinId
  if (is.null(rv$current.obj@experimentData@other$typeOfData)) {
    rv$typeOfDataset <- ""
    } else {
      rv$typeOfDataset <- rv$current.obj@experimentData@other$typeOfData
    }


  withProgress(message = 'Loading memory',detail = '', value = 0, {
    incProgress(0.5, detail = 'Miscellaneous updates')
  colnames(fData(rv$current.obj)) <- gsub(".", "_", colnames(fData(rv$current.obj)), fixed=TRUE)
  names(rv$current.obj@experimentData@other) <- gsub(".", "_", names(rv$current.obj@experimentData@other), fixed=TRUE)
  pData(rv$current.obj)$Sample.name <- gsub(".", "_", pData(rv$current.obj)$Sample.name, fixed=TRUE)

    #If there are already pVal values, then do no compute them
    # if (G_logFC_Column %in% names(Biobase::fData(rv$current.obj) )){
    #     rv$resAnaDiff <- list(logFC = Biobase::fData(rv$current.obj)$logFC,
    #                           P_Value = Biobase::fData(rv$current.obj)$P_Value)
    #     rv$widgets$hypothesisTest$th_logFC <- rv$current.obj@experimentData@other$threshold_logFC
    #     #rv$widgets$anaDiff$th_pval  <- rv$current.obj@experimentData@other$threshold_p_value
    # }

  if (is.null(rv$current.obj@experimentData@other$RawPValues ))
    rv$current.obj@experimentData@other$RawPValues <- FALSE

    rv$PlotParams$paletteConditions <- GetExamplePalette()

    if (rv$typeOfDataset == "peptide" && !is.null(rv$proteinId) && (rv$proteinId != "")){
     print("begin compute adjacency matrix")
        incProgress(0.6, detail = 'Compute Adjacency Matrices')
        ComputeAdjacencyMatrices()
      incProgress(0.7, detail = 'Compute Connex Composants')
      ComputeConnexComposants()
    }

      if (length(which(is.na(Biobase::exprs(rv$current.obj)))) == 0)
        {
          rv$res.pca <- wrapper.pca(rv$current.obj, rv$PCA_varScale, ncp=Compute_PCA_nbDimensions())
      }
   
    name <- paste0("Original", ".", rv$typeOfDataset)
    if (is.null(rv$current.obj@experimentData@other$Params))
       rv$current.obj <- saveParameters(rv$current.obj, name,"-")
     else {
       names(rv$current.obj@experimentData@other$Params) <- paste0('prev.',names(rv$current.obj@experimentData@other$Params))
     }

  UpdateDatasetWidget(rv$current.obj, name)
  incProgress(0.9, detail = 'Build UI') 
  ClearNavbarPage()
  BuildNavbarPage()
  })
}



UpdateDatasetWidget <- function(obj, name){
  rv$processSaved <- TRUE
  rv$dataset[[name]] <- obj
  updateSelectInput(session, "datasets", choices = names(rv$dataset), selected = name)
}


###-------------------------------------------------------------------
writeToCommandLogFile <- function(txt, verbose = FALSE){
    rv$commandLog <- c(rv$commandLog, txt)
}

###-------------------------------------------------------------------





GetCurrentObjName <- reactive({rv$datasets[[input$datasets]]})

createPNGFromWidget <- function(tempplot, pattern){
  tmp_filename <- paste0(pattern, '.html')
  png_filename <- paste0(pattern, '.png')
  htmlwidgets::saveWidget(widget = tempplot, file = paste(tempdir(), tmp_filename, sep="/"))
  webshot::webshot(url = paste(tempdir(), tmp_filename, sep="/"),
                   file = paste(tempdir(), png_filename, sep="/"),
                   delay = 1,
                   zoom = zoomWebshot)
}


resetModuleProcess <- function(moduleName){
  
  switch (moduleName,
          Filtering ={rv$widgets$filtering <- list(ChooseFilters = "None",
                                                         seuilNA = 0,
                                                         DT_filterSummary = data.frame(Filter=NULL, 
                                                                                       Prefix=NULL,
                                                                                       nbDeleted=NULL, 
                                                                                       Total=NULL, 
                                                                                       stringsAsFactors=F),
                                                         DT_numfilterSummary = data.frame(Filter=NULL, 
                                                                                          Condition=NULL,
                                                                                          nbDeleted=NULL, 
                                                                                          Total=NULL, 
                                                                                          stringsAsFactors=F)
                                                         )

          updateSelectInput(session, "ChooseFilters", selected = rv$widgets$filtering$ChooseFilters)
          updateSelectInput(session, "seuilNA", selected = rv$widgets$filtering$seuilNA)
          
          rvModProcess$moduleFiltering = list(name = "Filtering",
                                                  stepsNames = c("MV filtering", "String-based filtering","Numerical filtering", "Summary", "Validate"),
                                                  isMandatory = rep(FALSE,5),
                                                  ll.UI = list( screenStep1 = uiOutput("screenFiltering1"),
                                                                screenStep2 = uiOutput("screenFiltering2"),
                                                                screenStep3 = uiOutput("screenFiltering3"),
                                                                screenStep4 = uiOutput("screenFiltering4"),
                                                                screenStep5 = uiOutput("screenFiltering5")))
          rvModProcess$moduleFilteringDone =  rep(FALSE,5)
          },
          
          
          Aggregation ={
            rv$widgets$aggregation = list(includeSharedPeptides = "Yes2",
                                           operator = "Mean",
                                           considerPeptides = 'allPeptides',
                                           proteinId = "None",
                                           topN = 3,
                                           filterProtAfterAgregation = NULL,
                                           columnsForProteinDataset.box = NULL,
                                           nbPeptides = 0
                                          )
            
                        rvModProcess$moduleAggregation = list(name = "Aggregation",
                                                stepsNames = c("Aggregation", "Add metadata", "Save"),
                                                isMandatory = c(TRUE, FALSE, TRUE),
                                                ll.UI = list( screenStep1 = uiOutput("screenAggregation1"),
                                                              screenStep2 = uiOutput("screenAggregation2"),
                                                              screenStep3 = uiOutput("screenAggregation3")))
                        ## update widgets in UI
                        updateSelectInput(session, "proteinId", selected = rv$widgets$aggregation$proteinId)
                        updateRadioButtons(session, "radioBtn_includeShared", selected = rv$widgets$aggregation$includeSharedPeptides)
                        updateRadioButtons(session, "AggregationConsider", selected = rv$widgets$aggregation$considerPeptides)
                        updateNumericInput(session, "nTopn", value=rv$widgets$aggregation$topN)
                        updateRadioButtons(session, "AggregationOperator", selected = rv$widgets$aggregation$operator)
                        
                        
                        rvModProcess$moduleAggregationDone =  rep(FALSE,3)
                        },
          
          Normalization ={
            rv$widgets$normalization <- list(method = "None",
                                             type = "None",
                                             varReduction = FALSE,
                                             quantile = 0.15,
                                             spanLOESS = 0.7)
                             
          
                          rvModProcess$moduleNormalization = list(name = "Normalization",
                                                  stepsNames = c("Normalization", "Validate"),
                                                  isMandatory = rep(FALSE,2),
                                                  ll.UI = list( screenStep1 = uiOutput("screenNormalization1"),
                                                                screenStep2 = uiOutput("screenNormalization2")))
                          ## update widgets in UI
                          updateSelectInput(session, "normalization.method", selected = rv$widgets$normalization$method)
                          updateSelectInput(session, "normalization.type", selected = rv$widgets$normalization$type)
                          updateTextInput(session,"spanLOESS", value = rv$widgets$normalization$spanLOESS)
                          updateTextInput(session, "normalization.quantile", value = rv$widgets$normalization$quantile)
                          updateCheckboxInput(session, "normalization.variance.reduction", value = rv$widgets$normalization$varReduction)

                          
                          rvModProcess$moduleNormalizationDone =  rep(FALSE,2)
                        },
          
          
          
          PepImputation ={rv$widgets$peptideImput <- list( pepLevel_algorithm = "None",
                                               pepLevel_basicAlgorithm = "None",
                                               pepLevel_detQuantile = 2.5,
                                               pepLevel_detQuant_factor = 1,
                                               pepLevel_imp4p_nbiter = 10,
                                               pepLevel_imp4p_withLapala = FALSE,
                                               pepLevel_imp4p_qmin = 2.5,
                                               pepLevel_imp4pLAPALA_distrib = "beta",
                                               pepLevel_KNN_n = 10)
          rvModProcess$modulePepImputation = list(name = "PepImputation",
                                                  stepsNames = c("Imputation", "Save"),
                                                  isMandatory = c(TRUE, TRUE),
                                                  ll.UI = list(uiOutput("screenPepImputation1"),
                                                               uiOutput("screenPepImputation2")))
          ## update widgets in UI
          updateSelectInput(session,"peptideLevel_missing.value.algorithm", selected = rv$widgets$peptideImput$pepLevel_algorithm)
          updateSelectInput(session,"peptideLevel_missing.value.basic.algorithm", selected = rv$widgets$peptideImput$pepLevel_basicAlgorithm)
          updateNumericInput(session,"peptideLevel_detQuant_quantile", value = rv$widgets$peptideImput$pepLevel_detQuantile)
          updateNumericInput(session,"peptideLevel_detQuant_factor", value = rv$widgets$peptideImput$pepLevel_detQuant_factor)
          updateNumericInput(session,"KNN_n",  value = rv$widgets$peptideImput$pepLevel_KNN_n)
          updateNumericInput(session,"peptideLevel_imp4p_nbiter", value = rv$widgets$peptideImput$pepLevel_imp4p_nbiter)
          updateCheckboxInput(session,"peptideLevel_imp4p_withLapala", value = rv$widgets$peptideImput$pepLevel_imp4p_withLapala)
          updateNumericInput(session,"peptideLevel_imp4p_qmin",  value = rv$widgets$peptideImput$pepLevel_imp4p_qmin)
          updateRadioButtons(session, "peptideLevel_imp4pLAPALA_distrib", selected = rv$widgets$peptideImput$pepLevel_imp4pLAPALA_distrib)
          
          
          rvModProcess$modulePepImputationDone =  rep(FALSE,2)
          },
          
          
          
          ProtImputation ={rv$widgets$proteinImput <- list(POV_algorithm = "None",
                                               POV_detQuant_quantile = 2.5,
                                               POV_detQuant_factor = 1,
                                               POV_KNN_n = 10,
                                               MEC_algorithm = "None",
                                               MEC_detQuant_quantile = 2.5,
                                               MEC_detQuant_factor = 1,
                                               MEC_fixedValue= 0)
          rvModProcess$moduleProtImputation = list(name = "ProtImputation",
                                                   stepsNames = c("Partially Observed Values", "Missing on Entire Condition", "Save"),
                                                   isMandatory = c(TRUE, FALSE, TRUE),
                                                   ll.UI = list( screenStep1 = uiOutput("screenProtImput1"),
                                                                 screenStep2 = uiOutput("screenProtImput2"),
                                                                 screenStep3 = uiOutput("screenProtImput3")
                                                   ))
          
          ## update widgets in UI
          updateSelectInput(session,"POV_missing.value.algorithm",selected=rv$widgets$proteinImput$POV_algorithm)
          updateSelectInput(session,"MEC_missing.value.algorithm", selected=rv$widgets$proteinImput$MEC_algorithm)
          updateNumericInput(session,"POV_detQuant_quantile", value = rv$widgets$proteinImput$POV_detQuant_quantile)
          updateNumericInput(session,"POV_detQuant_factor", value = rv$widgets$proteinImput$POV_detQuant_factor)
          updateNumericInput(session,"KNN_nbNeighbors", value = rv$widgets$proteinImput$POV_KNN_n)
          updateNumericInput(session, "MEC_detQuant_quantile", value = rv$widgets$proteinImput$MEC_detQuant_quantile)
          updateNumericInput(session, "MEC_detQuant_factor", value = rv$widgets$proteinImput$MEC_detQuant_factor)
          updateNumericInput(session, "MEC_fixedValue", value = rv$widgets$proteinImput$MEC_fixedValue)
          
          
          rvModProcess$moduleProtImputationDone =  rep(FALSE,3)
          rv$imputePlotsSteps = list(step0 = NULL,
                                     step1 = NULL,
                                     step2 = NULL)
          },
          
          
          
          HypothesisTest ={
            rv$widgets$hypothesisTest = list(design = "None",
                                                 method = "None",
                                                 ttest_options = "Student",
                                                 th_logFC = 0,
                                                 listNomsComparaison = NULL)
            
          rvModProcess$moduleHypothesisTest = list(name = "HypothesisTest",
                                                   stepsNames = c("HypothesisTest", "Save"),
                                                   isMandatory = c(TRUE, TRUE),
                                                   ll.UI = list( screenStep1 = uiOutput("screenHypoTest1"),
                                                                 screenStep2 = uiOutput("screenHypoTest2")))
          ## update widgets in UI
          updateSelectInput(session,"anaDiff_Design", selected = rv$widgets$hypothesisTest$design)
          updateSelectInput(session,"diffAnaMethod", selected = rv$widgets$hypothesisTest$method)
          updateRadioButtons(session,"ttest_options", selected = rv$widgets$hypothesisTest$ttest_options)
          updateTextInput(session, "seuilLogFC", value= rv$widgets$hypothesisTest$th_logFC)
          
          rv$res_AllPairwiseComparisons <- NULL
          rv$tempplot$logFCDistr <- NULL
          
          rvModProcess$moduleHypothesisTestDone =  rep(FALSE,2)
          },
          
          
          
          Convert ={
            rv$widgets$Convert = list(datafile = NULL,
                                      selectIdent = FALSE,
                                             convert_proteinId = character(0),
                                             idBox = "Auto ID",
                                             eDatabox = character(0),
                                             typeOfData = "peptide",
                                             checkDataLogged = "no",
                                             replaceAllZeros =TRUE,
                                             convert_reorder = "no",
                                            XLSsheets = character(0),
                                            design = NULL)
            
            
            rvModProcess$moduleConvert = list(name = "Convert",
                                              stepsNames = c("Select file", "Data Id", "Exp. & feat. data", "Build design", "Convert"),
                                              isMandatory = rep(TRUE,5),
                                              ll.UI = list( screenStep1 = uiOutput("Convert_SelectFile"),
                                                            screenStep2 = uiOutput("Convert_DataId"),
                                                            screenStep3 = uiOutput("Convert_ExpFeatData"),
                                                            screenStep2 = uiOutput("Convert_BuildDesign"),
                                                            screenStep3 = uiOutput("Convert_Convert")
                                              ))
            ## update widgets in UI
            updateCheckboxInput(session,"selectIdent", value = rv$widgets$Convert$selectIdent)
            updateSelectInput(session,"convert_proteinId",selected = rv$widgets$convert_proteinId)
            updateSelectInput(session,"idBox", selected = rv$widgets$Convert$idBox)
            updateRadioButtons(session, "typeOfData", selected=rv$widgets$Convert$typeOfData)
            updateRadioButtons(session, "checkDataLogged", selected=rv$widgets$Convert$checkDataLogged)
            updateCheckboxInput(session,"replaceAllZeros", value= rv$widgets$Convert$replaceAllZeros)
            updateCheckboxInput(session,"convert_reorder", value= rv$widgets$Convert$convert_reorder)
            updateSelectInput(session,"XLSsheets", selected= rv$widgets$Convert$XLSsheets)
            
            rvModProcess$moduleConvertDone =  rep(FALSE,5)
            
          },
          
          
          
          AnaDiff = {
            rv$nbTotalAnaDiff = NULL
            rv$nbSelectedAnaDiff = NULL
            rv$nbSelectedTotal_Step3 = NULL
            rv$nbSelected_Step3 = NULL  
            rv$conditions <- list(cond1 = NULL, cond2 = NULL)
            rv$calibrationRes <- NULL
            rv$errMsgcalibrationPlot <- NULL
            rv$errMsgcalibrationPlotALL <- NULL
            rv$pi0 <- NULL
            
            rv$widgets$anaDiff <- list(Comparison = "None",
                                    Condition1 = "",
                                    Condition2 = "",
                                    swapVolcano = FALSE,
                                    filterType = "None",
                                    filter_th_NA = 0,
                                    calibMethod = 'None',
                                    numValCalibMethod = 0,
                                    th_pval = 0,
                                    FDR = 0,
                                    NbSelected = 0,
                                    nBinsHistpval = 80,
                                    downloadAnaDiff = "All",
                                    tooltipInfo=NULL)
            
            rvModProcess$moduleAnaDiff = list(name = "AnaDiff",
                                              stepsNames = c("Pairwise comparison", "P-value calibration", "FDR","Summary"),
                                              isMandatory = rep(TRUE,4),
                                              ll.UI = list( screenStep1 = uiOutput("screenAnaDiff1"),
                                                            screenStep2 = uiOutput("screenAnaDiff2"),
                                                            screenStep3 = uiOutput("screenAnaDiff3"),
                                                            screenStep2 = uiOutput("screenAnaDiff4")
                                              ))
            ## update widgets in UI
            #if (!is.null(input$showpvalTable) )updateCheckboxInput(session, 'showpvalTable', value = FALSE)
            updateSelectInput(session, "selectComparison", selected=rv$widgets$anaDiff$Comparison)
            updateSelectInput(session, "AnaDiff_seuilNA", selected = rv$widgets$anaDiff$filter_th_NA)
            updateRadioButtons(session, "AnaDiff_ChooseFilters", selected=rv$widgets$anaDiff$filterType)
            updateSelectInput(session, "tooltipInfo", selected=character(0))
            updateSelectInput(session,"calibrationMethod", selected = rv$widgets$anaDiff$calibMethod)
            updateNumericInput(session,"numericValCalibration",value = rv$widgets$anaDiff$numValCalibMethod)
            updateSelectInput(session,"nBinsHistpval",selected=rv$widgets$anaDiff$nBinsHistpval)
            updateTextInput(session, "seuilPVal",  value=rv$widgets$anaDiff$th_pval)
            updateRadioButtons(session, "downloadAnaDiff", selected="All")
            updateRadioButtons(session, "swapVolcano", selected = rv$widgets$anaDiff$swapVolcano)
            
            rvModProcess$moduleAnaDiffDone =  rep(FALSE,4)
          },
          
          GO = {
            
            rv$widgets$go <- list(
              sourceOfProtID = NULL,
              idFrom = "UNIPROT",
              Organism = character(0),
              Ontology = character(0),
              UniprotIDCol = character(0),
              UNIPROTID_File = NULL,
              GO_level = 2,
              universe =NULL,
              UniverseFile = NULL,
              pvalueCutoff = 0.01,
              
              ProtIDList=NULL,
              gene=NULL,
              proteinsNotMapped=NULL,
              ratio=NULL,
              uniprotID=NULL,
              universeData=NULL,
              enrichGO_data=NULL,
              groupGO_data=NULL
            )
            
  
            
            rvModProcess$moduleGO = list(name = "GO",
                                              stepsNames = c("GO setup", "GO classification", "GO enrichment", "Parameter summary"),
                                              isMandatory = c(TRUE, FALSE, FALSE, FALSE),
                                              ll.UI = list( screenStep1 = uiOutput("screenGO1"),
                                                            screenStep2 = uiOutput("screenGO2"),
                                                            screenStep3 = uiOutput("screenGO3"),
                                                            screenStep2 = uiOutput("screenGO4")
                                              ))
            
            rvModProcess$moduleGODone =  rep(FALSE,4)
            
          }
          )
}



###-------------------------------------------------------------------
ClearMemory <- function(){
  
  ########
  ### Settings
  ########
  
  rv$processSaved = FALSE
  rv$current.navPage = NULL
  rv$current.comp = NULL
  rv$colorsVolcanoplot = list(In=orangeProstar, Out='lightgrey')
  rv$colorsTypeMV = list(MEC=orangeProstar, POV='lightblue')
  rv$typeOfPalette = 'predefined'
  rv$whichGroup2Color = 'Condition'
  rv$PCA_axes = c(1,2)
  rv$PCA_varScale = TRUE
  rv$choosePalette = 'Dark2'
  
  rv$res.pca = NULL
  ########
  ### Parameters
  ######## 
  rv$dataset = list()
  rv$current.obj = NULL
    rv$current.obj.name = NULL
    rv$deleted.mvLines = NULL
    rv$deleted.stringBased.exprsData = NULL
    rv$deleted.stringBased.fData = NULL
    rv$deleted.stringBased = NULL
    rv$deleted.numeric.exprsData = NULL
    rv$deleted.numeric = NULL
    rv$deleted.numeric.fData = NULL
    
    rv$listLogFC <- list()
    
    # variable to keep memory of previous datasets before 
    # transformation of the data
    # Variable that contains the log for the current R session
    rv$text.log = data.frame(Date="", 
                             Dataset="", 
                             History="", 
                             stringsAsFactors=F)
    rv$tableVersions = NULL
    
    rv$tab1 = NULL
    rv$dirname = ""
    rv$dirnameforlink = ""
    rv$temp.aggregate = NULL
    
    rv$typeOfDataset = ""
    rv$proteinId = NULL
    rv$commandLog =  "" 
    rv$matAdj = NULL
    rv$CC = NULL
    rv$resAnaDiff = list(logFC=NULL, P_Value=NULL, condition1 = NULL, condition2 = NULL)
    rv$res_AllPairwiseComparisons = data.frame()
    rv$indexNA = NULL
    rv$pourcentageNA = 0
    rv$nb.empty.lines = 0
    rv$nbDeleted = 0
    rv$nbDeletedInfos = NULL
    rv$fdr = NULL
    #rv$ValidFilteringClicked = FALSE
    rv$ValidImputationClicked = FALSE
    

    rv$impute_Step = 0
    
    rv$settings_nDigits = 10
    rv$hot = NULL
    rv$newOrder = NULL
    rv$designChecked = NULL
    rv$designSaved = FALSE
    rv$conditionsChecked = NULL
    
    rv$newOrder = NULL
    rv$designChecked = NULL
    rv$designSaved = FALSE
    rv$conditionsChecked = NULL
    rv$nbPOVimputed = 0
    rv$nbMVimputed = 0
    
    rv$updateDesign_designSaved=FALSE
    rv$updateDesign_designChecked=NULL
    rv$updateDesign_hot=NULL
    rv$updateDesign_newOrder=NULL
    rv$updateDesign_conditionsChecked=NULL
    
    rv$outfile = NULL
    rv$designIsValid = FALSE
    rv$MECIndex = NULL
    rv$tempDatasetImputation = NULL
    rv$text.log <- data.frame(Date="", 
                              Dataset="", 
                              History="", 
                              stringsAsFactors=F)
    rv$GOWarningMessage = NULL
    
    rv$iDat = NULL
    
    
    
    rv$tempplot = list(Density = NULL,
                       corrMatrix = NULL,
                       varDist = NULL,
                       mvHisto_HC = NULL,
                       mvHisto_perLines_HC = NULL,
                       histo_missvalues_per_lines_per_conditions = NULL)
    rv$PlotParams = list(legDS = NULL,
                         corrMatrixGradient = defaultGradientRate,
                         legDS_Violinplot = NULL,
                         heatmap.linkage = 'complete',
                          heatmap.distance = "euclidean",
                        paletteConditions = RColorBrewer::brewer.pal(8,"Dark2"),
                      legendForSamples = NULL
                      )
    rv$indProgressDemomode = 0
    rv$AggregProtStats = data.frame(name = c("Number of peptides",
                                              "Number of specific peptides",
                                              "Number of shared peptides", 
                                              "Number of proteins",
                                              "Number of proteins only defined by specific peptides",
                                              "Number of proteins only defined by shared peptides",
                                              "Number of proteins defined both by shared and specific peptides"),
                                     nb = rep(0,7))
    rv$distance = "euclidean"
    
    
    
    
    
    unlink(paste(tempdir(), sessionID, commandLogFile, sep="/"))
    unlink("www/*pdf")
    
    
    resetModuleProcess("Aggregation")
    resetModuleProcess("Normalization")
    resetModuleProcess("Filtering")
    resetModuleProcess("PepImputation")
    resetModuleProcess("ProtImputation")
    resetModuleProcess("HypothesisTest")
    #resetModuleProcess("Convert")
    resetModuleProcess("AnaDiff")
    
    
    
}






#-------------------------------------------------------------
rv <- reactiveValues(
  UI_TabsList = NULL,
  UI_fileSourced = NULL,
  SRV_fileSourced = NULL,
  
  processSaved = FALSE,
  
  current.navPage = NULL,
  # variable to handle the current object that will be showed
    current.comp = NULL,
    current.obj = NULL,
    current.obj.name = NULL,
    deleted.mvLines = NULL,
    deleted.stringBased.exprsData = NULL,
    deleted.stringBased.fData = NULL,
    deleted.stringBased = NULL,
    deleted.numeric.exprsData = NULL,
    deleted.numeric = NULL,
    deleted.numeric.fData = NULL,

  pi0 = NULL,
  typeOfPalette = 'predefined',
  whichGroup2Color = 'Condition',
  PCA_axes = c(1,2),
  PCA_varScale = TRUE,
  choosePalette = 'Dark2',
  res.pca = NULL,
  
  init.distance = "euclidean",
   outfile = NULL,
  tableVersions = NULL,
  
  colorsVolcanoplot = list(In=orangeProstar, Out='lightgrey'),
  colorsTypeMV = list(MEC=orangeProstar, POV='lightblue'),
    # variable to keep memory of previous datasets before 
    # transformation of the data
    dataset = list(),
    # Variable that contains the log for the current R session
    text.log = data.frame(Date="", 
                          Dataset="", 
                          History="", 
                          stringsAsFactors=F),
    listLogFC = list(),
    
    tab1 = NULL,
    dirname = "",
    dirnameforlink = "",
    conditions = list(cond1 = NULL, cond2 = NULL),
    temp.aggregate = NULL,
    #params.anaDiff = data.frame(param = c('Condition1', 'Condition2', 'Comparison', 'swapVolcano','filterType', 'filter_th_NA', 'calibMethod', 'numValCalibMethod', 'th_pval', 'FDR', 'NbSelected'),
    #                            value = c("", "", "", 'FALSE', "", '0', "", '','1e-60', 0, '0'),
    #                            stringsAsFactors = FALSE ),
 
    # design = list(designChecked=NULL,
    #                  hot=NULL,
    #                  newOrder=NULL,
    #                  conditionsChecked=NULL,
    #                  designSaved=FALSE),
  widgets = list(
                  filtering = list(ChooseFilters = "None",
                                    seuilNA = 0,
                                    DT_filterSummary = data.frame(Filter=NULL, 
                                                                  Prefix=NULL,
                                                                  nbDeleted=NULL, 
                                                                  Total=NULL, 
                                                                  stringsAsFactors=F),
                                    DT_numfilterSummary = data.frame(Filter=NULL, 
                                                                     Condition=NULL,
                                                                     nbDeleted=NULL, 
                                                                     Total=NULL, 
                                                                    stringsAsFactors=F)
                                   ),
  normalization=list(method = "None",
                                      type = "None",
                                      varReduction = FALSE,
                                      quantile = 0.15,
                                      spanLOESS = 0.7),
   aggregation = list(includeSharedPeptides = "Yes2",
                                    operator = "Mean",
                                    considerPeptides = 'allPeptides',
                                    proteinId = "None",
                                    topN = 3),
       hypothesisTest = list(design = "None",
                            method = "None",
                            ttest_options = "Student",
                            th_logFC = 0,
                            listNomsComparaison = NULL),
       peptideImput = list( pepLevel_algorithm = "None",
                            pepLevel_basicAlgorithm = "detQuantile",
                            pepLevel_detQuantile = 2.5,
                            pepLevel_detQuant_factor = 1,
                            pepLevel_imp4p_nbiter = 10,
                            pepLevel_imp4p_withLapala = FALSE,
                            pepLevel_imp4p_qmin = 2.5,
                            pepLevel_imp4pLAPALA_distrib = "beta",
                            pepLevel_KNN_n = 10),
       proteinImput = list(POV_algorithm = "None",
                           POV_detQuant_quantile = 2.5,
                           POV_detQuant_factor = 1,
                           POV_KNN_n = 10,
                           MEC_algorithm = "None",
                           MEC_detQuant_quantile = 2.5,
                           MEC_detQuant_factor = 1,
                           MEC_fixedValue= 0),
       anaDiff = list(Comparison = "None",
                      Condition1 = "",
                      Condition2 = "",
                      swapVolcano = FALSE,
                      filterType = "None",
                      filter_th_NA = 0,
                      calibMethod = 'None',
                      numValCalibMethod = 0,
                      th_pval = 0,
                      FDR = 0,
                      NbSelected = 0,
                      nBinsHistpval = 80,
                      downloadAnaDiff = "All",
                      tooltipInfo=NULL),
        go = list(
          sourceOfProtID = NULL,
          idFrom = "UNIPROT",
          Organism = character(0),
          Ontology = character(0),
          UniprotIDCol = character(0),
          UNIPROTID_File = NULL,
          GO_level = 2,
          universe =NULL,
          UniverseFile = NULL,
          pvalueCutoff = 0.01
        )
  ),
    hot = NULL,
    newOrder = NULL,
    designChecked = NULL,
    designSaved = FALSE,
    conditionsChecked = NULL,
    
    
    settings_nDigits = 10,
    calibrationRes = NULL,
    errMsgcalibrationPlot = NULL,
    errMsgcalibrationPlotALL = NULL,
    typeOfDataset = "",
  proteinId = NULL,
    #ValidFilteringClicked = FALSE,
    ValidImputationClicked = FALSE,
    commandLog = "", 
    normalizationFamily = NULL,
    normalizationMethod = NULL, 
    matAdj = NULL,
    CC = NULL,
    resAnaDiff = list(logFC=NULL, P_Value=NULL, condition1 = NULL, condition2 = NULL),
    res_AllPairwiseComparisons = data.frame(),
    progressImputation = 0,
    indexNA = NULL,
    IP_Client= "",
    pourcentageNA = 0,
    nb.empty.lines = 0,
    nbDeleted = 0,
    nbDeletedInfos = NULL,
    fdr = NULL,
    nbSelectedAnaDiff = NULL,
    nbTotalAnaDiff = NULL,
    nbSelectedTotal_Step3 = NULL,
    nbSelected_Step3 = NULL,


    GOWarningMessage = NULL,

    impute_Step = 0,

    iDat = NULL,
    tempDatasetImputation = NULL,
    MECIndex = NULL,
     nbPOVimputed = 0,
    nbMVimputed = 0,
  imputePlotsSteps = list(step0 = NULL,
                            step1 = NULL,
                            step2 = NULL),
    tempplot = list(Density = NULL,
                    corrMatrix = NULL,
                    mvHisto_HC = NULL,
                    mvHisto_perLines_HC = NULL,
                    histo_missvalues_per_lines_per_conditions = NULL),
    PlotParams = list(legDS = NULL,
                      corrMatrixGradient = defaultGradientRate,
                      legDS_Violinplot = NULL,
                      heatmap.linkage = 'complete',
                      heatmap.distance = "euclidean",
                      paletteConditions = RColorBrewer::brewer.pal(8,"Dark2"),
                      legendForSamples = NULL
                      ),
    indProgressDemomode = 0,
    AggregProtStats = data.frame(name = c("Number of peptides",
                                              "Number of specific peptides",
                                              "Number of shared peptides", 
                                              "Number of proteins",
                                              "Number of proteins only defined by specific peptides",
                                              "Number of proteins only defined by shared peptides",
                                              "Number of proteins defined both by shared and specific peptides"),
                                     nb = rep(0,7))
    
    )







###-------------------------------------------------------------------
catchToList <- function(expr) {
    val <- NULL
    myWarnings <- NULL
    myErrors <- NULL
    wHandler <- function(w) {
        myWarnings <<- c(myWarnings, w$message)
        invokeRestart("muffleWarning")
    }
    myError <- NULL
    eHandler <- function(e) {
        myError <<- c(myErrors, e$message)
        NULL
    }
    val <- tryCatch(withCallingHandlers(expr, warning = wHandler), 
                    error = eHandler)
    list(value = val, warnings = myWarnings, error=myError)
} 






###-------------------------------------------------------------------
# output$currentObjLoaded <- reactive({
#     #rv$current.obj
#     return(!is.null(isolate({rv$current.obj})))})


retroCompatibility <- reactive({
  req(rv$current.obj)
  if ("FC" %in% colnames(Biobase::fData(rv$current.obj))){
    idx <- which(colnames(Biobase::fData(rv$current.obj)) == "FC")
    names(Biobase::fData(rv$current.obj))[idx] <-"logFC"
  }
  
  if ("Experiment" %in% colnames(Biobase::pData(rv$current.obj))){
    idx <- which(colnames(Biobase::pData(rv$current.obj)) == "Experiment")
    names(Biobase::pData(rv$current.obj))[idx] <-"Sample.name"
  }
  
  if ("Label" %in% colnames(Biobase::pData(rv$current.obj))){
    idx <- which(colnames(Biobase::pData(rv$current.obj)) == "Label")
    names(Biobase::pData(rv$current.obj))[idx] <-"Condition"
  }
})












NeedsUpdate <- reactive({
  req(rv$current.obj)
  
  PROSTAR.version <- rv$current.obj@experimentData@other$Prostar_Version
  
  if (!is.null(PROSTAR.version) && (compareVersion(PROSTAR.version,"1.12.9") != -1)
      && (DAPAR::check.design(Biobase::pData(rv$current.obj))$valid))
  {return (FALSE)}
  
  else {
    return(TRUE)
  }
})


observeEvent(input$LinkToUsefulLinksTab, {
  updateTabsetPanel(session, 'navPage', "usefulLinksTab")
})



Get_ParamValue <- function(pp, key){
  switch(pp,
         params.anaDiff= df <- rv$widgets$anaDiff
  ) 
  
  return(df[which(df$param==key),]$value)
}




buildWritableVector <- function(v){
  t <- "c("
  for (i in v){
    t <- paste(t, "\"", as.character(i), "\"", sep="")
    if (i == last(v)) {t <- paste(t, ")", sep="")}
    else {t <- paste(t, ",", sep="")}
  }
  return(t)
}



GetBioconductorVersions <- function(){
  ll.versions <- list()
  
  DAPARdata.version <- Prostar.version <- DAPAR.version <- NULL
  tryCatch({
    require(XML)
    Prostar.html <- readHTMLTable("http://bioconductor.org/packages/release/bioc/html/Prostar.html")
    DAPAR.html <- readHTMLTable("http://bioconductor.org/packages/release/bioc/html/DAPAR.html")
    DAPARdata.html <- readHTMLTable("http://bioconductor.org/packages/release/data/experiment/html/DAPARdata.html")
    
    ll.versions$Prostar <-as.character(Prostar.html[[3]][2][1,])
    ll.versions$DAPAR <-as.character(DAPAR.html[[3]][2][1,])
    ll.versions$DAPARdata <- as.character(DAPARdata.html[[3]][2][1,])
    
  }, warning = function(w) {
    return()
  }, error = function(e) {
    return()
  }, finally = {
    #cleanup-code 
  })
  
  
  return (ll.versions)
}


GetLocalVersions <- function(){
  local.version <- list()
  #loc.pkgs <-c("Prostar.loc", "DAPAR.loc", "DAPARdata.loc")
  local.version <- list(Prostar = installed.packages(lib.loc=Prostar.loc)["Prostar","Version"],
                   DAPAR = installed.packages(lib.loc=DAPAR.loc)["DAPAR","Version"],
                   DAPARdata = installed.packages(lib.loc=DAPARdata.loc)["DAPARdata","Version"])
  
  
  return(local.version)
}



getPackagesVersions2 <- reactive({
  
  outOfDate <- "(Out of date)"
  dev <- "(Devel)"
  
  bioconductor.version <-GetBioconductorVersions()
  local.version <- GetLocalVersions()

  names <- c(as.character(tags$a(href="http://www.bioconductor.org/packages/release/bioc/html/Prostar.html", "Prostar")), 
             as.character(tags$a(href="http://www.bioconductor.org/packages/release/bioc/html/DAPAR.html", "DAPAR")), 
             as.character(tags$a(href="http://www.bioconductor.org/packages/release/data/experiment/html/DAPARdata.html", "DAPARdata")))
  
  
  df <- data.frame("Name" = names,
                   "Installed packages"= unlist(local.version), 
                   "Bioc release" =  unlist(bioconductor.version),
                   stringsAsFactors = FALSE)
  
  if (!is.null(local.version$Prostar) && !is.null(local.version$DAPAR)) {
    tryCatch({
      compare.prostar <- compareVersion(local.version$Prostar,bioconductor.version$Prostar)
      if (compare.prostar == 0){}
      if (compare.prostar == 1){
        df[1,"Name"] <-   paste(names[1],  "<strong>",dev, "</strong>", sep=" ")
        }
      if (compare.prostar==-1){
        df[1,"Name"] <-   paste(names[1], "<strong>", outOfDate, "</strong>", sep=" ")
        }
      
      compare.dapar <- compareVersion(local.version$DAPAR,bioconductor.version$DAPAR)
        if (compare.dapar == 0){}
      if (compare.dapar == 1){df[2,"Name"] <-   paste(names[2],  "<strong>",dev, "</strong>", sep=" ")}
      if (compare.dapar ==-1){
        df[2,"Name"] <-   paste(names[2],  "<strong>",outOfDate, "</strong>", sep=" ")
      }
      
      if (compareVersion(local.version$DAPARdata,bioconductor.version$DAPARdata) == 0){}
      if (compareVersion(local.version$DAPARdata , bioconductor.version$DAPARdata) == 1){
        df[3,"Name"] <-   paste(names[3],  "<strong>",dev, "</strong>", sep=" ")
        }
      if (compareVersion(local.version$DAPARdata , bioconductor.version$DAPARdata)==-1){
        df[3,"Name"] <-   paste(names[3],  "<strong>",outOfDate, "</strong>", sep=" ")
      }
    }, warning = function(w) {
      return()
    }, error = function(e) {
      return()
    }, finally = {
      #cleanup-code 
    })
    
  }
  
  df
})


buildTable <- function(text, color, colorCurrentPos){
  paste0("     ", text, "     ")
  rows.color <- rows.text <-  rows.cursor <- list()
  rows.text <- list()
  for( i in 1:length( color ) ) {
    rows.color[[i]] <-lapply( color[i], function( x ) tags$th(  style=paste0("background-color:", x,"; height: 20px;" ) ))
    rows.cursor[[i]] <-lapply( colorCurrentPos[i], function( x ) tags$th(  style=paste0("background-color:", x,"; height: 5px;" ) ))
    rows.text[[i]] <- lapply( text[i], function( x ) tags$td( x ) ) 
  }
  
  html.table <-  tags$table(style = "width: 100%; text-align: center;border: 1;border-collapse: separate;border-spacing: 10px;padding-top: 0px;",
                            tags$tr( rows.color ),
                            tags$tr( rows.cursor ),
                            tags$tr( rows.text )
  )
  return(html.table)
  
}




GetOnlineZipVersion <- function(){
  
  thepage <- readLines('http://prabig-prostar.univ-lyon1.fr/ProstarZeroInstall/')
  substr(thepage[12], regexpr("Prostar_",thepage[12])[1], 2+regexpr("zip",thepage[12])[1])
  
  
  thetable <- readHTMLTable('http://prabig-prostar.univ-lyon1.fr/ProstarZeroInstall/', stringsAsFactors=FALSE)
  onlineZipVersion <- thetable[[1]]$Name[3]
  
  return(onlineZipVersion)
}

