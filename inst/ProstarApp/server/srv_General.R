
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
observeEvent( input$datasets,ignoreInit = TRUE,{ 

    isolate({
        if (!is.null(input$datasets)) {
            rv$current.obj <- rv$dataset[[input$datasets]]

        if (!is.null( rv$current.obj))
            rv$typeOfDataset <- rv$current.obj@experimentData@other$typeOfData
        }

    })
    
})





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



######################################
loadObjectInMemoryFromConverter <- function(){
  req(rv$current.obj)
  
  name <- paste0("Original", ".", rv$typeOfDataset)
  rv$dataset[[name]] <- rv$current.obj
  ClearNavbarPage()
  BuildNavbarPage()
    
}




GetCurrentObjName <- reactive({
  rv$datasets[[input$datasets]]})


resetModuleProcess <- function(moduleName, obj){
  
  switch (moduleName,
          Aggregation ={rv$widgets$aggregation = list(includeSharedPeptides = "Yes2",
                                           operator = "Mean",
                                           considerPeptides = 'allPeptides',
                                           proteinId = "None",
                                           topN = 3)
          rvModProcess$moduleAggregation = list(name = "Aggregation",
                                                stepsNames = c("Aggregation 1", "Aggregation 2", "Save"),
                                                isMandatory = rep(TRUE, 3),
                                                ll.UI = list( screenStep1 = uiOutput("screenAggregation1"),
                                                              screenStep2 = uiOutput("screenAggregation2"),
                                                              screenStep3 = uiOutput("screenAggregation3")))
          rvModProcess$moduleAggregationDone =  rep(FALSE,3)
          },
          Filtering ={
            rv$deleted.mvLines <- NULL
            rv$deleted.stringBased.exprsData <- NULL
            rv$deleted.stringBased <- NULL
            rv$deleted.stringBased.fData <- NULL
            rv$deleted.stringBased <- NULL
            rv$widgets$filtering <- list(ChooseFilters = "None",
                                                    seuilNA = 0,
                                                    DT_filterSummary = data.frame(Filtre=NULL, 
                                                           Prefix=NULL,
                                                           nbDeleted=NULL, 
                                                           Total=NULL, 
                                                           stringsAsFactors=F))
            
            rvModProcess$moduleFiltering = list(name = "Filtering",
                                                stepsNames = c("MV filtering", "String-based filtering", "Summary", "Save"),
                                                isMandatory = c(FALSE, FALSE, FALSE, TRUE),
                                                ll.UI = list( uiOutput("screenFiltering1"),
                                                              uiOutput("screenFiltering2"),
                                                              uiOutput("screenFiltering3"),
                                                              uiOutput("screenFiltering4")))
            rvModProcess$moduleFilteringDone = rep(FALSE, 4)
          },
          Normalization ={rv$widgets$normalization <- list(method = "None",
                                             type = "None",
                                             varReduction = FALSE,
                                             quantile = 0.15,
                                             spanLOESS = 0.7)
          rv$normalizationFamily <- NULL
          rv$normalizationMethod <- NULL 
          
          rvModProcess$moduleNormalization = list(name = "Normalization",
                                                  stepsNames = c("Normalization", "Validate"),
                                                  isMandatory = rep(FALSE,2),
                                                  ll.UI = list( screenStep1 = uiOutput("screenNormalization1"),
                                                                screenStep2 = uiOutput("screenNormalization2")))
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
                                                  stepsNames = c("PeptideImputation 1", "Save"),
                                                  isMandatory = c(TRUE, TRUE),
                                                  ll.UI = list(uiOutput("screenPepImputation1"),
                                                               uiOutput("screenPepImputation2")))
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
          rvModProcess$moduleHypothesisTestDone =  rep(FALSE,2)
          },
          
          
          
          Convert ={
            
            rvModProcess$moduleConvert = list(name = "Convert",
                                              stepsNames = c("Select file", "Data Id", "Epx. & feat. data", "Build design", "Convert"),
                                              isMandatory = rep(TRUE,5),
                                              ll.UI = list( screenStep1 = uiOutput("Convert_SelectFile"),
                                                            screenStep2 = uiOutput("Convert_DataId"),
                                                            screenStep3 = uiOutput("Convert_ExpFeatData"),
                                                            screenStep2 = uiOutput("Convert_BuildDesign"),
                                                            screenStep3 = uiOutput("Convert_Convert")
                                              ))
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
                                    NbSelected = 0)
            
            rvModProcess$moduleAnaDiff = list(name = "AnaDiff",
                                              stepsNames = c("Pairwise comparison", "P-value calibration", "FDR","Summary"),
                                              isMandatory = rep(TRUE,4),
                                              ll.UI = list( screenStep1 = uiOutput("screenAnaDiff1"),
                                                            screenStep2 = uiOutput("screenAnaDiff2"),
                                                            screenStep3 = uiOutput("screenAnaDiff3"),
                                                            screenStep2 = uiOutput("screenAnaDiff4")
                                              ))
            
            rvModProcess$moduleAnaDiffDone =  rep(FALSE,4)
            
            
            
          }
          )
}



###-------------------------------------------------------------------
ClearMemory <- function(){
  resetModuleProcess("Aggregation")
  resetModuleProcess("Normalization")
  resetModuleProcess("Filtering")
  resetModuleProcess("PepImputation")
  resetModuleProcess("ProtImputation")
  resetModuleProcess("HypothesisTest")
  resetModuleProcess("Convert")
  resetModuleProcess("AnaDiff")
  
  ########
  ### Settings
  ########
  
  ########
  ### Parameters
  ######## 
    rv$current.obj = NULL
    rv$current.obj.name = NULL
    
    
    rv$listLogFC <- list()
    
    # variable to keep memory of previous datasets before 
    # transformation of the data
    rv$dataset = list()
    rv$tab1 = NULL
    rv$dirname = ""
    rv$dirnameforlink = ""
    
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
   
    
}







#-------------------------------------------------------------
rv <- reactiveValues(
  

    dataset = list(),
    # Variable that contains the log for the current R session
    
    dirname = "",
    dirnameforlink = "",
    
  widgets = list(
                 filtering = list(ChooseFilters = "None",
                                  seuilNA = 0,
                                  DT_filterSummary = data.frame(Filtre=NULL, 
                                                                Prefix=NULL,
                                                                nbDeleted=NULL, 
                                                                Total=NULL, 
                                                                stringsAsFactors=F)),
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
                      NbSelected = 0)
  )
    )




observeEvent(input$LinkToUsefulLinksTab, {
  updateTabsetPanel(session, 'navPage', "usefulLinksTab")
})


current <- reactiveVal(
  list(pipeline=NULL,
       module = NULL,
       dataset = NULL)
  )

pipelines <- reactiveValues(
  pep = list(
    modules = c("Pep_Filter", "Pep_Norm", "Pep_Imput", "Pep_Hyptest"),
    datasets = c("Original", "Filtered", "Normalized", "Imputed", "HypTested"),
 p2p = list(
   modules = c("P2p_Filter", "P2p_Norm", "P2p_Imput", "P2p_Hyptest"),
   datasets = c("Original", "Filtered", "Normalized", "Imputed", "HypTested"),
 prot = list(
   modules = c("Prot_Filter", "Prot_Norm", "Prot_Imput", "Prot_Hyptest"),
   datasets = c("Original", "Filtered", "Normalized", "Imputed", "HypTested"))
 )
)
)

