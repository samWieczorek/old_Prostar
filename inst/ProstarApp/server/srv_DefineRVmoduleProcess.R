rvModProcess <- reactiveValues(
  
  moduleFiltering = list(name = "Filtering",
                             stepsNames = c("MV Removal", "By MS/MS enrichment", "String-based Removal","Numerical Removal", "Summary", "Save"),
                             isMandatory = c(rep(FALSE,5),TRUE),
                             ll.UI = list( screenStep1 = uiOutput("screenFiltering1"),
                                           screenStepxxx = uiOutput("screenFilteringxxx"),
                                           screenStep2 = uiOutput("screenFiltering2"),
                                           screenStep3 = uiOutput("screenFiltering3"),
                                           screenStep4 = uiOutput("screenFiltering4"),
                                           screenStep5 = uiOutput("screenFiltering5")),
                             rstFunc = reactive({resetModuleFiltering()})),
  moduleFilteringDone =  rep(FALSE,6),
  moduleFilteringForceReset = 0,
  
  
  
  
  moduleNormalization = list(name = "Normalization",
                             stepsNames = c("Normalization", "Save"),
                             isMandatory = rep(TRUE,2),
                             forceReset = FALSE,
                             ll.UI = list( screenStep1 = uiOutput("screenNormalization1"),
                                           screenStep2 = uiOutput("screenNormalization2")),
                             rstFunc = reactive({resetModuleNormalization()})),
  moduleNormalizationDone =  rep(FALSE,2),
  moduleNormalizationForceReset = 0,
  
  
  
  moduleAggregation = list(name = "Aggregation",
                           stepsNames = c("Aggregation", "Add metadata", "Save"),
                           isMandatory = c(TRUE, FALSE, TRUE),
                           forceReset = FALSE,
                           ll.UI = list(screenStep1 = uiOutput("screenAggregation1"),
                                        screenStep2 = uiOutput("screenAggregation2"),
                                        screenStep3 = uiOutput("screenAggregation3")),
                          rstFunc = reactive({resetModuleAggregation()})
                          ),
  moduleAggregationForceReset = 0,
  moduleAggregationDone =  rep(FALSE,3),
  
  
  
  
  
  moduleProtImputation = list(name = "ProtImputation",
                              stepsNames = c("Partially Observed Values", "Missing on Entire Condition", "Save"),
                              isMandatory = c(TRUE, FALSE, TRUE),
                              forceReset = FALSE,
                              ll.UI = list(screenStep1 = uiOutput("screenProtImput1"),
                                           screenStep2 = uiOutput("screenProtImput2"),
                                           screenStep3 = uiOutput("screenProtImput3")),
                              rstFunc = reactive({resetModuleProtimputation()})
                              ),
  moduleProtImputationDone =  rep(FALSE,3),
  moduleProtImputationForceReset = 0,
  
  
  modulePepImputation = list(name = "PepImputation",
                             stepsNames = c("Imputation", "Save"),
                             isMandatory = rep(TRUE, 2),
                             forceReset = FALSE,
                             ll.UI = list(uiOutput("screenPepImputation1"),
                                          uiOutput("screenPepImputation2")),
                             rstFunc = reactive({resetModulePepImputation()})),
  modulePepImputationDone =  rep(FALSE,2),
  modulePepImputationForceReset = 0,
  
  
  moduleHypothesisTest = list(name = "HypothesisTest",
                              stepsNames = c("HypothesisTest", "Save"),
                              isMandatory = rep(TRUE, 2),
                              forceReset = FALSE,
                              ll.UI = list(screenStep1 = uiOutput("screenHypoTest1"),
                                          screenStep2 = uiOutput("screenHypoTest2")),
                              rstFunc = reactive({resetModuleHypothesisTest()})),
  moduleHypothesisTestDone =  rep(FALSE,2),
  moduleHypothesisTestForceReset = 0,
  
  
  
  moduleConvert = list(name = "Convert",
                       stepsNames = c("Select file", "Data Id", "Exp. & feat. data", "Build design", "Convert"),
                       isMandatory = rep(TRUE,5),
                       forceReset = FALSE,
                       ll.UI = list( screenStep1 = uiOutput("Convert_SelectFile"),
                                     screenStep2 = uiOutput("Convert_DataId"),
                                     screenStep3 = uiOutput("Convert_ExpFeatData"),
                                     screenStep4 = uiOutput("Convert_BuildDesign"),
                                     screenStep5 = uiOutput("Convert_Convert")
                                    ),
                       rstFunc = reactive({resetModuleConvert()})),
  moduleConvertForceReset = 0,
  moduleConvertDone =  rep(FALSE,5),
  
  moduleAnaDiff = list(name = "AnaDiff",
                       stepsNames = c("Pairwise comparison", "P-value calibration", "FDR","Summary"),
                       isMandatory = rep(TRUE,4),
                       forceReset = FALSE,
                       ll.UI = list( screenStep1 = uiOutput("screenAnaDiff1"),
                                     screenStep2 = uiOutput("screenAnaDiff2"),
                                     screenStep3 = uiOutput("screenAnaDiff3"),
                                     screenStep4 = uiOutput("screenAnaDiff4")
                       ),
                       rstFunc = reactive({resetModuleAnaDiff()})),
  moduleAnaDiffForceReset = 0,
  moduleAnaDiffDone =  rep(FALSE,4),
  
  moduleGO = list(name = "GO",
                       stepsNames = c("GO setup", "GO classification", "GO enrichment", "Parameter summary"),
                       isMandatory = c(TRUE, FALSE, FALSE, FALSE),
                       forceReset = FALSE,
                       ll.UI = list( screenStep1 = uiOutput("screenGO1"),
                                     screenStep2 = uiOutput("screenGO2"),
                                     screenStep3 = uiOutput("screenGO3"),
                                     screenStep4 = uiOutput("screenGO4")
                       ),
                       rstFunc = reactive({resetModuleGO()})),
  moduleGOForceReset = 0,
  moduleGODone =  rep(FALSE,4)
)

