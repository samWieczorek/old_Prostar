rvModProcess <- reactiveValues(
  moduleFiltering = list(name = "Filtering",
                         stepsNames = c("MV filtering", "String-based filtering", "Summary", "Save"),
                         isMandatory = c(FALSE, FALSE, FALSE, TRUE),
                         ll.UI = list( uiOutput("screenFiltering1"),
                                       uiOutput("screenFiltering2"),
                                       uiOutput("screenFiltering3"),
                                       uiOutput("screenFiltering4")),
                         rstFunc = reactive({resetModuleFiltering()})),
  moduleFilteringDone = rep(FALSE, 4),
  
  moduleNormalization = list(name = "Normalization",
                             stepsNames = c("Normalization", "Validate"),
                             isMandatory = rep(FALSE,2),
                             ll.UI = list( screenStep1 = uiOutput("screenNormalization1"),
                                           screenStep2 = uiOutput("screenNormalization2")),
                             rstFunc = reactive({resetModuleNormalization()})),
  moduleNormalizationDone =  rep(FALSE,2),
  
  
  
  
  moduleAggregation = list(name = "Aggregation",
                           stepsNames = c("Aggregation 1", "Aggregation 2", "Save"),
                           isMandatory = rep(TRUE, 3),
                           ll.UI = list(screenStep1 = uiOutput("screenAggregation1"),
                                        screenStep2 = uiOutput("screenAggregation2"),
                                        screenStep3 = uiOutput("screenAggregation3")),
                          rstFunc = reactive({resetModuleAggregation()})
                          ),
  moduleAggregationDone =  rep(FALSE,3),
  
  
  
  
  
  moduleProtImputation = list(name = "ProtImputation",
                              stepsNames = c("Partially Observed Values", "Missing on Entire Condition", "Save"),
                              isMandatory = c(TRUE, FALSE, TRUE),
                              ll.UI = list(screenStep1 = uiOutput("screenProtImput1"),
                                           screenStep2 = uiOutput("screenProtImput2"),
                                           screenStep3 = uiOutput("screenProtImput3")),
                              rstFunc = reactive({resetModuleProtimputation()})
                              ),
  moduleProtImputationDone =  rep(FALSE,3),
  
  modulePepImputation = list(name = "PepImputation",
                             stepsNames = c("PeptideImputation 1", "Save"),
                             isMandatory = rep(TRUE, 2),
                             ll.UI = list(uiOutput("screenPepImputation1"),
                                          uiOutput("screenPepImputation2")),
                             rstFunc = reactive({resetModulePepImputation()})),
  modulePepImputationDone =  rep(FALSE,2),
  
  
  moduleHypothesisTest = list(name = "HypothesisTest",
                              stepsNames = c("HypothesisTest", "Save"),
                              isMandatory = rep(TRUE, 2),
                              ll.UI = list(screenStep1 = uiOutput("screenHypoTest1"),
                                          screenStep2 = uiOutput("screenHypoTest2")),
                              rstFunc = reactive({resetModuleHypothesisTest()})),
  moduleHypothesisTestDone =  rep(FALSE,2),
  
  moduleConvert = list(name = "Convert",
                       stepsNames = c("Select file", "Data Id", "Epx. & feat. data", "Build design", "Convert"),
                       isMandatory = rep(TRUE,5),
                       ll.UI = list( screenStep1 = uiOutput("Convert_SelectFile"),
                                     screenStep2 = uiOutput("Convert_DataId"),
                                     screenStep3 = uiOutput("Convert_ExpFeatData"),
                                     screenStep2 = uiOutput("Convert_BuildDesign"),
                                     screenStep3 = uiOutput("Convert_Convert")
                                    ),
                       rstFunc = reactive({resetModuleConvert()})),
  moduleConvertDone =  rep(FALSE,5),
  
  moduleAnaDiff = list(name = "AnaDiff",
                       stepsNames = c("Pairwise comparison", "P-value calibration", "FDR","Summary"),
                       isMandatory = rep(TRUE,4),
                       ll.UI = list( screenStep1 = uiOutput("screenAnaDiff1"),
                                     screenStep2 = uiOutput("screenAnaDiff2"),
                                     screenStep3 = uiOutput("screenAnaDiff3"),
                                     screenStep2 = uiOutput("screenAnaDiff4")
                       ),
                       rstFunc = reactive({resetModuleAnaDiff()})),
  moduleAnaDiffDone =  rep(FALSE,4)
)

