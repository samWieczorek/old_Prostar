rvModProcess <- reactiveValues(
  
  moduleFiltering = list(name = "Filtering",
                             stepsNames = c("MV filtering", "String-based filtering","Numerical filtering", "Summary", "Save"),
                             isMandatory = c(rep(FALSE,4),TRUE),
                             ll.UI = list( screenStep1 = uiOutput("screenFiltering1"),
                                           screenStep2 = uiOutput("screenFiltering2"),
                                           screenStep3 = uiOutput("screenFiltering3"),
                                           screenStep4 = uiOutput("screenFiltering4"),
                                           screenStep5 = uiOutput("screenFiltering5")),
                             rstFunc = reactive({resetModuleFiltering()})),
  moduleNormalizationDone =  rep(FALSE,5),
  
  
  
  
  moduleNormalization = list(name = "Normalization",
                             stepsNames = c("Normalization", "Save"),
                             isMandatory = rep(TRUE,2),
                             ll.UI = list( screenStep1 = uiOutput("screenNormalization1"),
                                           screenStep2 = uiOutput("screenNormalization2")),
                             rstFunc = reactive({resetModuleNormalization()})),
  moduleNormalizationDone =  rep(FALSE,2),
  
  
  
  
  moduleAggregation = list(name = "Aggregation",
                           stepsNames = c("Aggregation", "Add metadata", "Save"),
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
                             stepsNames = c("Imputation", "Save"),
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
  
  moduleHypothesisTestPeptide = list(name = "HypothesisTestPeptide",
                              stepsNames = c("HypothesisTestPeptide", "Save"),
                              isMandatory = rep(TRUE, 2),
                              ll.UI = list(screenStep1 = uiOutput("screenHypoTestPeptide1"),
                                           screenStep2 = uiOutput("screenHypoTestPeptide2")),
                              rstFunc = reactive({resetModuleHypothesisTestPeptide()})),
  moduleHypothesisTestPeptideDone =  rep(FALSE,2),
  
  
  moduleHypothesisTestPeptidomic = list(name = "HypothesisTestPeptidomic",
                                     stepsNames = c("HypothesisTestPeptidomic", "Save"),
                                     isMandatory = rep(TRUE, 2),
                                     ll.UI = list(screenStep1 = uiOutput("screenHypoTestPeptidomic1"),
                                                  screenStep2 = uiOutput("screenHypoTestPeptidomic2")),
                                     rstFunc = reactive({resetModuleHypothesisTestPeptidomic()})),
  moduleHypothesisTestPeptidomicDone =  rep(FALSE,2),
  
  
  
  moduleConvert = list(name = "Convert",
                       stepsNames = c("Select file", "Data Id", "Exp. & feat. data", "Build design", "Convert"),
                       isMandatory = rep(TRUE,5),
                       ll.UI = list( screenStep1 = uiOutput("Convert_SelectFile"),
                                     screenStep2 = uiOutput("Convert_DataId"),
                                     screenStep3 = uiOutput("Convert_ExpFeatData"),
                                     screenStep4 = uiOutput("Convert_BuildDesign"),
                                     screenStep5 = uiOutput("Convert_Convert")
                                    ),
                       rstFunc = reactive({resetModuleConvert()})),
  moduleConvertDone =  rep(FALSE,5),
  
  moduleAnaDiff = list(name = "AnaDiff",
                       stepsNames = c("Pairwise comparison", "P-value calibration", "FDR","Summary"),
                       isMandatory = rep(TRUE,4),
                       ll.UI = list( screenStep1 = uiOutput("screenAnaDiff1"),
                                     screenStep2 = uiOutput("screenAnaDiff2"),
                                     screenStep3 = uiOutput("screenAnaDiff3"),
                                     screenStep4 = uiOutput("screenAnaDiff4")
                       ),
                       rstFunc = reactive({resetModuleAnaDiff()})),
  moduleAnaDiffDone =  rep(FALSE,4),
  
  moduleGO = list(name = "GO",
                  stepsNames = c("GO setup", "GO classification", "GO enrichment", "Parameter summary"),
                  isMandatory = c(TRUE, FALSE, FALSE, FALSE),
                  ll.UI = list( screenStep1 = uiOutput("screenGO1"),
                                screenStep2 = uiOutput("screenGO2"),
                                screenStep3 = uiOutput("screenGO3"),
                                screenStep4 = uiOutput("screenGO4")
                  ),
                  rstFunc = reactive({resetModuleGO()})),
  moduleGODone =  rep(FALSE,4)
)

