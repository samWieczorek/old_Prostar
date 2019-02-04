rvModProcess <- reactiveValues(
moduleFiltering = list(stepsNames = c("MV filtering", "String-based filtering", "Validate"),
                       isMandatory = c(FALSE, FALSE, TRUE),
                       ll.UI = list( uiOutput("mv_Filtering"),
                                     uiOutput("stringBased_Filtering"),
                                     uiOutput("valid_Filtering"))),
moduleFilteringDone = c(FALSE, FALSE, FALSE),

moduleNormalization = list(stepsNames = c("Normalization"),
                           isMandatory = c(FALSE),
                           ll.UI = list( screenStep1 = uiOutput("screenNormalization"))),
moduleNormalizationDone =  rep(FALSE,1),




moduleAggregation = list(stepsNames = c("Aggregation 1", "Aggregation 2"),
                         isMandatory = c(TRUE, TRUE),
                         ll.UI = list( screenStep1 = uiOutput("screenAggregation1"),
                                       screenStep2 = uiOutput("screenAggregation2"))),
moduleAggregationDone =  rep(FALSE,2),





moduleProtImputation = list(stepsNames = c("Partially Observed Values", "Missing on Entire Condition", "Validate & Save"),
                            isMandatory = c(TRUE, FALSE, TRUE),
                            ll.UI = list( screenStep1 = uiOutput("screenProtImput1"),
                                          screenStep2 = uiOutput("screenProtImput2"),
                                          screenStep3 = uiOutput("screenProtImput3")
                            )),
moduleProtImputationDone =  rep(FALSE,3),

modulePepImputation = list(stepsNames = c("PeptideImputation 1"),
                           isMandatory = c(TRUE),
                           ll.UI = list(uiOutput("screenPepImputation"))),
modulePepImputationDone =  rep(FALSE,1),


moduleHypothesisTest = list(stepsNames = c("HypothesisTest"),
                            isMandatory = c(TRUE),
                            ll.UI = list( screenStep1 = uiOutput("screenHypoTest1"))),
moduleHypothesisTestDone =  rep(FALSE,1),

moduleConvert = list(stepsNames = c("Select file", "Data Id", "Epx. & feat. data", "Build design", "Convert"),
                     isMandatory = rep(TRUE,5),
                     ll.UI = list( screenStep1 = uiOutput("Convert_SelectFile"),
                                   screenStep2 = uiOutput("Convert_DataId"),
                                   screenStep3 = uiOutput("Convert_ExpFeatData"),
                                   screenStep2 = uiOutput("Convert_BuildDesign"),
                                   screenStep3 = uiOutput("Convert_Convert")
                     )),
moduleConvertDone =  rep(FALSE,5)
)

resetRVModProcess <- function(){
  ##variables for navigation
  rvModProcess$moduleFiltering = list(stepsNames = c("MV filtering", "String-based filtering", "Validate"),
                            isMandatory = c(FALSE, FALSE, TRUE),
                            ll.UI = list( uiOutput("mv_Filtering"),
                                          uiOutput("stringBased_Filtering"),
                                          uiOutput("valid_Filtering")))
  rvModProcess$moduleFilteringDone = c(FALSE, FALSE, FALSE)
  
  rvModProcess$moduleNormalization = list(stepsNames = c("Normalization"),
                                isMandatory = c(FALSE),
                                ll.UI = list( screenStep1 = uiOutput("screenNormalization")))
  rvModProcess$moduleNormalizationDone =  rep(FALSE,1)
  
  
  
  
  rvModProcess$moduleAggregation = list(stepsNames = c("Aggregation 1", "Aggregation 2"),
                              isMandatory = c(TRUE, TRUE),
                              ll.UI = list( screenStep1 = uiOutput("screenAggregation1"),
                                            screenStep2 = uiOutput("screenAggregation2")))
  rvModProcess$moduleAggregationDone =  rep(FALSE,2)
  
  
  
  
  
  rvModProcess$moduleProtImputation = list(stepsNames = c("Partially Observed Values", "Missing on Entire Condition", "Validate & Save"),
                                 isMandatory = c(TRUE, FALSE, TRUE),
                                 ll.UI = list( screenStep1 = uiOutput("screenProtImput1"),
                                               screenStep2 = uiOutput("screenProtImput2"),
                                               screenStep3 = uiOutput("screenProtImput3")
                                 ))
  rvModProcess$moduleProtImputationDone =  rep(FALSE,3)
  
  rvModProcess$modulePepImputation = list(stepsNames = c("PeptideImputation 1"),
                                isMandatory = c(TRUE),
                                ll.UI = list(uiOutput("screenPepImputation")))
  rvModProcess$modulePepImputationDone =  rep(FALSE,1)
  
  
  rvModProcess$moduleHypothesisTest = list(stepsNames = c("HypothesisTest"),
                                 isMandatory = c(TRUE),
                                 ll.UI = list( screenStep1 = uiOutput("screenHypoTest1")))
  rvModProcess$moduleHypothesisTestDone =  rep(FALSE,1)
  
  rvModProcess$moduleConvert = list(stepsNames = c("Select file", "Data Id", "Epx. & feat. data", "Build design", "Convert"),
                          isMandatory = rep(TRUE,5),
                          ll.UI = list( screenStep1 = uiOutput("Convert_SelectFile"),
                                        screenStep2 = uiOutput("Convert_DataId"),
                                        screenStep3 = uiOutput("Convert_ExpFeatData"),
                                        screenStep2 = uiOutput("Convert_BuildDesign"),
                                        screenStep3 = uiOutput("Convert_Convert")
                          ))
  rvModProcess$moduleConvertDone =  rep(FALSE,5)
  
}