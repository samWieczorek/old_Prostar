

callModule(moduleProcess, "moduleProcess_HypothesisTest", 
           isDone = reactive({rvModProcess$moduleHypothesisTestDone}), 
           pages = reactive({rvModProcess$moduleHypothesisTest}),
           rstFunc = resetModuleHypothesisTest,
           forceReset = reactive({rvModProcess$moduleHypothesisTestForceReset })  )


resetModuleHypothesisTest <- reactive({  
  ## update widgets values (reactive values)
  resetModuleProcess("HypothesisTest")
    
  rv$widgets$hypothesisTest$design <- "None"
  rv$widgets$hypothesisTest$method <- "None"
  rv$widgets$hypothesisTest$ttest_options <- "Student"
  rv$widgets$hypothesisTest$th_logFC <- 0
  rv$widgets$hypothesisTest$listNomsComparaison <- NULL
  
  rv$res_AllPairwiseComparisons <- NULL
  rv$tempplot$logFCDistr <- NULL
  rvModProcess$moduleHypothesisTestDone = rep(FALSE, 2)
  rv$current.obj <- rv$dataset[[input$datasets]]
})


callModule(module_Not_a_numeric,"test_seuillogFC", reactive({rv$widgets$hypothesisTest$th_logFC}))

observeEvent(input$anaDiff_Design, ignoreInit=T,{  rv$widgets$hypothesisTest$design<- input$anaDiff_Design})
observeEvent(input$diffAnaMethod,{rv$widgets$hypothesisTest$method <- input$diffAnaMethod})
observeEvent(input$seuilLogFC,{  rv$widgets$hypothesisTest$th_logFC<- as.numeric(input$seuilLogFC)})
observeEvent(input$ttest_options,{rv$widgets$hypothesisTest$ttest_options <- input$ttest_options})

output$screenHypoTest1 <- renderUI({
  
   rv$current.obj
  isolate({
    NA.count<- length(which(is.na(Biobase::exprs(rv$current.obj))))

     
  if (NA.count > 0){
    tags$p("Your dataset contains missing values. Before using the differential analysis, you must filter/impute them")
  } else {
    tagList(
      
      tags$div(
        tags$div( style="display:inline-block; vertical-align: middle;padding-right: 20px;",
                  selectInput("anaDiff_Design", "Contrast", 
                              choices=c("None"="None", "One vs One"="OnevsOne", "One vs All"="OnevsAll"),
                              selected=rv$widgets$hypothesisTest$design,
                              width='150px')
        ),
        tags$div( style="display:inline-block; vertical-align: middle;padding-right: 20px;",
                  selectInput("diffAnaMethod","Statistical test",
                              choices = anaDiffMethod_Choices,
                              selected=rv$widgets$hypothesisTest$method,
                              width='150px')
        ),
        tags$div( style="display:inline-block; vertical-align: middle; padding-right: 20px;",
                  hidden( radioButtons("ttest_options", "t-tests options",choices=c("Student", "Welch"),
                                       selected=rv$widgets$hypothesisTest$ttest_options,
                                       width='150px'))
        ),
        tags$div( style="display:inline-block; vertical-align: middle; padding-right: 20px;",
                  textInput("seuilLogFC", "log(FC) threshold",  
                               value=rv$widgets$hypothesisTest$th_logFC,
                               width='150px'),
                  module_Not_a_numericUI("test_seuillogFC")
                  
        ),
        tags$div( style="display:inline-block; vertical-align: middle; padding-right: 20px;",
                  uiOutput("correspondingRatio")
                  
        )
        
        )
      ,
      tags$hr(),
      highchartOutput("FoldChangePlot", height="100%")
    )
    
  }
  })
})


output$screenHypoTest2 <- renderUI({
  tagList(
    uiOutput("btn_valid")
  )
})





output$correspondingRatio <- renderUI({
  
  ratio <- as.numeric(rv$widgets$hypothesisTest$th_logFC)
    
p("(FC = ", 2^(ratio), ")")
  
})


output$btn_valid <- renderUI({
  cond <- (rv$widgets$hypothesisTest$method != "None")&&(rv$widgets$hypothesisTest$design != "None")
  if (!cond){return(NULL)}
  actionButton("ValidTest","Save significance test", class = actionBtnClass)
})


observeEvent(rv$widgets$hypothesisTest$method,{
  
  toggle(id = "ttest_options",  condition = (rv$widgets$hypothesisTest$method == "ttests"))
})



output$FoldChangePlot <- renderHighchart({
  req(ComputeComparisons()$logFC)
  rv$PlotParams$paletteConditions
  rv$widgets$hypothesisTest$th_logFC
  print("ON EST DANS LA FONCTION")
  if (length(ComputeComparisons()$logFC)==0){return(NULL)}
 
  rv$tempplot$logFCDistr <- hc_logFC_DensityPlot(ComputeComparisons()$logFC,as.numeric(rv$widgets$hypothesisTest$th_logFC))
 # rv$tempplot$logFCDistr
})



########################################################

### calcul des comparaisons                         ####
########################################################
ComputeComparisons <- reactive({
  req(rv$widgets$hypothesisTest$method)
  req(rv$widgets$hypothesisTest$design)
  rv$widgets$hypothesisTest$ttest_options
  if ((rv$widgets$hypothesisTest$method=="None")|| (rv$widgets$hypothesisTest$design=="None")) {return (NULL)}
  if (length(which(is.na(Biobase::exprs(rv$current.obj)))) > 0) { return()}
  
  rv$res_AllPairwiseComparisons <- NULL
#isolate({
  #if (is.null(rv$current.obj@experimentData@other$Params[["HypothesisTest"]])){
    switch(rv$widgets$hypothesisTest$method,
           Limma={
             rv$res_AllPairwiseComparisons <- limmaCompleteTest(Biobase::exprs(rv$current.obj), 
                                                                Biobase::pData(rv$current.obj),
                                                                rv$widgets$hypothesisTest$design) 
             
           },
           ttests={
             rv$res_AllPairwiseComparisons <- wrapper.t_test_Complete(rv$current.obj, 
                                                                      contrast=rv$widgets$hypothesisTest$design,
                                                                      type=rv$widgets$hypothesisTest$ttest_options)
           })
  rv$widgets$hypothesisTest$listNomsComparaison <- colnames(rv$res_AllPairwiseComparisons$logFC)
    
  
  rvModProcess$moduleHypothesisTestDone[1] <- TRUE
  rv$res_AllPairwiseComparisons
})
#})




########################################################################
#
#
########################################################################
observeEvent(input$ValidTest,{ 
  #req(rv$res_AllPairwiseComparisons)
  
#isolate({
  rv$current.obj <- DAPAR::diffAnaSave(obj = rv$current.obj, allComp = rv$res_AllPairwiseComparisons)
  
  name <- paste("HypothesisTest.", rv$typeOfDataset, sep="")
  rv$current.obj <- saveParameters(rv$current.obj, name,"HypothesisTest", build_ParamsList_HypothesisTest())
  BuildNavbarPage()
  rvModProcess$moduleHypothesisTestDone[2] <- TRUE
  UpdateDatasetWidget(rv$current.obj, name)

#})
  
})