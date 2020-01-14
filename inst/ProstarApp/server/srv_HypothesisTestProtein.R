

callModule(moduleProcess, "moduleProcess_HypothesisTestProtein", 
           isDone = reactive({rvModProcess$moduleHypothesisTestProteinDone}), 
           pages = reactive({rvModProcess$moduleHypothesisTestProtein}),
           rstFunc = resetModuleHypothesisTestProtein,
           forceReset = reactive({rvModProcess$moduleHypothesisTestProteinForceReset })  )


resetModuleHypothesisTestProtein <- reactive({  
  ## update widgets values (reactive values)
  resetModuleProcess("HypothesisTestProtein")
  
  rv$widgets$HypothesisTestProtein$design <- "None"
  rv$widgets$HypothesisTestProtein$method <- "None"
  rv$widgets$HypothesisTestProtein$ttest_options <- "Student"
  rv$widgets$HypothesisTestProtein$th_logFC <- 0
  rv$widgets$HypothesisTestProtein$listNomsComparaison <- NULL
  
  rv$res_AllPairwiseComparisons <- NULL
  rv$tempplot$logFCDistr <- NULL
  
  rvModProcess$moduleHypothesisTestProteinDone = rep(FALSE, 2)
  rv$current.obj <- rv$dataset[[input$datasets]]
})

callModule(module_Not_a_numeric,"HypoTestProt_test_seuillogFC", reactive({rv$widgets$HypothesisTestProtein$th_logFC}))

observeEvent(input$HypoTestProt_PerformLogFCPlot, {
  rv$widgets$HypothesisTestProtein$design<- input$HypoTestProt_anaDiff_Design
  rv$widgets$HypothesisTestProtein$method <- input$HypoTestProt_diffAnaMethod
  rv$widgets$HypothesisTestProtein$th_logFC<- as.numeric(input$HypoTestProt_seuilLogFC)
  rv$widgets$HypothesisTestProtein$ttest_options <- input$HypoTestProt_ttest_options                                                
})


output$screenHypoTestProtein1 <- renderUI({
  
  rv$current.obj
  isolate({
    NA.count<- length(which(is.na(Biobase::exprs(rv$current.obj))))
  if (NA.count > 0){
    tags$p("Your dataset contains missing values. Before using the differential analysis, you must filter/impute them")
  } else {
    tagList(
      
      tags$div(
        tags$div( style="display:inline-block; vertical-align: middle;padding-right: 20px;",
                  selectInput("HypoTestProt_anaDiff_Design", "Contrast", 
                              choices=c("None"="None", "One vs One"="OnevsOne", "One vs All"="OnevsAll"),
                              selected=rv$widgets$HypothesisTestProtein$design,
                              width='150px')
        ),
        tags$div( style="display:inline-block; vertical-align: middle;padding-right: 20px;",
                  selectInput("HypoTestProt_diffAnaMethod","Statistical test",
                              choices = anaDiffMethod_Choices,
                              selected=rv$widgets$HypothesisTestProtein$method,
                              width='150px')
        ),
        tags$div( style="display:inline-block; vertical-align: middle; padding-right: 20px;",
                  hidden( radioButtons("HypoTestProt_ttest_options", "t-tests options",choices=c("Student", "Welch"),
                                       selected=rv$widgets$HypothesisTestProtein$ttest_options,
                                       width='150px'))
        ),
        tags$div( style="display:inline-block; vertical-align: middle; padding-right: 20px;",
                  textInput("HypoTestProt_seuilLogFC", "log(FC) threshold",  
                               value=rv$widgets$HypothesisTestProtein$th_logFC,
                               width='150px'),
                  module_Not_a_numericUI("HypoTestProt_test_seuillogFC")
        ),
        tags$div( style="display:inline-block; vertical-align: middle; padding-right: 20px;",
                  uiOutput("HypoTestProt_correspondingRatio")
                  
        ),
        tags$div( style="display:inline-block; vertical-align: middle; padding-right: 20px;",
                  actionButton("HypoTestProt_PerformLogFCPlot", "Perform log FC plot",class = actionBtnClass )
                  
        )
        )
      ,
      tags$hr(),
      highchartOutput("HypoTestProt_FoldChangePlot", height="100%") 
    )
    
  }
    })
})


output$screenHypoTestProtein2 <- renderUI({
  tagList(
    uiOutput("HypoTestProt_btn_valid")
  )
})


output$HypoTestProt_correspondingRatio <- renderUI({
  ratio <- as.numeric(rv$widgets$HypothesisTestProtein$th_logFC)
  p("(FC = ", 2^(ratio), ")")
})


output$HypoTestProt_btn_valid <- renderUI({
  cond <- (rv$widgets$HypothesisTestProtein$method != "None")&&(rv$widgets$HypothesisTestProtein$design != "None")
  if (!cond){return(NULL)}
  actionButton("HypoTestProt_ValidTest","Save significance test", class = actionBtnClass)
})


observeEvent(rv$widgets$HypothesisTestProtein$method,{
  
  toggle(id = "HypoTestProt_ttest_options",  condition = (rv$widgets$HypothesisTestProtein$method == "ttests"))
})



output$HypoTestProt_FoldChangePlot <- renderHighchart({
  req(ComputeComparisons()$logFC)
  req(rv$PlotParams$paletteConditions)
  req(rv$widgets$HypothesisTestProtein$th_logFC)
  if (length(ComputeComparisons()$logFC)==0){return(NULL)}
  withProgress(message = 'Computing plot...',detail = '', value = 0.5, {
  rv$tempplot$logFCDistr <- hc_logFC_DensityPlot(ComputeComparisons()$logFC,as.numeric(rv$widgets$HypothesisTestProtein$th_logFC))
  })
})



########################################################

### calcul des comparaisons                         ####
########################################################
ComputeComparisons <- reactive({
  req(rv$widgets$HypothesisTestProtein$method)
  req(rv$widgets$HypothesisTestProtein$design)
  rv$widgets$HypothesisTestProtein$ttest_options
  if ((rv$widgets$HypothesisTestProtein$method=="None")|| (rv$widgets$HypothesisTestProtein$design=="None")) {return (NULL)}
  if (length(which(is.na(Biobase::exprs(rv$current.obj)))) > 0) { return()}
  
  rv$res_AllPairwiseComparisons <- NULL
  #isolate({
  #if (is.null(rv$current.obj@experimentData@other$Params[["HypothesisTestProtein"]])){
  withProgress(message = 'Computing comparisons ...',detail = '', value = 0.5, {
    switch(rv$widgets$HypothesisTestProtein$method,
           Limma={
             rv$res_AllPairwiseComparisons <- limmaCompleteTest(Biobase::exprs(rv$current.obj), 
                                                                Biobase::pData(rv$current.obj),
                                                                rv$widgets$HypothesisTestProtein$design) 
            },
           ttests={
             rv$res_AllPairwiseComparisons <- wrapper.t_test_Complete(rv$current.obj, 
                                                                      contrast=rv$widgets$HypothesisTestProtein$design,
                                                                      type=rv$widgets$HypothesisTestProtein$ttest_options)
           })
  rv$widgets$HypothesisTestProtein$listNomsComparaison <- colnames(rv$res_AllPairwiseComparisons$logFC)
    
  
  rvModProcess$moduleHypothesisTestProteinDone[1] <- TRUE
  })
  rv$res_AllPairwiseComparisons
#})
})




########################################################################
#
#
########################################################################
observeEvent(input$HypoTestProt_ValidTest,{ 
  #req(rv$res_AllPairwiseComparisons)
  
  #isolate({
  rv$current.obj <- DAPAR::diffAnaSave(obj = rv$current.obj, allComp = rv$res_AllPairwiseComparisons)
  
  name <- paste("HypothesisTestProtein.", rv$typeOfDataset, sep="")
  rv$current.obj <- saveParameters(rv$current.obj, name,"HypothesisTestProtein", build_ParamsList_HypothesisTestProtein())
  BuildNavbarPage()
  rvModProcess$moduleHypothesisTestProteinDone[2] <- TRUE
  UpdateDatasetWidget(rv$current.obj, name)

  
})