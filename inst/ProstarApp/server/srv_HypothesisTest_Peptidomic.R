

callModule(moduleProcess, "moduleProcess_HypothesisTestPeptidomic", 
           isDone = reactive({rvModProcess$moduleHypothesisTestPeptidomicDone}), 
           pages = reactive({rvModProcess$moduleHypothesisTestPeptidomic}),
           rstFunc = resetModuleHypothesisTestPeptidomic,
           forceReset = reactive({rvModProcess$moduleHypothesisTestPeptidomicForceReset })  )


resetModuleHypothesisTestPeptidomic <- reactive({  
  ## update widgets values (reactive values)
  resetModuleProcess("HypothesisTestPeptidomic")
  
  rv$widgets$HypothesisTestPeptidomic$design <- "None"
  rv$widgets$HypothesisTestPeptidomic$method <- "None"
  rv$widgets$HypothesisTestPeptidomic$ttest_options <- "Student"
  rv$widgets$HypothesisTestPeptidomic$th_logFC <- 0
  rv$widgets$HypothesisTestPeptidomic$listNomsComparaison <- NULL
  
  rv$res_AllPairwiseComparisons <- NULL
  rv$tempplot$logFCDistr <- NULL
  

  rvModProcess$moduleHypothesisTestPeptidomicDone = rep(FALSE, 2)
  ##update dataset to put the previous one
  rv$current.obj <- rv$dataset[[last(names(rv$dataset))]] 
})


callModule(module_Not_a_numeric,"HypoTestPeptidomic_test_seuillogFC", reactive({rv$widgets$HypothesisTestPeptidomic$th_logFC}))

observeEvent(input$HypoTestPeptidomic_PerformLogFCPlot, {
  rv$widgets$HypothesisTestPeptidomic$design<- input$HypoTestPeptidomic_anaDiff_Design
  rv$widgets$HypothesisTestPeptidomic$method <- input$HypoTestPeptidomic_diffAnaMethod
  rv$widgets$HypothesisTestPeptidomic$th_logFC<- as.numeric(input$HypoTestPeptidomic_seuilLogFC)
  rv$widgets$HypothesisTestPeptidomic$ttest_options <- input$HypoTestPeptidomic_ttest_options                                                
})

output$screenHypoTestPeptidomic1 <- renderUI({
  
  #rv$current.obj
  isolate({
    NA.count<- length(which(is.na(Biobase::exprs(rv$current.obj))))
    if (NA.count > 0){
      tags$p("Your dataset contains missing values. Before using the differential analysis, you must filter/impute them")
    } else {
      tagList(
        
        tags$div(
          tags$div( style="display:inline-block; vertical-align: middle;padding-right: 20px;",
                    selectInput("HypoTestPeptidomic_anaDiff_Design", "Contrast", 
                                choices=c("None"="None", "One vs One"="OnevsOne", "One vs All"="OnevsAll"),
                                selected=rv$widgets$HypothesisTestPeptidomic$design,
                                width='150px')
          ),
          tags$div( style="display:inline-block; vertical-align: middle;padding-right: 20px;",
                    selectInput("HypoTestPeptidomic_diffAnaMethod","Statistical test",
                                choices = anaDiffMethod_Choices,
                                selected=rv$widgets$HypothesisTestPeptidomic$method,
                                width='150px')
          ),
          tags$div( style="display:inline-block; vertical-align: middle; padding-right: 20px;",
                    hidden( radioButtons("HypoTestPeptidomic_ttest_options", "t-tests options",choices=c("Student", "Welch"),
                                         selected=rv$widgets$HypothesisTestPepidomic$ttest_options,
                                         width='150px'))
          ),
          tags$div( style="display:inline-block; vertical-align: middle; padding-right: 20px;",
                    textInput("HypoTestPeptidomic_seuilLogFC", "log(FC) threshold",  
                              value=rv$widgets$HypothesisTestPeptidomic$th_logFC,
                              width='150px'),
                    module_Not_a_numericUI("test_seuillogFC")
          ),
          tags$div( style="display:inline-block; vertical-align: middle; padding-right: 20px;",
                    uiOutput("HypoTestPeptidomic_correspondingRatio")
                    
          ),
          tags$div( style="display:inline-block; vertical-align: middle; padding-right: 20px;",
                    actionButton("HypoTestPeptidomic_PerformLogFCPlot", "Perform log FC plot",class = actionBtnClass )
          )
        )
        ,
        tags$hr(),
        highchartOutput("HypoTestPeptidomic_FoldChangePlot", height="100%") %>% withSpinner(type=spinnerType)
      )
      
      }
    })
  })


output$screenHypoTestPeptidomic2 <- renderUI({
  tagList(
    uiOutput("HypoTestPeptidomic_btn_valid")
  )
})


output$HypoTestPeptidomic_correspondingRatio <- renderUI({
  ratio <- as.numeric(rv$widgets$HypothesisTestPeptidomic$th_logFC)
  p("(FC = ", 2^(ratio), ")")
})


output$HypoTestPeptidomic_btn_valid <- renderUI({
  cond <- (rv$widgets$HypothesisTestPeptidomic$method != "None")&&(rv$widgets$HypothesisTestPeptidomic$design != "None")
  if (!cond){return(NULL)}
  actionButton("HypoTestPeptidomic_ValidTest","Save significance test", class = actionBtnClass)
})


observeEvent(rv$widgets$HypothesisTestPeptidomic$method,{
  
  toggle(id = "HypoTestPeptidomic_ttest_options",  condition = (rv$widgets$HypothesisTestPeptidomic$method == "ttests"))
})



output$HypoTestPeptidomic_FoldChangePlot <- renderHighchart({
  #req(rv$res_AllPairwiseComparisons) 
  #rv$PlotParams$paletteConditions
  
  #data <- ComputeComparisonsPeptidomic()
  #print(str(data))
  #print(as.numeric(rv$widgets$HypothesisTestPeptidomic$th_logFC))
  #rv$tempplot$logFCDistr <- hc_logFC_DensityPlot(data$logFC,as.numeric(rv$widgets$HypothesisTestPeptidomic$th_logFC))
  #rv$tempplot$logFCDistr
  req(ComputeComparisons()$logFC)
  req(rv$PlotParams$paletteConditions)
  req(rv$widgets$HypothesisTestPeptidomic$th_logFC)
  if (length(ComputeComparisons()$logFC)==0){return(NULL)}
  withProgress(message = 'Computing plot...',detail = '', value = 0.5, {
    
    rv$tempplot$logFCDistr <- hc_logFC_DensityPlot(ComputeComparisons()$logFC,as.numeric(rv$widgets$HypothesisTestPeptidomic$th_logFC))
  })
})


########################################################

### calcul des comparaisons                         ####
########################################################
ComputeComparisons <- reactive({
  req(rv$widgets$HypothesisTestPeptidomic$method)
  req(rv$widgets$HypothesisTestPeptidomic$design)
  rv$widgets$HypothesisTestPeptidomic$ttest_options
  if ((rv$widgets$HypothesisTestPeptidomic$method=="None")|| (rv$widgets$HypothesisTestPeptidomic$design=="None")) {return (NULL)}
  if (length(which(is.na(Biobase::exprs(rv$current.obj)))) > 0) { return()}
  
  rv$res_AllPairwiseComparisons <- NULL
  #isolate({
    #if (is.null(rv$current.obj@experimentData@other$Params[["HypothesisTest"]])){
    withProgress(message = 'Computing comparisons ...',detail = '', value = 0.5, {
      switch(rv$widgets$HypothesisTestPeptidomic$method,

           Limma={
             rv$res_AllPairwiseComparisons <- limmaCompleteTest(Biobase::exprs(rv$current.obj), 
                                                                Biobase::pData(rv$current.obj),
                                                                rv$widgets$HypothesisTestPeptidomic$design) 
             
           },
           ttests={
             rv$res_AllPairwiseComparisons <- wrapper.t_test_Complete(rv$current.obj, 
                                                                      contrast=rv$widgets$HypothesisTestPeptidomic$design,
                                                                      type=rv$widgets$HypothesisTestPeptidomic$ttest_options)
           })
    rv$widgets$HypothesisTestPeptidomic$listNomsComparaison <- colnames(rv$res_AllPairwiseComparisons$logFC)
    
    
    rvModProcess$moduleHypothesisTestPeptidomicDone[1] <- TRUE
    })
    rv$res_AllPairwiseComparisons
  #})
})



########################################################################
#
#
########################################################################
observeEvent(input$HypoTestPeptidomic_ValidTest,{ 
  # req(rv$current.obj)
  req(rv$res_AllPairwiseComparisons)
  
  if (length(which(is.na(Biobase::exprs(rv$current.obj)))) > 0) { return()}
  
  ### Save RAW data
  #isolate({
  rv$current.obj <- DAPAR::diffAnaSave(obj = rv$current.obj, allComp = rv$res_AllPairwiseComparisons)
  
  
  name <- paste("HypothesisTestPeptidomic.", rv$typeOfDataset, sep="")
  rv$current.obj <- saveParameters(rv$current.obj, name,"HypothesisTestPeptidomic", build_ParamsList_HypothesisTestPeptidomic())
  BuildNavbarPage()
  rvModProcess$moduleHypothesisTestPeptidomicDone[2] <- TRUE
  UpdateDatasetWidget(rv$current.obj, name)
  
  #})
  
})