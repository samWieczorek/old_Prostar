callModule(moduleProcess, "moduleProcess_HypothesisTestPeptide", 
           isDone = reactive({rvModProcess$moduleHypothesisTestPeptideDone}), 
           pages = reactive({rvModProcess$moduleHypothesisTestPeptide}),
           rstFunc = resetModuleHypothesisTestPeptide,
           forceReset = reactive({rvModProcess$moduleHypothesisTestPeptideForceReset })  )



resetModuleHypothesisTestPeptide <- reactive({  
  ## update widgets values (reactive values)
  resetModuleProcess("HypothesisTestPeptide")
  
  
  rv$widgets$HypothesisTestPeptide$design <- "None"
  rv$widgets$HypothesisTestPeptide$method <- "None"
  rv$widgets$HypothesisTestPeptide$ttest_options <- "Student"
  rv$widgets$HypothesisTestPeptide$th_logFC <- 0
  rv$widgets$HypothesisTestPeptide$listNomsComparaison <- NULL
  
  rv$res_AllPairwiseComparisons <- NULL
  rv$tempplot$logFCDistr <- NULL
  
  rvModProcess$moduleHypothesisTestPeptideDone = rep(FALSE, 2)
  ##update dataset to put the previous one
  rv$current.obj <- rv$dataset[[last(names(rv$dataset))]] 
})


callModule(module_Not_a_numeric,"HypoTestPept_test_seuillogFC", reactive({rv$widgets$HypothesisTestPeptide$th_logFC}))


observeEvent(input$HypoTestPept_PerformLogFCPlot, {
  rv$widgets$HypothesisTestPeptide$design<- input$HypoTestPept_anaDiff_Design
  rv$widgets$HypothesisTestPeptide$method <- input$HypoTestPept_diffAnaMethod
  rv$widgets$HypothesisTestPeptide$th_logFC<- as.numeric(input$HypoTestPept_seuilLogFC)
  rv$widgets$HypothesisTestPeptide$ttest_options <- input$HypoTestPept_ttest_options                                                
})


output$screenHypoTestPeptide1 <- renderUI({
  
  # req(rv$current.obj)
  isolate({
    NA.count<- length(which(is.na(Biobase::exprs(rv$current.obj))))
    if (NA.count > 0){
      tags$p("Your dataset contains missing values. Before using the differential analysis, you must filter/impute them")
    } else {
      tagList(
        
        tags$div(
          tags$div( style="display:inline-block; vertical-align: middle;padding-right: 20px;",
                    selectInput("HypoTestPept_anaDiff_Design", "Contrast", 
                                choices=c("None"="None", "One vs One"="OnevsOne", "One vs All"="OnevsAll"),
                                selected=rv$widgets$HypothesisTestPeptide$design,
                                width='150px')
          ),
          tags$div( style="display:inline-block; vertical-align: middle;padding-right: 20px;",
                    selectInput("HypoTestPept_diffAnaMethod","Statistical test",
                                choices = c("None"="None", "groupttests"="groupttests", "PEPA"="groupttests"),
                                selected=rv$widgets$HypothesisTestPeptide$method,
                                width='150px')
          ),
          tags$div( style="display:inline-block; vertical-align: middle; padding-right: 20px;",
                    hidden( radioButtons("HypoTestPept_ttest_options", "t-tests options",choices=c("Student", "Welch"),
                                         selected=rv$widgets$HypothesisTestPeptide$ttest_options,
                                         width='150px'))
          ),
          tags$div( style="display:inline-block; vertical-align: middle; padding-right: 20px;",
                    textInput("HypoTestPept_seuilLogFC", "log(FC) threshold",  
                              value="0",
                              width='150px'),
                    module_Not_a_numericUI("HypoTestPept_test_seuillogFC")
          ),
          tags$div( style="display:inline-block; vertical-align: middle; padding-right: 20px;",
                    uiOutput("HypoTestPept_correspondingRatio")
                    
          ),
          tags$div( style="display:inline-block; vertical-align: middle; padding-right: 20px;",
                    actionButton("HypoTestPept_PerformLogFCPlot", "Perform log FC plot",class = actionBtnClass )
                    
          )
        )
        ,
        tags$hr(),
        highchartOutput("HypoTestPept_FoldChangePlot", height="100%")
      )
      
    }
  })
})


output$screenHypoTestPeptide2 <- renderUI({
  tagList(
    uiOutput("HypoTestPept_btn_valid")
  )
})

output$HypoTestPept_correspondingRatio <- renderUI({
  ratio <- as.numeric(rv$widgets$HypothesisTestPeptide$th_logFC)
  p("(FC = ", 2^(ratio), ")")
})



output$HypoTestPept_btn_valid <- renderUI({
  cond <- (input$HypoTestPept_diffAnaMethod != "None")&&(input$HypoTestPept_anaDiff_Design != "None")
  if (!cond){return(NULL)}
  actionButton("HypoTestPept_ValidTest","Save significance test", class = actionBtnClass)
})


observeEvent(rv$widgets$HypothesisTestPeptide$method,{
  
  toggle(id = "HypoTestPept_ttest_options",  condition = (rv$widgets$HypothesisTestPeptide$method == "groupttests"))
})



output$HypoTestPept_FoldChangePlot <- renderHighchart({
  req(ComputeComparisonsPeptide()$logFC)
  req(rv$PlotParams$paletteConditions)
  req(rv$widgets$HypothesisTestProtein$th_logFC)
  data <- ComputeComparisonsPeptide()
  if (length(data$logFC)==0){return(NULL)}
  withProgress(message = 'Computing plot...',detail = '', value = 0.5, {
    rv$tempplot$HypoTestPept_logFCDistr <- hc_logFC_DensityPlot(data$logFC,as.numeric(rv$widgets$HypothesisTestPeptide$th_logFC))
  })
 
})



########################################################

### calcul des comparaisons                         ####
########################################################
ComputeComparisonsPeptide <- reactive({
  req(rv$widgets$HypothesisTestPeptide$method)
  req(rv$widgets$HypothesisTestPeptide$design)
  rv$widgets$HypothesisTestPeptide$ttest_options
  if ((rv$widgets$HypothesisTestPeptide$method=="None")|| (rv$widgets$HypothesisTestPeptide$design=="None")) {return (NULL)}
  if (length(which(is.na(Biobase::exprs(rv$current.obj)))) > 0) { return()}
  
  rv$res_AllPairwiseComparisons <- NULL
  #isolate({
    #if (is.null(rv$current.obj@experimentData@other$Params[["HypothesisTestPeptide"]])){
  withProgress(message = 'Computing comparisons ...',detail = '', value = 0.5, {
    switch(rv$widgets$HypothesisTestPeptide$method,
           PEPA={
             rv$res_AllPairwiseComparisons <- NULL
             
           },
           groupttests={
             print("IN group t tests")
               qData <- Biobase::exprs(rv$current.obj)
               sTab <- Biobase::pData(rv$current.obj)
               X.spec <- rv$matAdj$matWithUniquePeptides
               
               print(head(qData))
               print(sTab)
               print(dim(X.spec))
               print(rv$widgets$HypothesisTestPeptide$design)
               print(rv$widgets$HypothesisTestPeptide$ttest_options)
               
               
             rv$res_AllPairwiseComparisons <- compute.group.t.tests(qData, sTab, X.spec, 
                                                                      contrast=rv$widgets$HypothesisTestPeptide$design,
                                                                      type=rv$widgets$HypothesisTestPeptide$ttest_options)
           })
    rv$widgets$HypothesisTestPeptide$listNomsComparaison <- colnames(rv$res_AllPairwiseComparisons$logFC)
    
    
    rvModProcess$moduleHypothesisTestPeptideDone[1] <- TRUE
    rv$res_AllPairwiseComparisons
  })
})




########################################################################
#
#
########################################################################
observeEvent(input$HypoTestProt_ValidTest,{ 
  # req(rv$current.obj)
  req(rv$res_AllPairwiseComparisons)
  
  if (length(which(is.na(Biobase::exprs(rv$current.obj)))) > 0) { return()}
  
  ### Save RAW data
  isolate({
    
    #.protData <- Biobase::exprs(rv$current.obj)
    nrow <- length(rv$res_AllPairwiseComparisons[[1]][[1]])
    m <- dim(Biobase::exprs(rv$current.obj))[2]
    .protData <- matrix(rep(1,nrow*m), ncol=m)
    colnames(.protData) <- colnames(Biobase::exprs(rv$current.obj))
    test <- finalizeAggregation(obj.pep = rv$current.obj,
                                pepData = Biobase::exprs(rv$current.obj),
                                protData = .protData,
                                X = rv$matAdj$matWithUniquePeptides
                                )
    
    rv$current.obj <- DAPAR::diffAnaSave(obj = rv$current.obj,allComp = rv$res_AllPairwiseComparisons)
    
    
    name <- paste("HypothesisTestPeptide.", rv$typeOfDataset, sep="")
    rv$current.obj <- saveParameters(rv$current.obj, name,"HypothesisTestPeptide", build_ParamsList_HypothesisTestPeptide())
    BuildNavbarPage()
    rvModProcess$moduleHypothesisTestPeptideDone[2] <- TRUE
    UpdateDatasetWidget(rv$current.obj, name)
  })
})