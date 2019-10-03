
callModule(module_Not_a_numeric,"test_seuillogFCPeptide", reactive({input$seuilLogFCPeptide}))


observeEvent(input$seuilLogFCPeptide,{  rv$widgets$HypothesisTestPeptide$th_logFC<- as.numeric(input$seuilLogFCPeptide)})



callModule(moduleProcess, "moduleProcess_HypothesisTestPeptide", 
           isDone = reactive({rvModProcess$moduleHypothesisTestPeptideDone}), 
           pages = reactive({rvModProcess$moduleHypothesisTestPeptide}),
           rstFunc = resetModuleHypothesisTestPeptide)


resetModuleHypothesisTestPeptide <- reactive({  
  ## update widgets values (reactive values)
  resetModuleProcess("HypothesisTestPeptide")
  
  ## update widgets in UI
  #updateSelectInput(session,"anaDiff_DesignPeptide", selected = rv$widgets$hypothesisTestPeptide$design)
  #updateSelectInput(session,"diffAnaMethodPeptide", selected = rv$widgets$hypothesisTestPeptide$method)
  #updateRadioButtons(session,"ttest_optionsPeptide", selected = rv$widgets$hypothesisTestPeptide$ttest_options)
  #updateTextInput(session, "seuilLogFCPeptide", value= rv$widgets$hypothesisTestPeptide$th_logFC)
  
  rvModProcess$moduleHypothesisTestPeptideDone = rep(FALSE, 2)
  ##update dataset to put the previous one
  rv$current.obj <- rv$dataset[[last(names(rv$dataset))]] 
  
})



observeEvent(input$ttest_optionsPeptide,{rv$widgets$HypothesisTestPeptide$ttest_options <- input$ttest_optionsPeptide})

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
                    selectInput("anaDiff_DesignPeptide", "Contrast", 
                                choices=c("None"="None", "One vs One"="OnevsOne", "One vs All"="OnevsAll"),
                                selected=rv$widgets$HypothesisTestPeptide$design,
                                width='150px')
          ),
          tags$div( style="display:inline-block; vertical-align: middle;padding-right: 20px;",
                    selectInput("diffAnaMethodPeptide","Statistical test",
                                choices = c("None"="None", "groupttests"="groupttests", "PEPA"="groupttests"),
                                selected=rv$widgets$HypothesisTestPeptide$method,
                                width='150px')
          ),
          tags$div( style="display:inline-block; vertical-align: middle; padding-right: 20px;",
                    hidden( radioButtons("ttest_optionsPeptide", "t-tests options",choices=c("Student", "Welch"),
                                         selected=rv$widgets$HypothesisTestPeptide$ttest_options,
                                         width='150px'))
          ),
          tags$div( style="display:inline-block; vertical-align: middle; padding-right: 20px;",
                    textInput("seuilLogFCPeptide", "log(FC) threshold",  
                              value="0",
                              width='150px'),
                    module_Not_a_numericUI("test_seuillogFCPeptide")
          ),
          tags$div( style="display:inline-block; vertical-align: middle; padding-right: 20px;",
                    uiOutput("correspondingRatioPeptide")
                    
          )
        )
        ,
        tags$hr(),
        highchartOutput("FoldChangePlotPeptide", height="100%") %>% withSpinner(type=spinnerType)
      )
      
    }
  })
})


output$screenHypoTestPeptide2 <- renderUI({
  tagList(
    uiOutput("btn_validPeptide")
  )
})



output$btn_validPeptide <- renderUI({
  cond <- (input$diffAnaMethodPeptide != "None")&&(input$anaDiff_DesignPeptide != "None")
  if (!cond){return(NULL)}
  actionButton("ValidTestPeptide","Save significance test", class = actionBtnClass)
})


observeEvent(input$diffAnaMethodPeptide,{
  
  toggle(id = "ttest_optionsPeptide",  condition = (input$diffAnaMethodPeptide == "groupttests"))
})



output$FoldChangePlotPeptide <- renderHighchart({
  #req(rv$res_AllPairwiseComparisons)
  rv$PlotParams$paletteConditions
  
  data <- ComputeComparisonsPeptide()
  print(str(data))
  
  rv$tempplot$logFCDistrPeptide <- hc_logFC_DensityPlot(data$logFC,as.numeric(input$seuilLogFCPeptide))
  rv$tempplot$logFCDistrPeptide
})



output$correspondingRatioPeptide <- renderUI({
  
  ratio <- as.numeric(input$seuilLogFCPeptide)
  
  p("(FC = ", 2^(ratio), ")")
  
})



########################################################

### calcul des comparaisons                         ####
########################################################
ComputeComparisonsPeptide <- reactive({
  req(input$diffAnaMethodPeptide)
  req(input$anaDiff_DesignPeptide)
  input$ttest_optionsPeptide
  if ((input$diffAnaMethodPeptide=="None")|| (input$anaDiff_DesignPeptide=="None")) {return (NULL)}
  if (length(which(is.na(Biobase::exprs(rv$current.obj)))) > 0) { return()}
  
  isolate({
    #if (is.null(rv$current.obj@experimentData@other$Params[["HypothesisTestPeptide"]])){
    switch(input$diffAnaMethodPeptide,
           PEPA={
             rv$res_AllPairwiseComparisons <- NULL
             
           },
           groupttests={
               qData <- Biobase::exprs(rv$current.obj)
               sTab <- Biobase::pData(rv$current.obj)
               X.spec <- rv$matAdj$matWithUniquePeptides
               
               print(head(qData))
               print(sTab)
               print(dim(X.spec))
               print(input$anaDiff_DesignPeptide)
               print(input$ttest_optionsPeptide)
               
               
             rv$res_AllPairwiseComparisons <- compute.group.t.tests(qData, sTab, X.spec, 
                                                                      contrast=input$anaDiff_DesignPeptide,
                                                                      type=input$ttest_optionsPeptide)
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
observeEvent(input$ValidTestPeptide,{ 
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
    
    rv$current.obj <- DAPAR::diffAnaSave(obj = rv$current.obj,
                                         allComp = rv$res_AllPairwiseComparisons)
    
    
    name <- paste("HypothesisTestPeptide.", rv$typeOfDataset, sep="")
    rv$current.obj <- saveParameters(rv$current.obj, name,"HypothesisTestPeptide", build_ParamsList_HypothesisTestPeptide())
    
    rv$dataset[[name]] <- rv$current.obj
    rvModProcess$moduleHypothesisTestPeptideDone[2] <- TRUE
    
    updateSelectInput(session, "datasets", choices = names(rv$dataset), selected = name)
    BuildNavbarPage()
  })
  
})