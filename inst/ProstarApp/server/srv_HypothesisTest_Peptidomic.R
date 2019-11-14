

callModule(module_Not_a_numeric,"test_seuillogFCPeptidomic", reactive({input$seuilLogFCPeptidomic}))


observeEvent(input$seuilLogFCPeptidomic,{  rv$widgets$hypothesisTest$th_logFC<- as.numeric(input$seuilLogFCPeptidomic)})



callModule(moduleProcess, "moduleProcess_HypothesisTestPeptidomic", 
           isDone = reactive({rvModProcess$moduleHypothesisTestPeptidomicDone}), 
           pages = reactive({rvModProcess$moduleHypothesisTestPeptidomic}),
           rstFunc = resetModuleHypothesisTestPeptidomic,
           forceReset = reactive({rvModProcess$moduleHypothesisTestPeptidomicForceReset })  )



resetModuleHypothesisTestPeptidomic <- reactive({  
  ## update widgets values (reactive values)
  resetModuleProcess("HypothesisTest")
  
  ## update widgets in UI
  updateSelectInput(session,"anaDiff_DesignPeptidomic", selected = rv$widgets$hypothesisTest$design)
  updateSelectInput(session,"diffAnaMethodPeptidomic", selected = rv$widgets$hypothesisTest$method)
  updateRadioButtons(session,"ttest_optionsPeptidomic", selected = rv$widgets$hypothesisTest$ttest_options)
  updateTextInput(session, "seuilLogFCPeptidomic", value= rv$widgets$hypothesisTest$th_logFC)
  
  rvModProcess$moduleHypothesisTestPeptidomicDone = rep(FALSE, 2)
  ##update dataset to put the previous one
  rv$current.obj <- rv$dataset[[last(names(rv$dataset))]] 
  
})



observeEvent(input$ttest_optionsPeptidomic,{rv$widgets$hypothesisTest$ttest_options <- input$ttest_optionsPeptidomic})

output$screenHypoTestPeptidomic1 <- renderUI({
  
  isolate({
    NA.count<- length(which(is.na(Biobase::exprs(rv$current.obj))))
    if (NA.count > 0){
      tags$p("Your dataset contains missing values. Before using the differential analysis, you must filter/impute them")
    } else {
      tagList(
        
        tags$div(
          tags$div( style="display:inline-block; vertical-align: middle;padding-right: 20px;",
                    selectInput("anaDiff_DesignPeptidomic", "Contrast", 
                                choices=c("None"="None", "One vs One"="OnevsOne", "One vs All"="OnevsAll"),
                                selected=rv$widgets$hypothesisTest$design,
                                width='150px')
          ),
          tags$div( style="display:inline-block; vertical-align: middle;padding-right: 20px;",
                    selectInput("diffAnaMethodPeptidomic","Statistical test",
                                choices = anaDiffMethod_Choices,
                                selected=rv$widgets$hypothesisTest$method,
                                width='150px')
          ),
          tags$div( style="display:inline-block; vertical-align: middle; padding-right: 20px;",
                    hidden( radioButtons("ttest_optionsPeptidomic", "t-tests options",choices=c("Student", "Welch"),
                                         selected=rv$widgets$hypothesisTest$ttest_options,
                                         width='150px'))
          ),
          tags$div( style="display:inline-block; vertical-align: middle; padding-right: 20px;",
                    textInput("seuilLogFCPeptidomic", "log(FC) threshold",  
                              value="0",
                              width='150px'),
                    module_Not_a_numericUI("test_seuillogFCPeptidomic")
          ),
          tags$div( style="display:inline-block; vertical-align: middle; padding-right: 20px;",
                    uiOutput("correspondingRatioPeptidomic")
                    
          )
        )
        ,
        tags$hr(),
        highchartOutput("FoldChangePlotPeptidomic", height="100%") %>% withSpinner(type=spinnerType)
      )
      
    }
  })
})


output$screenHypoTestPeptidomic2 <- renderUI({
  tagList(
    uiOutput("btn_validPeptidomic")
  )
})



output$btn_validPeptidomic <- renderUI({
  cond <- (input$diffAnaMethodPeptidomic != "None")&&(input$anaDiff_DesignPeptidomic != "None")
  if (!cond){return(NULL)}
  actionButton("ValidTestPeptidomic","Save significance test", class = actionBtnClass)
})


observeEvent(input$diffAnaMethodPeptidomic,{
  
  toggle(id = "ttest_optionsPeptidomic",  condition = (input$diffAnaMethodPeptidomic == "ttests"))
})



output$FoldChangePlotPeptidomic <- renderHighchart({
  #req(rv$res_AllPairwiseComparisons)
  rv$PlotParams$paletteConditions
  
  data <- ComputeComparisonsPeptidomic()
  print(str(data))
  print(as.numeric(input$seuilLogFCPeptidomic))
  rv$tempplot$logFCDistr <- hc_logFC_DensityPlot(data$logFC,as.numeric(input$seuilLogFCPeptidomic))
  rv$tempplot$logFCDistr
})



output$correspondingRatioPeptidomic <- renderUI({
  
  ratio <- as.numeric(input$seuilLogFCPeptidomic)
  
  p("(FC = ", 2^(ratio), ")")
  
})



########################################################

### calcul des comparaisons                         ####
########################################################
ComputeComparisonsPeptidomic <- reactive({
  req(input$diffAnaMethodPeptidomic)
  req(input$anaDiff_DesignPeptidomic)
  input$ttest_optionsPeptidomic
  if ((input$diffAnaMethodPeptidomic=="None")|| (input$anaDiff_DesignPeptidomic=="None")) {return (NULL)}
  if (length(which(is.na(Biobase::exprs(rv$current.obj)))) > 0) { return()}
  
  isolate({
    #if (is.null(rv$current.obj@experimentData@other$Params[["HypothesisTest"]])){
    switch(input$diffAnaMethodPeptidomic,
           Limma={
             rv$res_AllPairwiseComparisons <- limmaCompleteTest(Biobase::exprs(rv$current.obj), 
                                                                Biobase::pData(rv$current.obj),
                                                                input$anaDiff_DesignPeptidomic) 
             
           },
           ttests={
             rv$res_AllPairwiseComparisons <- wrapper.t_test_Complete(rv$current.obj, 
                                                                      contrast=input$anaDiff_DesignPeptidomic,
                                                                      type=input$ttest_optionsPeptidomic)
           })
    rv$widgets$hypothesisTest$listNomsComparaison <- colnames(rv$res_AllPairwiseComparisons$logFC)
    
    
    rvModProcess$moduleHypothesisTestPeptidomicDone[1] <- TRUE
    rv$res_AllPairwiseComparisons
  })
})




########################################################################
#
#
########################################################################
observeEvent(input$ValidTestPeptidomic,{ 
  # req(rv$current.obj)
  req(rv$res_AllPairwiseComparisons)
  
  if (length(which(is.na(Biobase::exprs(rv$current.obj)))) > 0) { return()}
  
  ### Save RAW data
  isolate({
    rv$current.obj <- DAPAR::diffAnaSave(obj = rv$current.obj,
                                         allComp = rv$res_AllPairwiseComparisons)
    
    
    name <- paste("HypothesisTest.", rv$typeOfDataset, sep="")
    rv$current.obj <- saveParameters(rv$current.obj, name,"HypothesisTest", build_ParamsList_HypothesisTest())
    
    rv$dataset[[name]] <- rv$current.obj
    rvModProcess$moduleHypothesisTestPeptidomicDone[2] <- TRUE
    
    updateSelectInput(session, "datasets", choices = names(rv$dataset), selected = name)
    BuildNavbarPage()
  })
  
})

