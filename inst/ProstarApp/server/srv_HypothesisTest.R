
observeEvent(input$seuilLogFC,{  rv$widgets$hypothesisTest$th_logFC<- input$seuilLogFC})


output$testPanel <- renderUI({
  req(rv$current.obj)
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
                                       selected=input$ttest_options,
                                       width='150px'))
        ),
        tags$div( style="display:inline-block; vertical-align: middle;",
                  textInput("seuilLogFC", "log(FC) threshold",  
                               value=as.numeric(input$seuilLogFC),
                               width='150px')
        ),
        tags$div( style="display:inline-block; vertical-align: middle;",
                  (actionButton("ValidTest","Save significance test", class = actionBtnClass))
        )
      ),
      tags$hr(),
      highchartOutput("FoldChangePlot", height="100%") %>% withSpinner(type=spinnerType)
    )
    
  }
})

observeEvent(c(input$diffAnaMethod, input$anaDiff_Design),{
  shinyjs::toggle("ValidTest", condition=(sum(c(input$diffAnaMethod,input$anaDiff_Design) != "None")==2 ))
})


observeEvent(input$diffAnaMethod,{
  
  toggle(id = "ttest_options",  condition = (input$diffAnaMethod == "ttests"))
})



output$FoldChangePlot <- renderHighchart({
  req(rv$res_AllPairwiseComparisons)
  rv$PlotParams$paletteConditions
  
  data <- rv$res_AllPairwiseComparisons
  rv$tempplot$logFCDistr <- hc_logFC_DensityPlot(data$logFC,as.numeric(input$seuilLogFC))
  rv$tempplot$logFCDistr
})



########################################################

### calcul des comparaisons              ####
#######################################################
observe({
  req(input$diffAnaMethod)
  req(input$anaDiff_Design)
  input$ttest_options
  
  if (input$diffAnaMethod=="None") {return ()}
  if (input$anaDiff_Design=="None") {return ()}
  if (length(which(is.na(Biobase::exprs(rv$current.obj)))) > 0) { return()}
  

  #if (is.null(rv$current.obj@experimentData@other$Params[["HypothesisTest"]])){
    switch(input$diffAnaMethod,
           Limma={
             rv$res_AllPairwiseComparisons <- limmaCompleteTest(Biobase::exprs(rv$current.obj), 
                                                                Biobase::pData(rv$current.obj),
                                                                input$anaDiff_Design) 
             
           },
           ttests={
             rv$res_AllPairwiseComparisons <- wrapper.t_test_Complete(rv$current.obj, 
                                                                      Contrast=input$anaDiff_Design,
                                                                      type=input$ttest_options)
           })
  rv$widgets$hypothesisTest$listNomsComparaison <- colnames(rv$res_AllPairwiseComparisons$logFC)
    

})




########################################################################
#
#
########################################################################
observeEvent(input$ValidTest,{ 
  req(rv$current.obj)
  req(rv$res_AllPairwiseComparisons)
  
  if (length(which(is.na(Biobase::exprs(rv$current.obj)))) > 0) { return()}
  
  ### Save RAW data

rv$current.obj <- DAPAR::diffAnaSave(obj = rv$current.obj,
                             allComp = rv$res_AllPairwiseComparisons)
  
  
  name <- paste("HypothesisTest.", rv$typeOfDataset, sep="")
  rv$current.obj <- saveParameters(rv$current.obj, name,"HypothesisTest", build_ParamsList_HypothesisTest())
  
  rv$dataset[[name]] <- rv$current.obj
  
  
  updateSelectInput(session, "datasets", choices = names(rv$dataset), selected = name)
  
  
  updateSelectInput(session,"anaDiff_Design", selected=input$anaDiff_Design)
  updateSelectInput(session,"diffAnaMethod", selected=input$diffAnaMethod )
  updateRadioButtons(session, "ttest_options", selected=input$ttest_options)
  updateNumericInput(session, "seuilLogFC", value=as.numeric(input$seuilLogFC))
  
})