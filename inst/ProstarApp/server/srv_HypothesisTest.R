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
                              choices=c("None"="", "One vs One"="OnevsOne", "One vs All"="OnevsAll"),
                              width='150px')
        ),
        tags$div( style="display:inline-block; vertical-align: middle;padding-right: 20px;",
                  selectInput("diffAnaMethod","Statistical test",choices = anaDiffMethod_Choices,
                              width='150px')
        ),
        tags$div( style="display:inline-block; vertical-align: middle; padding-right: 20px;",
                  hidden( radioButtons("ttest_options", "t-tests options",choices=c("Student", "Welch"),
                                       width='150px'))
        ),
        tags$div( style="display:inline-block; vertical-align: middle;",
                  numericInput("seuilLogFC", "log(FC) threshold", min=0, step=0.1, value=0,
                               width='150px')
        ),
        tags$div( style="display:inline-block; vertical-align: middle;",
                  hidden(actionButton("ValidTest","Save significance test"))
        )
      ),
      tags$hr(),
      highchartOutput("FoldChangePlot", height="100%") %>% withSpinner(type=spinnerType)
    )
    
  }
})

observe({
  shinyjs::toggle("ValidTest", condition=((input$anaDiff_Design != "") && (input$diffAnaMethod != "")))
})

observeEvent(input$seuilLogFC,{
  rv$seuilLogFC <- as.numeric(input$seuilLogFC)
})

observeEvent(input$diffAnaMethod,{
  
  toggle(id = "ttest_options",  condition = (input$diffAnaMethod == "ttests"))
})



output$FoldChangePlot <- renderHighchart({
  req(rv$res_AllPairwiseComparisons)
  req(input$seuilLogFC)
  req(input$diffAnaMethod)
  req(input$anaDiff_Design)
  rv$PlotParams$paletteConditions
  
  
  if ((input$diffAnaMethod=="None") || (input$anaDiff_Design=="None")){return(NULL)}
  data <- rv$res_AllPairwiseComparisons
  rv$tempplot$logFCDistr <- hc_logFC_DensityPlot(data$logFC,input$seuilLogFC)
  rv$tempplot$logFCDistr
})



########################################################

### calcul des comparaisons              ####
#######################################################
observe({
  req(input$diffAnaMethod)
  req(input$anaDiff_Design)
  input$ttest_options
  
  
  if ((input$diffAnaMethod=="None")) {return ()}
  if ((input$anaDiff_Design=="None")) {return ()}
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
    rv$listNomsComparaison <- colnames(rv$res_AllPairwiseComparisons$logFC)

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
l.params <- list(design = input$anaDiff_Design,
                   method = input$diffAnaMethod,
                   ttest_options = input$ttest_options,
                   th_logFC = as.numeric(input$seuilLogFC),
                   AllPairwiseCompNames = list(logFC = colnames(rv$res_AllPairwiseComparisons$logFC), 
                                               P_Value=colnames(rv$res_AllPairwiseComparisons$P_Value))
  )
  
  
  temp <- DAPAR::diffAnaSave(obj = rv$current.obj,
                             allComp = rv$res_AllPairwiseComparisons)
  
  rv$current.obj <- saveParameters(rv$current.obj, "HypothesisTest", l.params)
  
  name <- paste("HypothesisTest.", rv$typeOfDataset, sep="")
  
  rv$dataset[[name]] <- rv$current.obj
 # rv$current.obj <- temp
  UpdateLog("Test", l.params)
  
  updateSelectInput(session, "datasets", 
                    choices = names(rv$dataset),
                    selected = name)
  
  
  updateSelectInput(session,"anaDiff_Design", selected=input$anaDiff_Design)
  updateSelectInput(session,"diffAnaMethod", selected=input$diffAnaMethod )
  updateRadioButtons(session, "ttest_options", selected=input$ttest_options)
  updateNumericInput(session, "seuilLogFC", value=input$seuilLogFC)
  
})