
observeEvent(input$anaDiff_Design, { rv$widgets$hypothesisTest$design <- input$anaDiff_Design})
observeEvent(input$diffAnaMethod, { rv$widgets$hypothesisTest$method <- input$diffAnaMethod})
observeEvent(input$ttest_options, { rv$widgets$hypothesisTest$ttest_options <- input$ttest_options})
observeEvent(input$seuilLogFC, { rv$widgets$hypothesisTest$th_logFC <- as.numeric(input$seuilLogFC)})


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
                                       selected=rv$widgets$hypothesisTest$ttest_options,
                                       width='150px'))
        ),
        tags$div( style="display:inline-block; vertical-align: middle;",
                  textInput("seuilLogFC", "log(FC) threshold",  
                               value=rv$widgets$hypothesisTest$th_logFC,
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


observeEvent(rv$widgets$hypothesisTest$method,{
  
  toggle(id = "ttest_options",  condition = (rv$widgets$hypothesisTest$method == "ttests"))
})



output$FoldChangePlot <- renderHighchart({
  req(rv$res_AllPairwiseComparisons)
  rv$PlotParams$paletteConditions
  
  
  #if ((rv$widgets$hypothesisTest$design=="None") || (rv$widgets$hypothesisTest$design=="None")){return(NULL)}
  data <- rv$res_AllPairwiseComparisons
  rv$tempplot$logFCDistr <- hc_logFC_DensityPlot(data$logFC,rv$widgets$hypothesisTest$th_logFC)
  rv$tempplot$logFCDistr
})



########################################################

### calcul des comparaisons              ####
#######################################################
observeEvent(rv$widgets$hypothesisTest,{
 # req(rv$widgets$hypothesisTest$method)
 # req(rv$widgets$hypothesisTest$design)
  #rv$widgets$hypothesisTest$ttest_options
  
  
  if (rv$widgets$hypothesisTest$method=="None") {return ()}
  if (rv$widgets$hypothesisTest$design=="None") {return ()}
  if (length(which(is.na(Biobase::exprs(rv$current.obj)))) > 0) { return()}
  

  #if (is.null(rv$current.obj@experimentData@other$Params[["HypothesisTest"]])){
    switch(rv$widgets$hypothesisTest$method,
           Limma={
             rv$res_AllPairwiseComparisons <- limmaCompleteTest(Biobase::exprs(rv$current.obj), 
                                                                Biobase::pData(rv$current.obj),
                                                                rv$widgets$hypothesisTest$design) 
             
           },
           ttests={
             rv$res_AllPairwiseComparisons <- wrapper.t_test_Complete(rv$current.obj, 
                                                                      Contrast=rv$widgets$hypothesisTest$design,
                                                                      type=rv$widgets$hypothesisTest$ttest_options)
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
l.params <- rv$widgets$hypothesisTest
  
  
  
rv$current.obj <- DAPAR::diffAnaSave(obj = rv$current.obj,
                             allComp = rv$res_AllPairwiseComparisons)
  
  
  name <- paste("HypothesisTest.", rv$typeOfDataset, sep="")
  rv$current.obj <- saveParameters(rv$current.obj, name,"HypothesisTest", l.params)
  
  rv$dataset[[name]] <- rv$current.obj
  
  
  updateSelectInput(session, "datasets", choices = names(rv$dataset), selected = name)
  
  
  updateSelectInput(session,"anaDiff_Design", selected=rv$widgets$hypothesisTest$design)
  updateSelectInput(session,"diffAnaMethod", selected=rv$widgets$hypothesisTest$method )
  updateRadioButtons(session, "ttest_options", selected=rv$widgets$hypothesisTest$ttest_options)
  updateNumericInput(session, "seuilLogFC", value=rv$widgets$hypothesisTest$th_logFC)
  
})