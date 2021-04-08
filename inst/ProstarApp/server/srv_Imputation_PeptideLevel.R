require(imp4p)

callModule(moduleMVPlots,"mvImputationPlots_PeptideLevel", 
           data=reactive(rv$current.obj),
           title = reactive("POV distribution"),
           palette =reactive(unique(rv$PlotParams$paletteConditions)))


callModule(moduleDetQuantImpValues, "peptide_DetQuantValues_DT", 
           reactive({rv$widgets$peptideImput$pepLevel_detQuantile}), 
           reactive({rv$widgets$peptideImput$pepLevel_detQuant_factor}))


callModule(modulePopover,"modulePopover_HelpImputationPeptide", 
           data = reactive(list(title = HTML("<strong>Algorithm</strong>"),
                                content= HTML(paste0("<ul><li><strong>imp4p [Ref. 7]</strong> a proteomic-specific multiple imputation method that operates on peptide-level datasets and which proposes to impute each missing value according to its nature (left-censored  or random). To tune the number of iterations, let us keep in mind that, the more iterations, the more accurate the results, yet the more time-consuming the computation.</li> <li><strong>Dummy censored:</strong> each missing value is supposed to be a censored value and is replaced by the XXX quantile of the corresponding sample abundance distribution <ul><li><strong>KNN </strong>see [Other ref. 2].</li><li><strong>MLE </strong>Imputation with the maximum likelihood estimate of the expected intensity (see the norm R package).</li></ul></ul>")))))


callModule(moduleProcess, "moduleProcess_PepImputation", 
           isDone = reactive({rvModProcess$modulePepImputationDone}), 
           pages = reactive({rvModProcess$modulePepImputation}),
           rstFunc = resetModulePepImputation,
           forceReset = reactive({rvModProcess$modulePepImputationForceReset })  )


resetModulePepImputation <- reactive({  
  ## update widgets values (reactive values)
  resetModuleProcess("PepImputation")
  
  rv$widgets$peptideImput$pepLevel_algorithm <- "None"
  rv$widgets$peptideImput$pepLevel_basicAlgorithm <- "None"
  rv$widgets$peptideImput$pepLevel_detQuantile <- 2.5
  rv$widgets$peptideImput$pepLevel_detQuant_factor <- 1
  rv$widgets$peptideImput$pepLevel_imp4p_nbiter <- 10
  rv$widgets$peptideImput$pepLevel_imp4p_withLapala <- FALSE
  rv$widgets$peptideImput$pepLevel_imp4p_qmin <- 2.5
  rv$widgets$peptideImput$pepLevel_imp4pLAPALA_distrib <- "beta"
  rv$widgets$peptideImput$pepLevel_KNN_n <- 10
  
  rvModProcess$modulePepImputationDone = rep(FALSE, 2)
  rv$current.obj <- rv$dataset[[input$datasets]] 
})



observeEvent(input$peptideLevel_missing.value.algorithm,{
  rv$widgets$peptideImput$pepLevel_algorithm <- input$peptideLevel_missing.value.algorithm})

observeEvent(input$peptideLevel_missing.value.basic.algorithm,{
  rv$widgets$peptideImput$pepLevel_basicAlgorithm <- input$peptideLevel_missing.value.basic.algorithm})

observeEvent(input$peptideLevel_detQuant_quantile,{
  rv$widgets$peptideImput$pepLevel_detQuantile <- input$peptideLevel_detQuant_quantile})

observeEvent(input$peptideLevel_detQuant_factor,{
  rv$widgets$peptideImput$pepLevel_detQuant_factor <- input$peptideLevel_detQuant_factor})

observeEvent(input$KNN_n,{
  rv$widgets$peptideImput$pepLevel_KNN_n <- input$KNN_n})

observeEvent(input$peptideLevel_imp4p_nbiter,{
  rv$widgets$peptideImput$pepLevel_imp4p_nbiter <- input$peptideLevel_imp4p_nbiter})


observeEvent(input$peptideLevel_imp4p_withLapala,{
  rv$widgets$peptideImput$pepLevel_imp4p_withLapala <- input$peptideLevel_imp4p_withLapala})

observeEvent(input$peptideLevel_imp4p_qmin,{
  rv$widgets$peptideImput$pepLevel_imp4p_qmin <- input$peptideLevel_imp4p_qmin})

observeEvent(input$peptideLevel_imp4pLAPALA_distrib,{
  rv$widgets$peptideImput$pepLevel_imp4pLAPALA_distrib <- input$peptideLevel_imp4pLAPALA_distrib})



##########
#####  UI for the PEPTIDE LEVEL Imputation process
##########
output$screenPepImputation1 <- renderUI({
  #req(rv$current.obj)
  # isolate({
  nbEmptyLines <- getNumberOfEmptyLines(Biobase::exprs(rv$current.obj))
  
  if (nbEmptyLines > 0) {
    tags$p("Your dataset contains empty lines (fully filled with missing values). In order to use
             the imputation tool, you must delete them by using the filter tool.")
    
  }
  else { 
    tabPanel("Miss. values imputation",
             id = "tabPanelImputation",
             value = "imputation",
             tags$div(
               tags$div( style="display:inline-block; vertical-align: top; padding-right: 20px;",
                         modulePopoverUI("modulePopover_HelpImputationPeptide"),
                         selectInput("peptideLevel_missing.value.algorithm",
                                     NULL,
                                     choices = imputationAlgorithms, 
                                     selected = rv$widgets$peptideImput$pepLevel_algorithm,
                                     width='150px')
               ),
               
               tags$div( style="display:inline-block; vertical-align: top; padding-right: 20px;",
                         uiOutput("basicAlgoUI")),
               tags$div( style="display:inline-block; vertical-align: top; padding-right: 20px;",
                         uiOutput("detQuantOptsUI"),
                         uiOutput("KNNOptsUI"),
                         uiOutput("imp4pOptsUI")),
               tags$div( style="display:inline-block; vertical-align: top; padding-right: 20px;",
                         uiOutput("imp4pOpts2UI")),
               tags$div( style="display:inline-block; vertical-align: top; padding-right: 20px;",
                         uiOutput("peptideLevel_detQuant_impValues"))
               
             ),
             tags$div(
               tags$div( style="display:inline-block; vertical-align: top; padding-right: 20px;",
                         actionButton("peptideLevel_perform.imputation.button", "Perform imputation", class = actionBtnClass))
             ),
             br(), br(), br(),
             uiOutput("warningImputationMethod"),
             
             ## progress bar
             #br(),
             #br(),
             #uiOutput(outputId = "progressOne")
             tagList(
               tags$hr(),
               withProgress(message = '',detail = '', value = 0, {
                 incProgress(0.5, detail = 'Aggregation in progress')
                 moduleMVPlotsUI("mvImputationPlots_PeptideLevel")
               })
             )      
             
    )
    
  }
  #})
})



output$screenPepImputation2 <- renderUI({
  
  tagList(
    actionButton("peptideLevel_ValidImputation", "Save imputation", class = actionBtnClass))
})



output$basicAlgoUI <- renderUI({
  if (rv$widgets$peptideImput$pepLevel_algorithm != "BasicMethods"){return(NULL)}
  
  selectInput("peptideLevel_missing.value.basic.algorithm", 
              "Methods", width='150px',
              choices = basicMethodsImputationAlgos,
              selected = rv$widgets$peptideImput$pepLevel_basicAlgorithm)
  
})


output$detQuantOptsUI <- renderUI({
  req(rv$widgets$peptideImput$pepLevel_basicAlgorithm)
  req(rv$widgets$peptideImput$pepLevel_algorithm)
  if ((rv$widgets$peptideImput$pepLevel_basicAlgorithm != "detQuantile") || 
      (rv$widgets$peptideImput$pepLevel_algorithm != "BasicMethods")){return(NULL)}
  
  tagList(
    tags$div( style="display:inline-block; vertical-align: top; padding-right: 20px;",
              numericInput("peptideLevel_detQuant_quantile", "Quantile", 
                           value = rv$widgets$peptideImput$pepLevel_detQuantile
                           , step=1, min=0, max=100,
                           width='100px')),
    tags$div( style="display:inline-block; vertical-align: top; padding-right: 20px;",
              numericInput("peptideLevel_detQuant_factor", "Factor", 
                           value = rv$widgets$peptideImput$pepLevel_detQuant_factor,
                           step=1, min=0, max=10,
                           width='100px')
    )
  )
  
})


output$KNNOptsUI <- renderUI({
  req(rv$widgets$peptideImput$pepLevel_basicAlgorithm)
  req(rv$widgets$peptideImput$pepLevel_algorithm)
  if ((rv$widgets$peptideImput$pepLevel_basicAlgorithm != "KNN") || 
      (rv$widgets$peptideImput$pepLevel_algorithm != "BasicMethods")){return(NULL)}
  
  isolate({
    numericInput("KNN_n", "Neighbors", 
                 value = rv$widgets$peptideImput$pepLevel_KNN_n, 
                 step=1, min=0, 
                 max=max(rv$widgets$peptideImput$KNN_n,nrow(rv$current.obj)),
                 width='100px')
  })
})


output$imp4pOptsUI <- renderUI({
  if (rv$widgets$peptideImput$pepLevel_algorithm != "imp4p"){return(NULL)}
  
  updateSelectInput(session,"peptideLevel_missing.value.basic.algorithm", selected="None")
  tagList(
    tags$div( style="display:inline-block; vertical-align: top; padding-right: 40px;",
              numericInput("peptideLevel_imp4p_nbiter", "Iterations", 
                           value = rv$widgets$peptideImput$pepLevel_imp4p_nbiter,
                           step=1, min=1, width='100px')),
    
    tags$div( style="display:inline-block; vertical-align: bottom; padding-right: 20px;",
<<<<<<< HEAD
              checkboxInput("peptideLevel_imp4p_withLapala", "Impute MEC also", 
=======
              checkboxInput("peptideLevel_imp4p_withLapala", "Impute MEC also",
>>>>>>> 3f7a010f9977e8f1f29597873b6afa82b0158f85
                            value = rv$widgets$peptideImput$pepLevel_imp4p_withLapala ))
  )
})


output$imp4pOpts2UI <- renderUI({
  if (!isTRUE(rv$widgets$peptideImput$pepLevel_imp4p_withLapala)){return(NULL)}
  
  
  tagList(
    tags$div( style="display:inline-block; vertical-align: top; padding-right: 20px;",
              numericInput("peptideLevel_imp4p_qmin", "Upper lapala bound", 
                           value = rv$widgets$peptideImput$pepLevel_imp4p_qmin,
                           step=0.1, min=0, max=100,
                           width='100px')),
    tags$div( style="display:inline-block; vertical-align: top; padding-right: 20px;",
              radioButtons("peptideLevel_imp4pLAPALA_distrib", "Distribution type", 
                           choices = G_imp4PDistributionType_Choices,
                           selected = rv$widgets$peptideImput$pepLevel_imp4pLAPALA_distrib) 
    )
  )
})



output$peptideLevel_detQuant_impValues <- renderUI({
  req(rv$widgets$peptideImput$pepLevel_basicAlgorithm)
  req(rv$widgets$peptideImput$pepLevel_algorithm)
  if ((rv$widgets$peptideImput$pepLevel_basicAlgorithm != "detQuantile") || 
      (rv$widgets$peptideImput$pepLevel_algorithm != "BasicMethods")){return(NULL)}
  
  
  moduleDetQuantImpValuesUI("peptide_DetQuantValues_DT")
  
})

output$peptideLevel_TAB_detQuant_impValues <- renderDataTable(server=TRUE,{
  values <- getQuantile4Imp(Biobase::exprs(rv$current.obj), 
                            rv$widgets$peptideImput$pepLevel_detQuantile/100, 
                            rv$widgets$peptideImput$pepLevel_detQuant_factor)
  DT::datatable(round(as.data.frame(t(values$shiftedImpVal)), digits=rv$settings_nDigits),
                extensions = c('Scroller', 'Buttons'),
                options = list(initComplete = initComplete(),
                               buttons = list('copy',
                                              list(
                                                extend = 'csv',
                                                filename = 'detQuant_impValues'
                                              ),'print'),
                               dom='Bfrtip',
                               bLengthChange = FALSE))
})



# 
#------------------------------------------
##' Missing values imputation - reactivity behavior
##' @author Samuel Wieczorek
observeEvent(input$peptideLevel_perform.imputation.button,{
  
  nbMVBefore <- length(which(is.na(Biobase::exprs(rv$current.obj))==TRUE))
  algo <- rv$widgets$peptideImput$pepLevel_algorithm
  if (algo == "None"){
    rv$current.obj <- rv$dataset[[input$datasets]]
  } else {
    withProgress(message = '',detail = '', value = 0, {
      incProgress(0.5, detail = 'Imputation in progress')
      
      if (algo == "imp4p")
      {
        if (rv$widgets$peptideImput$pepLevel_imp4p_withLapala) {
          rv$current.obj <- wrapper.dapar.impute.mi(rv$dataset[[input$datasets]],
                                                    nb.iter = rv$widgets$peptideImput$pepLevel_imp4p_nbiter,
                                                    lapala = rv$widgets$peptideImput$pepLevel_imp4p_withLapala,
                                                    q.min = rv$widgets$peptideImput$pepLevel_imp4p_qmin / 100,
                                                    distribution = as.character(rv$widgets$peptideImput$pepLevel_imp4pLAPALA_distrib))
          
          
        } else {
          rv$current.obj <- wrapper.dapar.impute.mi(rv$dataset[[input$datasets]],
                                                    nb.iter = rv$widgets$peptideImput$pepLevel_imp4p_nbiter,
                                                    lapala = rv$widgets$peptideImput$pepLevel_imp4p_withLapala)
          
        }
        
        
      } else if (algo == "BasicMethods"){
        algoBasic <- rv$widgets$peptideImput$pepLevel_basicAlgorithm
        switch(algoBasic,
               KNN={  
                 rv$current.obj <- wrapper.impute.KNN(rv$dataset[[input$datasets]],K=rv$widgets$peptideImput$pepLevel_KNN_n)
               },
               MLE={
                 rv$current.obj <- wrapper.impute.mle(rv$dataset[[input$datasets]])},
               detQuantile=
                 {
                   rv$current.obj <- wrapper.impute.detQuant(rv$dataset[[input$datasets]],
                                                             qval = (rv$widgets$peptideImput$pepLevel_detQuantile/100),
                                                             factor = rv$widgets$peptideImput$pepLevel_detQuant_factor)
                 }
        )
      }
      incProgress(1, detail = 'Finalize imputation')
      
    })
  }
  
  
  nbMVAfter <- length(which(is.na(Biobase::exprs(rv$current.obj))==TRUE))
  rv$nbMVimputed <- nbMVAfter - nbMVBefore
  rvModProcess$modulePepImputationDone[1] <- TRUE
  
})






##' -- Validate the imputation ---------------------------------------
##' @author Samuel Wieczorek
observeEvent(input$peptideLevel_ValidImputation,{ 
  
  isolate({
    
    
    l.params <- build_ParamsList_PepImputation()
    
    name <- paste0("Imputed", ".", rv$typeOfDataset)
    rv$current.obj <- saveParameters(rv$current.obj, name,"peptideImputation",l.params)
    
    rvModProcess$modulePepImputationDone[2] <- TRUE
    UpdateDatasetWidget(rv$current.obj, name)
    
  })
})





output$peptideLevel_warningImputationMethod <- renderText({
  req(rv$widgets$peptideImput$pepLevel_algorithm)
  req(rv$widgets$peptideImput$pepLevel_imp4p_withLapala)
  
  
  if (rv$widgets$peptideImput$pepLevel_imp4p_withLapala == FALSE){return(NULL)}
  
  var <- ((rv$widgets$peptideImput$pepLevel_algorithm == "imp4p") && (isTRUE(rv$widgets$peptideImput$pepLevel_imp4p_withLapala))) ||
    (rv$widgets$peptideImput$pepLevel_basicAlgorithm ==  "BasicMethods")
  
  if (var){
    t <- "<br> 
    <font color=\"red\"><strong>Warning:</strong> Warning: Imputed MEC (Missing on the Entire Condition) 
    values must be very cautiously interpreted <br>[see the User manual, Section 6.3.1].</font color=\"red\">"
    HTML(t)}
  
})




###################


callModule(modulePopover,"modulePopover_helpForImputation", 
           data = reactive(list(title = p(if(is.null(rv$current.obj.name)) "No dataset" else paste0(rv$current.obj.name)),
                                
                                content="Before each processing step, a backup of the current dataset is stored. It is possible to reload one of them at any time.",
                                color = 'white')))



# output$helpForImputation <- renderText({
#   req(input$missing.value.algorithm)
#   input$missing.value.basic.algorithm
#   rv$typeOfDataset
#   
#   if ((input$missing.value.algorithm == "None")) {return(NULL)}
#   if ((input$missing.value.algorithm == "Basic methods") && is.null(input$missing.value.basic.algorithm == "None")) {return(NULL)}
#   
#   name <- NULL
#   
#   helpTextImputation <- list("imp4p" = "<strong>imp4p [5]</strong> is a proteomic-specific multiple imputation
#                              method that operates on peptide-level datasets and which proposes <br>
#                              to impute each missing value according to its nature (censored
#                              or random). <br> The more iterations, the more accurate the results,
#                              yet the more time-consuming.",
#                              "dummy censored" = "Dummy censored: each missing value is supposed to be a censored value and
#                              is replaced by the XXX quantile <br> of the corresponding sample
#                              abundance distribution",
#                              "KNN" = "<strong>K- nearest neighbors</strong>, see [7]",
#                              "MLE" = "<strong>Maximum likelihood estimation</strong>, see [8]")
#   
#   
#   if (input$missing.value.algorithm == "Basic methods") {
#     name <- input$missing.value.basic.algorithm}
#   else {name <- input$missing.value.algorithm}
#   
#   if (!is.null(name)) {
#     HTML(helpTextImputation[[name]])
#     
#   }
# })

