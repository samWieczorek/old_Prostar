require(imp4p)


#source(file.path("./src", "modules/Misc/moduleNavigation.R"),  local = TRUE)$value
#source(file.path("./src", "modules/Plots/moduleMVPlots.R"),  local = TRUE)$value
source(file.path("./src", "modules/process/common/moduleDetQuantImpValues.R"),  local = TRUE)$value




############# Definition of the module   #########################

modulePepImputationUI <- function(id){
  ns <- NS(id)
  tagList(
    uiOutput(ns('bars')),
    hr(),
    uiOutput(ns('screens'))
  )
}

modulePepImputation <- function(input, output, session, dataIn, screen.id, settings=NULL){
  ns <- session$ns
  
  #def.progress.saveFiltering <- c("Build Parameters list", "Save Parameters list", "Compte adjacency matrix", "Compute connex composants", "Save new dataset")
  
  ###### definition of RV for navigation process
  rvNavProcess <- reactiveValues(
    Done = rep(FALSE,2),
    def = list(name = "PepImputation",
               stepsNames = c("PeptideImputation 1", "Save"),
               isMandatory = rep(TRUE,5),
               ll.UI = list(uiOutput(ns("screenPepImputation1")),
                             uiOutput(ns("screenPepImputation2"))
               ),
               rstFunc = reactive({resetModulePepImputation()}))
  )
  
  
  
  ### appel du module de navigation
  observe({
    rv.pepImputation$nav2 <- callModule(moduleNavigation2, "moduleProcess_PepImputation", 
               isDone = reactive({rvNavProcess$Done}), 
               pages = reactive({rvNavProcess$def}),
               rstFunc = resetModulePepImputation,
               type = reactive({'bubble'}))
  })
  
  
  ### Definition of rv for the filtering module
  rv.pepImputation <- reactiveValues(
    ## temporary current data in module
    obj =  NULL,
    nav2 = NULL,
    ## return result of the module
    dataOut = NULL, 
    name = "processPepImputation",
    
    widgets = list( pepLevel_algorithm = "None",
                    pepLevel_basicAlgorithm = "None",
                    pepLevel_detQuantile = 2.5,
                    pepLevel_detQuant_factor = 1,
                    pepLevel_imp4p_nbiter = 10,
                    pepLevel_imp4p_withLapala = FALSE,
                    pepLevel_imp4p_qmin = 2.5,
                    pepLevel_imp4pLAPALA_distrib = "beta",
                    pepLevel_KNN_n = 10),
    
    nbMVimputed = NULL
  )
  

  ################################################
  
  
  resetModulePepImputation <- reactive({  
    ## update rv.filtering$widgets values (reactive values)
    #resetModuleProcess("PepImputation")
    
    rv.pepImputation$widgets$pepLevel_algorithm <- "None"
    rv.pepImputation$widgets$pepLevel_basicAlgorithm <- "None"
    rv.pepImputation$widgets$pepLevel_detQuantile <- 2.5
    rv.pepImputation$widgets$pepLevel_detQuant_factor <- 1
    rv.pepImputation$widgets$pepLevel_imp4p_nbiter <- 10
    rv.pepImputation$widgets$pepLevel_imp4p_withLapala <- FALSE
    rv.pepImputation$widgets$pepLevel_imp4p_qmin <- 2.5
    rv.pepImputation$widgets$pepLevel_imp4pLAPALA_distrib <- "beta"
    rv.pepImputation$widgets$pepLevel_KNN_n <- 10
    
    ## update rv.filtering$widgets in UI
    #updateSelectInput(session, "ChooseFilters", selected = rv.filtering$widgets$ChooseFilters)
    # updateSelectInput(session, "seuilNA", selected = rv.filtering$widgets$seuilNA)
    
    rvNavProcess$Done = rep(FALSE, 2)
    ##update dataset to put the previous one
    #rv.filtering$obj <- rv$dataset[[last(names(rv$dataset))]] 
    
  })
  
  
  ### initialisation de la variable globale du process
  observe({
    dataIn()
    rv.pepImputation$obj <- dataIn()
  })
  
  
  output$bars <- renderUI({
    rv.pepImputation$nav2()$bars
  })
  
  
  output$screens <- renderUI({
    rv.pepImputation$nav2()$screens
  })
  
  
  ################# END of definitino part   #############################
  #######################################################################
  
  
  
  
  
  ############## Call different secondary modules ####################
  callModule(moduleMVPlots,"mvImputationPlots_PeptideLevel", 
             data=reactive(rv.pepImputation$obj),
             title = reactive("POV distribution"),
             palette =reactive(unique(settings()$paletteConditions)))
  
  
  callModule(moduleDetQuantImpValues, "peptide_DetQuantValues_DT",
             dataIn = reactive({rv.pepImputation$obj}),
             reactive({input$peptideLevel_detQuant_quantile}), 
             reactive({input$peptideLevel_detQuant_factor}))
  
  
  callModule(modulePopover,"modulePopover_HelpImputationPeptide", 
             data = reactive(list(title = HTML("<strong>Algorithm</strong>"),
                                  content= HTML(paste0("<ul><li><strong>imp4p [Ref. 7]</strong> a proteomic-specific multiple imputation method that operates on peptide-level datasets and which proposes to impute each missing value according to its nature (left-censored  or random). To tune the number of iterations, let us keep in mind that, the more iterations, the more accurate the results, yet the more time-consuming the computation.</li> <li><strong>Dummy censored:</strong> each missing value is supposed to be a censored value and is replaced by the XXX quantile of the corresponding sample abundance distribution <ul><li><strong>KNN </strong>see [Other ref. 2].</li><li><strong>MLE </strong>Imputation with the maximum likelihood estimate of the expected intensity (see the norm R package).</li></ul></ul>")))))
  
  
  callModule(modulePopover,"modulePopover_helpForImputation", 
             data = reactive(list(title = p(if(is.null(rv.pepImputation$obj.name)) "No dataset" else paste0(rv.pepImputation$obj.name)),
                                  
                                  content="Before each processing step, a backup of the current dataset is stored. It is possible to reload one of them at any time.",
                                  color = 'white')))
  
  
  
  
  
  
  ###############################################################################
  
  
  
  
  observeEvent(input$peptideLevel_missing.value.algorithm,{
    rv.pepImputation$widgets$pepLevel_algorithm <- input$peptideLevel_missing.value.algorithm})
  
  observeEvent(input$peptideLevel_missing.value.basic.algorithm,{
    rv.pepImputation$widgets$pepLevel_basicAlgorithm <- input$peptideLevel_missing.value.basic.algorithm})
  
  observeEvent(input$peptideLevel_detQuant_quantile,{
    rv.pepImputation$widgets$pepLevel_detQuantile <- input$peptideLevel_detQuant_quantile})
  
  observeEvent(input$peptideLevel_detQuant_factor,{
    rv.pepImputation$widgets$pepLevel_detQuant_factor <- input$peptideLevel_detQuant_factor})
  
  observeEvent(input$KNN_n,{
    rv.pepImputation$widgets$pepLevel_KNN_n <- input$KNN_n})
  
  observeEvent(input$peptideLevel_imp4p_nbiter,{
    rv.pepImputation$widgets$pepLevel_imp4p_nbiter <- input$peptideLevel_imp4p_nbiter})
  
  
  observeEvent(input$peptideLevel_imp4p_withLapala,{
    rv.pepImputation$widgets$pepLevel_imp4p_withLapala <- input$peptideLevel_imp4p_withLapala})
  
  observeEvent(input$peptideLevel_imp4p_qmin,{
    rv.pepImputation$widgets$pepLevel_imp4p_qmin <- input$peptideLevel_imp4p_qmin})
  
  observeEvent(input$peptideLevel_imp4pLAPALA_distrib,{
    rv.pepImputation$widgets$pepLevel_imp4pLAPALA_distrib <- input$peptideLevel_imp4pLAPALA_distrib})
  
  
  
  
  
  
  
  
  ##########
  #####  UI for the PEPTIDE LEVEL Imputation process
  ##########
  output$screenPepImputation1 <- renderUI({
    #req(rv.pepImputation$obj)
    # isolate({
    nbEmptyLines <- getNumberOfEmptyLines(Biobase::exprs(rv.pepImputation$obj))
    
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
                           modulePopoverUI(ns("modulePopover_HelpImputationPeptide")),
                           selectInput(ns("peptideLevel_missing.value.algorithm"),
                                       NULL,
                                       choices = imputationAlgorithms, 
                                       selected = rv.pepImputation$widgets$pepLevel_algorithm,
                                       width='150px')
                 ),
                 
                 tags$div( style="display:inline-block; vertical-align: top; padding-right: 20px;",
                           uiOutput(ns("basicAlgoUI"))),
                 tags$div( style="display:inline-block; vertical-align: top; padding-right: 20px;",
                           uiOutput(ns("detQuantOptsUI")),
                           uiOutput(ns("KNNOptsUI")),
                           uiOutput(ns("imp4pOptsUI"))),
                 tags$div( style="display:inline-block; vertical-align: top; padding-right: 20px;",
                           uiOutput(ns("imp4pOpts2UI"))),
                 tags$div( style="display:inline-block; vertical-align: top; padding-right: 20px;",
                           uiOutput(ns("peptideLevel_detQuant_impValues")))
                 
               ),
               tags$div(
                 tags$div( style="display:inline-block; vertical-align: top; padding-right: 20px;",
                           actionButton(ns("peptideLevel_perform.imputation.button"), "Perform imputation", class = actionBtnClass))
               ),
               br(), br(), br(),
               uiOutput(ns("warningImputationMethod")),
               
               ## progress bar
               #br(),
               #br(),
               #uiOutput(outputId = "progressOne")
               tagList(
                 tags$hr(),
                 busyIndicator(WaitMsgPlot,wait = 0),
                 moduleMVPlotsUI(ns("mvImputationPlots_PeptideLevel"))
               )      
               
      )
      
    }
    #})
  })
  
  
  
  output$screenPepImputation2 <- renderUI({
    
    tagList(
      actionButton(ns("peptideLevel_ValidImputation"), "Save imputation", class = actionBtnClass))
  })
  
  
  
  output$basicAlgoUI <- renderUI({
    if (input$peptideLevel_missing.value.algorithm != "BasicMethods"){return(NULL)}
    
    selectInput(ns("peptideLevel_missing.value.basic.algorithm"), 
                "Methods", width='150px',
                choices = basicMethodsImputationAlgos,
                selected = rv.pepImputation$widgets$pepLevel_basicAlgorithm)
    
  })
  
  
  output$detQuantOptsUI <- renderUI({
    req(input$peptideLevel_missing.value.basic.algorithm)
    req(input$peptideLevel_missing.value.algorithm)
    if ((input$peptideLevel_missing.value.basic.algorithm != "detQuantile") || 
        (input$peptideLevel_missing.value.algorithm != "BasicMethods")){return(NULL)}
    
    tagList(
      tags$div( style="display:inline-block; vertical-align: top; padding-right: 20px;",
                numericInput(ns("peptideLevel_detQuant_quantile"), "Quantile", 
                             value = rv.pepImputation$widgets$pepLevel_detQuantile
                             , step=1, min=0, max=100,
                             width='100px')),
      tags$div( style="display:inline-block; vertical-align: top; padding-right: 20px;",
                numericInput(ns("peptideLevel_detQuant_factor"), "Factor", 
                             value = rv.pepImputation$widgets$pepLevel_detQuant_factor,
                             step=1, min=0, max=10,
                             width='100px')
      )
    )
    
  })
  
  
  output$KNNOptsUI <- renderUI({
    req(input$peptideLevel_missing.value.basic.algorithm)
    req(input$peptideLevel_missing.value.algorithm)
    if ((input$peptideLevel_missing.value.basic.algorithm != "KNN") || 
        (input$peptideLevel_missing.value.algorithm != "BasicMethods")){return(NULL)}
    
    isolate({
      numericInput(ns("KNN_n"), "Neighbors", 
                   value = rv.pepImputation$widgets$pepLevel_KNN_n, 
                   step=1, min=0, 
                   max=max(rv$widgets$peptideImput$KNN_n,nrow(rv.pepImputation$obj)),
                   width='100px')
    })
  })
  
  
  output$imp4pOptsUI <- renderUI({
    if (input$peptideLevel_missing.value.algorithm != "imp4p"){return(NULL)}
    
    updateSelectInput(session,"peptideLevel_missing.value.basic.algorithm", selected="None")
    tagList(
      tags$div( style="display:inline-block; vertical-align: top; padding-right: 40px;",
                numericInput(ns("peptideLevel_imp4p_nbiter"), "Iterations", 
                             value = rv.pepImputation$widgets$pepLevel_imp4p_nbiter,
                             step=1, min=1, width='100px')),
      
      tags$div( style="display:inline-block; vertical-align: bottom; padding-right: 20px;",
                checkboxInput(ns("peptideLevel_imp4p_withLapala"), "Impute MEC also", 
                              value = rv.pepImputation$widgets$pepLevel_imp4p_withLapala ))
    )
  })
  
  
  output$imp4pOpts2UI <- renderUI({
    if (!isTRUE(input$peptideLevel_imp4p_withLapala)){return(NULL)}
    
    
    tagList(
      tags$div( style="display:inline-block; vertical-align: top; padding-right: 20px;",
                numericInput(ns("peptideLevel_imp4p_qmin"), "Upper lapala bound", 
                             value = rv.pepImputation$widgets$pepLevel_imp4p_qmin,
                             step=0.1, min=0, max=100,
                             width='100px')),
      tags$div( style="display:inline-block; vertical-align: top; padding-right: 20px;",
                radioButtons(ns("peptideLevel_imp4pLAPALA_distrib"), "Distribution type", 
                             choices = G_imp4PDistributionType_Choices,
                             selected = rv.pepImputation$widgets$pepLevel_imp4pLAPALA_distrib) 
      )
    )
  })
  
  
  
  output$peptideLevel_detQuant_impValues <- renderUI({
    req(input$peptideLevel_missing.value.basic.algorithm)
    req(input$peptideLevel_missing.value.algorithm)
    if ((input$peptideLevel_missing.value.basic.algorithm != "detQuantile") || 
        (input$peptideLevel_missing.value.algorithm != "BasicMethods")){return(NULL)}
    
    
    moduleDetQuantImpValuesUI(ns("peptide_DetQuantValues_DT"))
    
  })
  
  output$peptideLevel_TAB_detQuant_impValues <- renderDataTable({
    values <- getQuantile4Imp(Biobase::exprs(rv.pepImputation$obj), 
                              input$peptideLevel_detQuant_quantile/100, 
                              input$peptideLevel_detQuant_factor)
    DT::datatable(round(as.data.frame(t(values$shiftedImpVal)), digits=rv$settings_nDigits),
                  extensions = c('Scroller', 'Buttons'),
                  options = list(initComplete = initComplete(),
                                 dom = 'Bfrtip',
                                 bLengthChange = FALSE))
  })
  
  
  
  # 
  #------------------------------------------
  ##' Missing values imputation - reactivity behavior
  ##' @author Samuel Wieczorek
  observeEvent(input$peptideLevel_perform.imputation.button,{
    
    nbMVBefore <- length(which(is.na(Biobase::exprs(rv.pepImputation$obj))==TRUE))
    algo <- input$peptideLevel_missing.value.algorithm
    if (algo == "None"){
      rv.pepImputation$obj <- dataIn()
    } else {
      withProgress(message = '',detail = '', value = 0, {
        incProgress(0.5, detail = 'Imputation in progress')
        
        if (algo == "imp4p")
        {
          if (input$peptideLevel_imp4p_withLapala) {
            rv.pepImputation$obj <- wrapper.dapar.impute.mi(dataIn(),
                                                      #eps = input$imp4p_eps,
                                                      nb.iter = input$peptideLevel_imp4p_nbiter,
                                                      lapala = input$peptideLevel_imp4p_withLapala,
                                                      q.min = input$peptideLevel_imp4p_qmin / 100,
                                                      distribution = as.character(input$peptideLevel_imp4pLAPALA_distrib))
            
            
          } else {
            rv.pepImputation$obj <- wrapper.dapar.impute.mi(dataIn(),
                                                      #eps = input$imp4p_eps,
                                                      nb.iter = input$peptideLevel_imp4p_nbiter,
                                                      lapala = input$peptideLevel_imp4p_withLapala)
            
          }
          
          
        } else if (algo == "BasicMethods"){
          algoBasic <- input$peptideLevel_missing.value.basic.algorithm
          switch(algoBasic,
                 KNN={  
                   busyIndicator(WaitMsgCalc,wait = 0)
                   rv.pepImputation$obj <- wrapper.impute.KNN(dataIn(),K=input$KNN_n)
                 },
                 MLE={
                   busyIndicator(WaitMsgCalc,wait = 0)
                   rv.pepImputation$obj <- wrapper.impute.mle(dataIn())},
                 detQuantile=
                 {
                   rv.pepImputation$obj <- wrapper.impute.detQuant(dataIn(),
                                                             qval = (input$peptideLevel_detQuant_quantile/100),
                                                             factor = input$peptideLevel_detQuant_factor)
                 }
          )
        }
        incProgress(1, detail = 'Finalize imputation')
        
      })
    }
    
    
    nbMVAfter <- length(which(is.na(Biobase::exprs(rv.pepImputation$obj))==TRUE))
    rv.pepImputation$nbMVimputed <- nbMVAfter - nbMVBefore
    rvNavProcess$Done[1] <- TRUE
    
  })
  
  
  
  
  
  
  ##' -- Validate the imputation ---------------------------------------
  ##' @author Samuel Wieczorek
  observeEvent(input$peptideLevel_ValidImputation,{ 
    
    isolate({
      l.params <- build_ParamsList_PepImputation()
      
      name <- paste0("Imputed", ".", rv.pepImputation$obj@experimentData@other$typeOfDataset)
      rv.pepImputation$obj <- saveParameters(rv.pepImputation$obj, name,"peptideImputation",l.params)
      
      rv.pepImputation$dataOut <- rv.pepImputation$obj
      rvNavProcess$Done[2] <- TRUE
      
     
    })
  })
  
  
  
  
  
  output$peptideLevel_warningImputationMethod <- renderText({
    req(input$peptideLevel_missing.value.algorithm)
    req(input$peptideLevel_imp4p_withLapala)
    
    
    if (input$peptideLevel_imp4p_withLapala == FALSE){return(NULL)}
    
    var <- ((input$peptideLevel_missing.value.algorithm == "imp4p") && (isTRUE(input$peptideLevel_imp4p_withLapala))) ||
      (input$peptideLevel_missing.value.basic.algorithm ==  "BasicMethods")
    
    if (var){
      t <- "<br> 
      <font color=\"red\"><strong>Warning:</strong> Warning: Imputed MEC (Missing on the Entire Condition) 
      values must be very cautiously interpreted <br>[see the User manual, Section 6.3.1].</font color=\"red\">"
      HTML(t)}
    
  })
  
  
  
  
  
  build_ParamsList_PepImputation <- reactive({
    
    ll <- list( pepLevel_algorithm = input$peptideLevel_missing.value.algorithm,
                pepLevel_basicAlgorithm = input$peptideLevel_missing.value.basic.algorithm,
                pepLevel_detQuantile = input$peptideLevel_detQuant_quantile,
                pepLevel_detQuant_factor = input$peptideLevel_detQuant_factor,
                pepLevel_imp4p_nbiter = input$peptideLevel_imp4p_nbiter,
                pepLevel_imp4p_withLapala = input$peptideLevel_imp4p_withLapala,
                pepLevel_imp4p_qmin = input$peptideLevel_imp4p_qmin,
                pepLevel_imp4pLAPALA_distrib = input$peptideLevel_imp4pLAPALA_distrib,
                pepLevel_KNN_n = input$KNN_n)
    ll
  })
  
  
  
  return(reactive({rv.pepImputation$dataOut}))
  
}




