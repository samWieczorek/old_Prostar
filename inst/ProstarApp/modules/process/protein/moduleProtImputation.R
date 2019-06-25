require(imp4p)


source(file.path(".", "modules/moduleNavigation.R"),  local = TRUE)$value
source(file.path(".", "modules/Plots/moduleMVPlots.R"),  local = TRUE)$value
source(file.path(".", "modules/process/common/moduleDetQuantImpValues.R"),  local = TRUE)$value




############# Definition of the module   #########################

moduleProtImputationUI <- function(id){
  ns <- NS(id)
  tagList(
    uiOutput(ns('bars')),
    hr(),
    uiOutput(ns('screens'))
  )
}

moduleProtImputation <- function(input, output, session, dataIn, screen.id, settings=NULL){
  ns <- session$ns
  
  #def.progress.saveFiltering <- c("Build Parameters list", "Save Parameters list", "Compte adjacency matrix", "Compute connex composants", "Save new dataset")
  
  ###### definition of RV for navigation process
  rvNavProcess <- reactiveValues(
    Done = rep(FALSE,3),
    def = list(name = "ProtImputation",
               stepsNames = c("ProteinImputation 1", "Save"),
               isMandatory = rep(TRUE,5),
               ll.UI = list(uiOutput(ns("screenProtImputation1")),
                            uiOutput(ns("screenProtImputation2")),
                            uiOutput(ns("screenProtImputation3"))
               ),
               rstFunc = reactive({resetModuleProtImputation()}))
  )
  
  
  
  ### appel du module de navigation
  observe({
    rv.protImputation$nav2 <- callModule(moduleNavigation2, "moduleProcess_ProtImputation", 
               isDone = reactive({rvNavProcess$Done}), 
               pages = reactive({rvNavProcess$def}),
               rstFunc = resetModuleProtImputation,
               type = reactive({'bubble'}))
  })
  
  
  ### Definition of rv for the filtering module
  rv.protImputation <- reactiveValues(
    ## temporary current data in module
    obj =  NULL,
    nav2 = NULL,
    ## return result of the module
    dataOut = NULL, 
    name = "processProtImputation",
    
    widgets = list(POV_algorithm = "None",
         POV_detQuant_quantile = 2.5,
         POV_detQuant_factor = 1,
         POV_KNN_n = 10,
         MEC_algorithm = "None",
         MEC_detQuant_quantile = 2.5,
         MEC_detQuant_factor = 1,
         MEC_fixedValue= 0),
    
    nbMVimputed = NULL
  )
  

  ################################################
  
  
  resetModuleProtImputation <- reactive({  
    ## update rv.filtering$widgets values (reactive values)
    #resetModuleProcess("ProtImputation")
    
    rv.protImputation$widgets$protLevel_algorithm <- "None"
    rv.protImputation$widgets$protLevel_basicAlgorithm <- "None"
    rv.protImputation$widgets$protLevel_detQuantile <- 2.5
    rv.protImputation$widgets$protLevel_detQuant_factor <- 1
    rv.protImputation$widgets$protLevel_imp4p_nbiter <- 10
    rv.protImputation$widgets$protLevel_imp4p_withLapala <- FALSE
    rv.protImputation$widgets$protLevel_imp4p_qmin <- 2.5
    rv.protImputation$widgets$protLevel_imp4pLAPALA_distrib <- "beta"
    rv.protImputation$widgets$protLevel_KNN_n <- 10
    
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
    rv.protImputation$obj <- dataIn()
  })
  
  
  output$bars <- renderUI({
    rv.protImputation$nav2()$bars
  })
  
  
  output$screens <- renderUI({
    rv.protImputation$nav2()$screens
  })
  
  
  ################# END of definitino part   #############################
  #######################################################################
  
  
  
  
  
  ############## Call different secondary modules ####################
  callModule(moduleMVPlots,"mvImputationPlots_ProteinLevel", 
             data=reactive(rv.protImputation$obj),
             title = reactive("POV distribution"),
             palette =reactive(unique(settings()$paletteConditions)))
  
  
  callModule(moduleDetQuantImpValues, "Protein_DetQuantValues_DT",
             dataIn = reactive({rv.protImputation$obj}),
             reactive({input$proteinLevel_detQuant_quantile}), 
             reactive({input$proteinLevel_detQuant_factor}))
  
  
  callModule(modulePopover,"modulePopover_HelpImputationProtein", 
             data = reactive(list(title = HTML("<strong>Algorithm</strong>"),
                                  content= HTML(paste0("<ul><li><strong>imp4p [Ref. 7]</strong> a proteomic-specific multiple imputation method that operates on protein-level datasets and which proposes to impute each missing value according to its nature (left-censored  or random). To tune the number of iterations, let us keep in mind that, the more iterations, the more accurate the results, yet the more time-consuming the computation.</li> <li><strong>Dummy censored:</strong> each missing value is supposed to be a censored value and is replaced by the XXX quantile of the corresponding sample abundance distribution <ul><li><strong>KNN </strong>see [Other ref. 2].</li><li><strong>MLE </strong>Imputation with the maximum likelihood estimate of the expected intensity (see the norm R package).</li></ul></ul>")))))
  
  
  callModule(modulePopover,"modulePopover_helpForImputation", 
             data = reactive(list(title = p(if(is.null(rv.protImputation$obj.name)) "No dataset" else paste0(rv.protImputation$obj.name)),
                                  
                                  content="Before each processing step, a backup of the current dataset is stored. It is possible to reload one of them at any time.",
                                  color = 'white')))
  
  
  
  
  
  
  ###############################################################################
  
  
  
  
  observeEvent(input$proteinLevel_missing.value.algorithm,{
    rv.protImputation$widgets$protLevel_algorithm <- input$proteinLevel_missing.value.algorithm})
  
  observeEvent(input$proteinLevel_missing.value.basic.algorithm,{
    rv.protImputation$widgets$protLevel_basicAlgorithm <- input$proteinLevel_missing.value.basic.algorithm})
  
  observeEvent(input$proteinLevel_detQuant_quantile,{
    rv.protImputation$widgets$protLevel_detQuantile <- input$proteinLevel_detQuant_quantile})
  
  observeEvent(input$proteinLevel_detQuant_factor,{
    rv.protImputation$widgets$protLevel_detQuant_factor <- input$proteinLevel_detQuant_factor})
  
  observeEvent(input$KNN_n,{
    rv.protImputation$widgets$protLevel_KNN_n <- input$KNN_n})
  
  observeEvent(input$proteinLevel_imp4p_nbiter,{
    rv.protImputation$widgets$protLevel_imp4p_nbiter <- input$proteinLevel_imp4p_nbiter})
  
  
  observeEvent(input$proteinLevel_imp4p_withLapala,{
    rv.protImputation$widgets$protLevel_imp4p_withLapala <- input$proteinLevel_imp4p_withLapala})
  
  observeEvent(input$proteinLevel_imp4p_qmin,{
    rv.protImputation$widgets$protLevel_imp4p_qmin <- input$proteinLevel_imp4p_qmin})
  
  observeEvent(input$proteinLevel_imp4pLAPALA_distrib,{
    rv.protImputation$widgets$protLevel_imp4pLAPALA_distrib <- input$proteinLevel_imp4pLAPALA_distrib})
  
  
  
  
  
  
  
  
  ##########
  #####  UI for the PEPTIDE LEVEL Imputation process
  ##########
  output$screenProtImputation1 <- renderUI({
    #req(rv.protImputation$obj)
    # isolate({
    nbEmptyLines <- getNumberOfEmptyLines(Biobase::exprs(rv.protImputation$obj))
    
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
                           selectInput(ns("proteinLevel_missing.value.algorithm"),
                                       NULL,
                                       choices = imputationAlgorithms, 
                                       selected = rv.protImputation$widgets$protLevel_algorithm,
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
                           uiOutput(ns("proteinLevel_detQuant_impValues")))
                 
               ),
               tags$div(
                 tags$div( style="display:inline-block; vertical-align: top; padding-right: 20px;",
                           actionButton(ns("proteinLevel_perform.imputation.button"), "Perform imputation", class = actionBtnClass))
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
                 moduleMVPlotsUI(ns("mvImputationPlots_proteinLevel"))
               )      
               
      )
      
    }
    #})
  })
  
  
  
  output$screenProtImputation2 <- renderUI({
    
    tagList(
      actionButton(ns("proteinLevel_ValidImputation"), "Save imputation", class = actionBtnClass))
  })
  
  
  
  output$basicAlgoUI <- renderUI({
    if (input$proteinLevel_missing.value.algorithm != "BasicMethods"){return(NULL)}
    
    selectInput(ns("proteinLevel_missing.value.basic.algorithm"), 
                "Methods", width='150px',
                choices = basicMethodsImputationAlgos,
                selected = rv.protImputation$widgets$protLevel_basicAlgorithm)
    
  })
  
  
  output$detQuantOptsUI <- renderUI({
    req(input$proteinLevel_missing.value.basic.algorithm)
    req(input$proteinLevel_missing.value.algorithm)
    if ((input$proteinLevel_missing.value.basic.algorithm != "detQuantile") || 
        (input$proteinLevel_missing.value.algorithm != "BasicMethods")){return(NULL)}
    
    tagList(
      tags$div( style="display:inline-block; vertical-align: top; padding-right: 20px;",
                numericInput(ns("proteinLevel_detQuant_quantile"), "Quantile", 
                             value = rv.protImputation$widgets$protLevel_detQuantile
                             , step=1, min=0, max=100,
                             width='100px')),
      tags$div( style="display:inline-block; vertical-align: top; padding-right: 20px;",
                numericInput(ns("proteinLevel_detQuant_factor"), "Factor", 
                             value = rv.protImputation$widgets$protLevel_detQuant_factor,
                             step=1, min=0, max=10,
                             width='100px')
      )
    )
    
  })
  
  
  output$KNNOptsUI <- renderUI({
    req(input$proteinLevel_missing.value.basic.algorithm)
    req(input$proteinLevel_missing.value.algorithm)
    if ((input$proteinLevel_missing.value.basic.algorithm != "KNN") || 
        (input$proteinLevel_missing.value.algorithm != "BasicMethods")){return(NULL)}
    
    isolate({
      numericInput(ns("KNN_n"), "Neighbors", 
                   value = rv.protImputation$widgets$protLevel_KNN_n, 
                   step=1, min=0, 
                   max=max(rv$widgets$peptideImput$KNN_n,nrow(rv.protImputation$obj)),
                   width='100px')
    })
  })
  
  
  output$imp4pOptsUI <- renderUI({
    if (input$proteinLevel_missing.value.algorithm != "imp4p"){return(NULL)}
    
    updateSelectInput(session,"proteinLevel_missing.value.basic.algorithm", selected="None")
    tagList(
      tags$div( style="display:inline-block; vertical-align: top; padding-right: 40px;",
                numericInput(ns("proteinLevel_imp4p_nbiter"), "Iterations", 
                             value = rv.protImputation$widgets$protLevel_imp4p_nbiter,
                             step=1, min=1, width='100px')),
      
      tags$div( style="display:inline-block; vertical-align: bottom; padding-right: 20px;",
                checkboxInput(ns("proteinLevel_imp4p_withLapala"), "Impute MEC also", 
                              value = rv.protImputation$widgets$protLevel_imp4p_withLapala ))
    )
  })
  
  
  output$imp4pOpts2UI <- renderUI({
    if (!isTRUE(input$proteinLevel_imp4p_withLapala)){return(NULL)}
    
    
    tagList(
      tags$div( style="display:inline-block; vertical-align: top; padding-right: 20px;",
                numericInput(ns("proteinLevel_imp4p_qmin"), "Upper lapala bound", 
                             value = rv.protImputation$widgets$protLevel_imp4p_qmin,
                             step=0.1, min=0, max=100,
                             width='100px')),
      tags$div( style="display:inline-block; vertical-align: top; padding-right: 20px;",
                radioButtons(ns("proteinLevel_imp4pLAPALA_distrib"), "Distribution type", 
                             choices = G_imp4PDistributionType_Choices,
                             selected = rv.protImputation$widgets$protLevel_imp4pLAPALA_distrib) 
      )
    )
  })
  
  
  
  output$proteinLevel_detQuant_impValues <- renderUI({
    req(input$proteinLevel_missing.value.basic.algorithm)
    req(input$proteinLevel_missing.value.algorithm)
    if ((input$proteinLevel_missing.value.basic.algorithm != "detQuantile") || 
        (input$proteinLevel_missing.value.algorithm != "BasicMethods")){return(NULL)}
    
    
    moduleDetQuantImpValuesUI(ns("peptide_DetQuantValues_DT"))
    
  })
  
  output$proteinLevel_TAB_detQuant_impValues <- renderDataTable({
    values <- getQuantile4Imp(Biobase::exprs(rv.protImputation$obj), 
                              input$proteinLevel_detQuant_quantile/100, 
                              input$proteinLevel_detQuant_factor)
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
  observeEvent(input$proteinLevel_perform.imputation.button,{
    
    nbMVBefore <- length(which(is.na(Biobase::exprs(rv.protImputation$obj))==TRUE))
    algo <- input$proteinLevel_missing.value.algorithm
    if (algo == "None"){
      rv.protImputation$obj <- dataIn()
    } else {
      withProgress(message = '',detail = '', value = 0, {
        incProgress(0.5, detail = 'Imputation in progress')
        
        if (algo == "imp4p")
        {
          if (input$proteinLevel_imp4p_withLapala) {
            rv.protImputation$obj <- wrapper.dapar.impute.mi(dataIn(),
                                                      #eps = input$imp4p_eps,
                                                      nb.iter = input$proteinLevel_imp4p_nbiter,
                                                      lapala = input$proteinLevel_imp4p_withLapala,
                                                      q.min = input$proteinLevel_imp4p_qmin / 100,
                                                      distribution = as.character(input$proteinLevel_imp4pLAPALA_distrib))
            
            
          } else {
            rv.protImputation$obj <- wrapper.dapar.impute.mi(dataIn(),
                                                      #eps = input$imp4p_eps,
                                                      nb.iter = input$proteinLevel_imp4p_nbiter,
                                                      lapala = input$proteinLevel_imp4p_withLapala)
            
          }
          
          
        } else if (algo == "BasicMethods"){
          algoBasic <- input$proteinLevel_missing.value.basic.algorithm
          switch(algoBasic,
                 KNN={  
                   busyIndicator(WaitMsgCalc,wait = 0)
                   rv.protImputation$obj <- wrapper.impute.KNN(dataIn(),K=input$KNN_n)
                 },
                 MLE={
                   busyIndicator(WaitMsgCalc,wait = 0)
                   rv.protImputation$obj <- wrapper.impute.mle(dataIn())},
                 detQuantile=
                 {
                   rv.protImputation$obj <- wrapper.impute.detQuant(dataIn(),
                                                             qval = (input$proteinLevel_detQuant_quantile/100),
                                                             factor = input$proteinLevel_detQuant_factor)
                 }
          )
        }
        incProgress(1, detail = 'Finalize imputation')
        
      })
    }
    
    
    nbMVAfter <- length(which(is.na(Biobase::exprs(rv.protImputation$obj))==TRUE))
    rv.protImputation$nbMVimputed <- nbMVAfter - nbMVBefore
    rvNavProcess$Done[1] <- TRUE
    
  })
  
  
  
  
  
  
  ##' -- Validate the imputation ---------------------------------------
  ##' @author Samuel Wieczorek
  observeEvent(input$proteinLevel_ValidImputation,{ 
    
    isolate({
      l.params <- build_ParamsList_ProtImputation()
      
      name <- paste0("Imputed", ".", rv.protImputation$obj@experimentData@other$typeOfDataset)
      rv.protImputation$obj <- saveParameters(rv.protImputation$obj, name,"peptideImputation",l.params)
      
      rv.protImputation$dataOut <- rv.protImputation$obj
      rvNavProcess$Done[2] <- TRUE
      
     
    })
  })
  
  
  
  
  
  output$proteinLevel_warningImputationMethod <- renderText({
    req(input$proteinLevel_missing.value.algorithm)
    req(input$proteinLevel_imp4p_withLapala)
    
    
    if (input$proteinLevel_imp4p_withLapala == FALSE){return(NULL)}
    
    var <- ((input$proteinLevel_missing.value.algorithm == "imp4p") && (isTRUE(input$proteinLevel_imp4p_withLapala))) ||
      (input$proteinLevel_missing.value.basic.algorithm ==  "BasicMethods")
    
    if (var){
      t <- "<br> 
      <font color=\"red\"><strong>Warning:</strong> Warning: Imputed MEC (Missing on the Entire Condition) 
      values must be very cautiously interpreted <br>[see the User manual, Section 6.3.1].</font color=\"red\">"
      HTML(t)}
    
  })
  
  
  
  
  
  build_ParamsList_ProtImputation <- reactive({
    
    ll <- list( protLevel_algorithm = input$proteinLevel_missing.value.algorithm,
                protLevel_basicAlgorithm = input$proteinLevel_missing.value.basic.algorithm,
                protLevel_detQuantile = input$proteinLevel_detQuant_quantile,
                protLevel_detQuant_factor = input$proteinLevel_detQuant_factor,
                protLevel_imp4p_nbiter = input$proteinLevel_imp4p_nbiter,
                protLevel_imp4p_withLapala = input$proteinLevel_imp4p_withLapala,
                protLevel_imp4p_qmin = input$proteinLevel_imp4p_qmin,
                protLevel_imp4pLAPALA_distrib = input$proteinLevel_imp4pLAPALA_distrib,
                protLevel_KNN_n = input$KNN_n)
    ll
  })
  
  
  
  return(reactive({rv.protImputation$dataOut}))
  
}




