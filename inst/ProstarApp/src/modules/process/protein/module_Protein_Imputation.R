require(imp4p)


#source(file.path("./src", "modules/Misc/moduleNavigation.R"),  local = TRUE)$value
source(file.path("./src", "modules/Plots/moduleMVPlots.R"),  local = TRUE)$value
source(file.path("./src", "modules/process/common/moduleDetQuantImpValues.R"),  local = TRUE)$value




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
               stepsNames = c("POV imputation", "MEC imputation", "Save"),
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
    MECIndex = NULL,
    nbPOVimputed = NULL,
    impute_Step = 0,
    imputePlotsSteps = list(),
    
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
    
    rv.protImputation$widgets$POV_algorithm <- "None"
    rv.protImputation$widgets$POV_detQuant_quantile <- 2.5
    rv.protImputation$widgets$POV_detQuant_factor <- 1
    rv.protImputation$widgets$POV_KNN_n <- 10
    rv.protImputation$widgets$MEC_algorithm <- "None"
    rv.protImputation$widgets$MEC_detQuant_quantile <- 2.5
    rv.protImputation$widgets$MEC_detQuant_factor <- 1
    rv.protImputation$widgets$MEC_fixedValue <- 0
    
    
    rv.protImputation$MECIndex <- NULL
    rv.protImputation$nbPOVimputed <- NULL
    rv.protImputation$impute_Step <- 0
    
    ## update rv.filtering$widgets in UI
    #updateSelectInput(session, "ChooseFilters", selected = rv.filtering$widgets$ChooseFilters)
    # updateSelectInput(session, "seuilNA", selected = rv.filtering$widgets$seuilNA)
    
    rvNavProcess$Done = rep(FALSE, 3)
    
    
    ## update widgets in UI
    # updateSelectInput(session,"POV_missing.value.algorithm",selected=rv.protImputation$widgets$POV_algorithm)
    # updateSelectInput(session,"MEC_missing.value.algorithm", selected=rv.protImputation$widgets$MEC_algorithm)
    # updateNumericInput(session,"POV_detQuant_quantile", value = rv.protImputation$widgets$POV_detQuant_quantile)
    # updateNumericInput(session,"POV_detQuant_factor", value = rv.protImputation$widgets$POV_detQuant_factor)
    # updateNumericInput(session,"KNN_nbNeighbors", value = rv.protImputation$widgets$POV_KNN_n)
    # updateNumericInput(session, "MEC_detQuant_quantile", value = rv.protImputation$widgets$MEC_detQuant_quantile)
    # updateNumericInput(session, "MEC_detQuant_factor", value = rv.protImputation$widgets$MEC_detQuant_factor)
    # updateNumericInput(session, "MEC_fixedValue", value = rv.protImputation$widgets$MEC_fixedValue)
    # 
    
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
  
  
  callModule(moduleMVPlots,"mvImputationPlots_MV", 
             data=reactive(rv.protImputation$imputePlotsSteps[["step0"]]),
             title = reactive("POV distribution"),
             palette =reactive(unique(rv.protImputation$PlotParams$paletteConditions)))
  callModule(moduleMVPlots,"mvImputationPlots_MEC", 
             data=reactive(rv.protImputation$imputePlotsSteps[["step1"]]),
             title = reactive("Distribution after POV imputation"),
             palette =reactive(unique(rv.protImputation$PlotParams$paletteConditions)))
  callModule(moduleMVPlots,"mvImputationPlots_Valid", 
             data=reactive(rv.protImputation$imputePlotsSteps[["step2"]]),
             title = reactive("Distribution after POV and MEC imputation"),
             palette =reactive(unique(rv.protImputation$PlotParams$paletteConditions)))
  
  callModule(moduleDetQuantImpValues, "POV_DetQuantValues_DT", 
             dataIn = reactive({rv.protImputation$obj}),
             reactive({input$POV_detQuant_quantile}), 
             reactive({input$POV_detQuant_factor}))
  
  callModule(moduleDetQuantImpValues, "MEC_DetQuantValues_DT", 
             dataIn = reactive({rv.protImputation$obj}),
             reactive({input$MEC_detQuant_quantile}), 
             reactive({input$MEC_detQuant_factor}))
  
  
  
  
  callModule(modulePopover,"modulePopover_HelpImputationProtein", 
             data = reactive(list(title = HTML("<strong>Algorithm</strong>"),
                                  content= HTML(paste0("<ul><li><strong>imp4p [Ref. 7]</strong> a proteomic-specific multiple imputation method that operates on protein-level datasets and which proposes to impute each missing value according to its nature (left-censored  or random). To tune the number of iterations, let us keep in mind that, the more iterations, the more accurate the results, yet the more time-consuming the computation.</li> <li><strong>Dummy censored:</strong> each missing value is supposed to be a censored value and is replaced by the XXX quantile of the corresponding sample abundance distribution <ul><li><strong>KNN </strong>see [Other ref. 2].</li><li><strong>MLE </strong>Imputation with the maximum likelihood estimate of the expected intensity (see the norm R package).</li></ul></ul>")))))
  
  
  callModule(modulePopover,"modulePopover_helpForImputation", 
             data = reactive(list(title = p(if(is.null(rv.protImputation$obj.name)) "No dataset" else paste0(rv.protImputation$obj.name)),
                                  
                                  content="Before each processing step, a backup of the current dataset is stored. It is possible to reload one of them at any time.",
                                  color = 'white')))
  
  
  
  
  
  
  ###############################################################################
  
  
  ########
  observeEvent(input$POV_missing.value.algorithm, {
    rv.protImputation$widgets$POV_algorithm <- input$POV_missing.value.algorithm
  })
  
  observeEvent(input$MEC_missing.value.algorithm, {
    rv.protImputation$widgets$MEC_algorithm <- input$MEC_missing.value.algorithm
  })
  
  observeEvent(input$POV_detQuant_quantile, {
    rv.protImputation$widgets$POV_detQuant_quantile <- input$POV_detQuant_quantile
  })
  
  observeEvent(input$POV_detQuant_factor, {
    rv.protImputation$widgets$POV_detQuant_factor <- input$POV_detQuant_factor
  })
  
  observeEvent(input$KNN_nbNeighbors, {
    rv.protImputation$widgets$POV_KNN_n <- input$KNN_nbNeighbors
  })
  
  observeEvent(input$MEC_detQuant_quantile, {
    rv.protImputation$widgets$MEC_detQuant_quantile <- input$MEC_detQuant_quantile
  })
  
  observeEvent(input$MEC_fixedValue, {
    rv.protImputation$widgets$MEC_detQuant_factor <- input$MEC_fixedValue
  })
  
  observeEvent(input$MEC_detQuant_factor, {
    rv.protImputation$widgets$MEC_fixedValue <- input$MEC_detQuant_factor
  })
  #########
  
  
  
  
  
  
  
  ##########
  #####  UI for the PEPTIDE LEVEL Imputation process
  ##########
  output$screenProtImputation1 <- renderUI({
    tagList(
      tags$div(
        tags$div( style="display:inline-block; vertical-align: top; padding-right: 20px;",
                  uiOutput(ns("sidebar_imputation_step1"))),
        tags$div( style="display:inline-block; vertical-align: top; padding-right: 20px;",
                  uiOutput(ns("POV_Params"))),
        tags$div( style="display:inline-block; vertical-align: top; padding-right: 20px;",
                  uiOutput(ns("POV_showDetQuantValues")))
      ),
      tags$div(
        tags$div( style="display:inline-block; vertical-align: top; padding-right: 20px;",
                  actionButton(ns("perform.imputationClassical.button"),
                               "Perform imputation", class = actionBtnClass)),
        tags$div( style="display:inline-block; vertical-align: top; padding-right: 20px;",       
                  uiOutput(ns("ImputationStep1Done")))
        ),
      
      htmlOutput(ns("helpForImputation")),
      tags$hr(),
      moduleMVPlotsUI(ns("mvImputationPlots_MV"))
    )
    
  })
  
  
  
  output$screenProtImputation2 <- renderUI({
    
    tagList(
      uiOutput(ns("warningMECImputation")),
      tags$div(
        tags$div( style="display:inline-block; vertical-align: top; padding-right: 20px;",
                  uiOutput(ns("MEC_chooseImputationMethod"))),
        tags$div( style="display:inline-block; vertical-align: top; padding-right: 20px;",
                  uiOutput(ns("MEC_Params"))),
        tags$div( style="display:inline-block; vertical-align: top; padding-right: 20px;",
                  uiOutput(ns("MEC_showDetQuantValues")))),
      
      tags$div(
        tags$div( style="display:inline-block; vertical-align: top; padding-right: 20px;",
                  actionButton(ns("perform.imputationMEC.button"),"Perform imputation", class = actionBtnClass)),
        tags$div( style="display:inline-block; vertical-align: top; padding-right: 20px;",
                  uiOutput(ns("ImputationStep2Done")))
        ),
      
      busyIndicator(WaitMsgCalc,wait = 0),
      tags$hr(),
      moduleMVPlotsUI(ns("mvImputationPlots_MEC"))
    )
  })
  
  
  
  output$screenProtImputation3 <- renderUI({
    
    tagList(
      tags$div( style="display:inline-block; vertical-align: top; padding-right: 20px;",
                actionButton(ns("ValidImputation"),"Save imputation", class = actionBtnClass)),
      tags$div( style="display:inline-block; vertical-align: top; padding-right: 20px;",
                uiOutput(ns("ImputationSaved"))),
      tags$hr(),
      moduleMVPlotsUI(ns("mvImputationPlots_Valid"))
    )
  })
  
  
 #######################################################################
  
  
  
  
  output$POV_showDetQuantValues <- renderUI({
    
    req(input$POV_missing.value.algorithm)
    
    if (input$POV_missing.value.algorithm == 'detQuantile')
    {
      tagList(
        h5("The MEC will be imputed by the following values :"),
        moduleDetQuantImpValuesUI(ns("POV_DetQuantValues_DT"))
      )
    }
  })
  
  output$MEC_showDetQuantValues <- renderUI({
    
    req(input$MEC_missing.value.algorithm)
    
    if (input$MEC_missing.value.algorithm == 'detQuantile')
    {
      tagList(
        h5("The MEC will be imputed by the following values :"),
        moduleDetQuantImpValuesUI(ns("MEC_DetQuantValues_DT"))
      )
    }
  })
  
  
  
  
  
  
  output$MEC_chooseImputationMethod <- renderUI({
    algo <- imputationAlgorithmsProteins_MEC
    
    tags$div( style="display:inline-block; vertical-align: top; padding-right: 40px;",
              selectInput(ns("MEC_missing.value.algorithm"), "Algorithm for MEC", choices = algo,
                          selected=rv.protImputation$widgets$MEC_algorithm, width='150px')
    )
  })
  
  
  
  
  output$sidebar_imputation_step1 <- renderUI({
    # req(rv$current.obj)
    
    isolate({
      print("######## rv.protImputation$imputePlotsSteps[[step0]] #######")
      print(rv.protImputation$imputePlotsSteps[["step0"]])
     
      
      print("######## rv.protImputation$obj #######")
      print(rv.protImputation$obj)
      
      
      if (is.null(rv.protImputation$imputePlotsSteps[["step0"]])){
        print("marqueur 1")
        rv.protImputation$imputePlotsSteps[["step0"]] <- rv.protImputation$obj
        print("marqueur 2")
        shinyjs::enable("perform.imputationClassical.button")
        print("marqueur 3")
        
        
      } else {
        shinyjs::disable("perform.imputationClassical.button")
      }
      
      algo <- imputationAlgorithmsProteins_POV
      
      tags$div( style="display:inline-block; vertical-align: top; padding-right: 40px;",
                selectInput(ns("POV_missing.value.algorithm"),"Algorithm for POV",
                            choices = algo, 
                            selected=rv.protImputation$widgets$POV_algorithm, 
                            width='150px')
      )
      
    })
  })
  
  
  observeEvent(input$perform.imputationClassical.button,{
    
    isolate({
      
    rv.protImputation$MECIndex <-NULL
      rv.protImputation$obj <- rv.protImputation$imputePlotsSteps[["step0"]]
      nbMVBefore <- length(which(is.na(Biobase::exprs(rv.protImputation$obj))==TRUE))
      
      withProgress(message = '',detail = '', value = 0, {
        incProgress(0.25, detail = 'Find MEC blocks')
        
      rv.protImputation$MECIndex <- findMECBlock(rv.protImputation$obj)
        busyIndicator(WaitMsgCalc,wait = 0)
        incProgress(0.5, detail = 'POV Imputation')
        switch(input$POV_missing.value.algorithm,
               slsa = {
                 rv.protImputation$obj <- wrapper.impute.slsa(rv.protImputation$obj)
               },
               detQuantile = {
                 rv.protImputation$obj <- wrapper.impute.detQuant(rv.protImputation$obj,
                                                           qval = input$POV_detQuant_quantile/100,
                                                           factor = input$POV_detQuant_factor)
                 
               },
               KNN = {
                 rv.protImputation$obj <- wrapper.impute.KNN(rv.protImputation$obj , input$KNN_nbNeighbors)
               }
        )
        incProgress(0.75, detail = 'Reintroduce MEC blocks')
        rv.protImputation$obj <- reIntroduceMEC(rv.protImputation$obj, rv.protImputation$MECIndex)
        incProgress(1, detail = 'Finalize POV imputation')
        nbMVAfter <- length(which(is.na(Biobase::exprs(rv.protImputation$obj))==TRUE))
      rv.protImputation$nbPOVimputed <- nbMVAfter - nbMVBefore
        
      rv.protImputation$impute_Step <- 1
      rv.protImputation$imputePlotsSteps[["step1"]] <- rv.protImputation$obj
      rvNavProcess$Done[1] <- TRUE
      
        shinyjs::enable("perform.imputationMEC.button")
        shinyjs::enable("ValidImputation")
        
      })
    })
  })
  
  
  
  
  observeEvent(input$perform.imputationMEC.button,{
    
    isolate({
      busyIndicator(WaitMsgCalc,wait = 0)
      withProgress(message = '',detail = '', value = 0, {
        incProgress(0.25, detail = 'Reintroduce MEC')
        
        rv.protImputation$obj <- reIntroduceMEC(rv.protImputation$obj, rv.protImputation$MECIndex)
        incProgress(0.75, detail = 'MEC Imputation')
        switch(input$MEC_missing.value.algorithm,
               detQuantile = {
                 rv.protImputation$obj <- wrapper.impute.detQuant(rv.protImputation$obj ,
                                                           qval = input$MEC_detQuant_quantile/100,
                                                           factor = input$MEC_detQuant_factor)
               },
               fixedValue = {
                 rv.protImputation$obj <- wrapper.impute.fixedValue(rv.protImputation$obj,
                                                             fixVal = input$MEC_fixedValue)
               }
        )
        
        incProgress(1, detail = 'Finalize MEC imputation')
      rv.protImputation$impute_Step <- 2
      rv.protImputation$imputePlotsSteps[["step2"]] <- rv.protImputation$obj
      rvNavProcess$Done[2] <- TRUE
      })
    })
  })
  
  
  
  
  
  
  
  ##' -- Validate and Save the imputation ---------------------------------------
  ##' @author Samuel Wieczorek
  observeEvent(input$ValidImputation,{
    
    isolate({
      
      name <- paste0("Imputed", ".", rv.protImputation$typeOfDataset)
      rv.protImputation$obj <- saveParameters(rv.protImputation$obj, name,"proteinImputation",build_ParamsList_ProtImputation())
      
      rv.protImputation$dataOut <- rv.protImputation$obj
      rv.protImputation$ValidImputationClicked <- TRUE
      rvNavProcess$Done[3] <- TRUE
    })
  })
  
  
  
  
  
  
  output$ImputationSaved <- renderUI({
    req(input$datasets)
    if ((length(grep("Imputed",input$datasets)) !=1) ) {return(NULL)  }
    else if (grep("Imputed",input$datasets) == 1 ) {
      h4("The imputed dataset has been saved.")
    }
  })
  
  output$ImputationStep1Done <- renderUI({
  rv.protImputation$impute_Step
    isolate({
      if (rv.protImputation$impute_Step >= 1) {
        tagList(
          h5(paste0("POV imputation done.", rv.protImputation$nbPOVimputed, " were imputed")),
          # br(),
          h5("Updated graphs can be seen on tab \"2 - Missing on the Entire Condition\".")
        )
      }
    })
  })
  
  
  output$ImputationStep2Done <- renderUI({
  rv.protImputation$impute_Step
    isolate({
      if (rv.protImputation$impute_Step >= 2) {
        tagList(
          h5("MEC imputation done."),
          h5("Updated graphs cans be seen on tab \"3 - Validate and save\"."))
      }
    })
  })
  
  output$warningMECImputation<- renderUI({
    
    tags$p(tags$b("Warning:"),"Imputing MEC in a conservative way
  is a real issue as, in the given condition, there is no observed value to rely on.
   Thus, if imputation is not avoidable, imputed MEC must be very cautiously interpreted.")
  })
  
  
  
  
  
  output$helpForImputation <- renderText({
    req(input$missing.value.algorithm)
    input$missing.value.basic.algorithm
    rv.protImputation$typeOfDataset
    
    if ((input$missing.value.algorithm == "None")) {return(NULL)}
    if ((input$missing.value.algorithm == "Basic methods") && is.null(input$missing.value.basic.algorithm == "None")) {return(NULL)}
    
    name <- NULL
    
    helpTextImputation <- list("imp4p" = "<strong>imp4p [5]</strong> is a proteomic-specific multiple imputation
                             method that operates on peptide-level datasets and which proposes <br>
                             to impute each missing value according to its nature (censored
                             or random). <br> The more iterations, the more accurate the results,
                             yet the more time-consuming.",
                               "dummy censored" = "Dummy censored: each missing value is supposed to be a censored value and
                             is replaced by the XXX quantile <br> of the corresponding sample
                             abundance distribution",
                               "KNN" = "<strong>K- nearest neighbors</strong>, see [7]",
                               "MLE" = "<strong>Maximum likelihood estimation</strong>, see [8]")
    
    
    if (input$missing.value.algorithm == "Basic methods") {
      name <- input$missing.value.basic.algorithm}
    else {name <- input$missing.value.algorithm}
    
    if (!is.null(name)) {
      HTML(helpTextImputation[[name]])
      
    }
  })
  
  
  
  
  output$POV_Params <- renderUI({
    req(input$POV_missing.value.algorithm)
    
    isolate({
      switch(input$POV_missing.value.algorithm,
             detQuantile = {
               
               tagList(
                 tags$div( style="display:inline-block; vertical-align: top; padding-right: 40px;",
                           numericInput(ns("POV_detQuant_quantile"), "Quantile", 
                                        value = rv.protImputation$widgets$POV_detQuant_quantile, 
                                        step=0.5, min=0, max=100, width='100px')),
                 tags$div( style="display:inline-block; vertical-align: top; padding-right: 40px;",
                           numericInput(ns("POV_detQuant_factor"), "Factor", 
                                        value = rv.protImputation$widgets$POV_detQuant_factor,
                                        step=0.1, min=0, max=10, width='100px'))
               )
             },
             KNN = {
               numericInput(ns("KNN_nbNeighbors"), "Neighbors", 
                            value = rv.protImputation$widgets$POV_KNN_n, step=1, min=0, 
                            max=max(nrow(rv.protImputation$obj), rv.protImputation$widgets$POV_KNN_n), 
                            width='100px')
             }
      )
      
    })
  })
  
  
  
  output$MEC_Params <- renderUI({
    req(input$MEC_missing.value.algorithm)
    isolate({
      switch (input$MEC_missing.value.algorithm,
              detQuantile = {
                tagList(
                  tags$div( style="display:inline-block; vertical-align: top; padding-right: 40px;",
                            numericInput(ns("MEC_detQuant_quantile"), "Quantile", 
                                         value = rv.protImputation$widgets$MEC_detQuant_quantile,
                                         step=0.5, min=0, max=100,
                                         width='100px')),
                  tags$div( style="display:inline-block; vertical-align: top; padding-right: 40px;",
                            numericInput(ns("MEC_detQuant_factor"), "Factor", 
                                         value = rv.protImputation$widgets$MEC_detQuant_factor, 
                                         step=0.1, min=0, max=10,
                                         width='100px'))
                )
              },
              fixedValue = {
                
                numericInput(ns("MEC_fixedValue"), "Fixed value", 
                             value = rv.protImputation$widgets$MEC_fixedValue, 
                             step=0.1, min=0, max=100,
                             width='100px')
                
              })
      
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




