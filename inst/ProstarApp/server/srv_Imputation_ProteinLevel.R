require(imp4p)


callModule(moduleMVPlots,"mvImputationPlots_MV", 
           data=reactive(rv$imputePlotsSteps[["step0"]]),
           title = reactive("POV distribution"),
           palette =reactive(unique(rv$PlotParams$paletteConditions)))
callModule(moduleMVPlots,"mvImputationPlots_MEC", 
           data=reactive(rv$imputePlotsSteps[["step1"]]),
           title = reactive("Distribution after POV imputation"),
           palette =reactive(unique(rv$PlotParams$paletteConditions)))
callModule(moduleMVPlots,"mvImputationPlots_Valid", 
           data=reactive(rv$imputePlotsSteps[["step2"]]),
           title = reactive("Distribution after POV and MEC imputation"),
           palette =reactive(unique(rv$PlotParams$paletteConditions)))

callModule(moduleDetQuantImpValues, "POV_DetQuantValues_DT", 
           reactive({input$POV_detQuant_quantile}), 
           reactive({input$POV_detQuant_factor}))

callModule(moduleDetQuantImpValues, "MEC_DetQuantValues_DT", 
           reactive({input$MEC_detQuant_quantile}), 
           reactive({input$MEC_detQuant_factor}))


callModule(moduleProcess, "moduleProcess_ProtImputation", 
           isDone = reactive({rvModProcess$moduleProtImputationDone}), 
           pages = reactive({rvModProcess$moduleProtImputation}),
           rstFunc = resetModuleProtImputation)

resetModuleProtImputation <- reactive({  
  ## update widgets values (reactive values)
  resetModuleProcess("ProtImputation")
    
  ## update widgets in UI
  updateSelectInput(session,"POV_missing.value.algorithm",selected=rv$widgets$proteinImput$POV_algorithm)
  updateSelectInput(session,"MEC_missing.value.algorithm", selected=rv$widgets$proteinImput$MEC_algorithm)
  updateNumericInput(session,"POV_detQuant_quantile", value = rv$widgets$proteinImput$POV_detQuant_quantile)
  updateNumericInput(session,"POV_detQuant_factor", value = rv$widgets$proteinImput$POV_detQuant_factor)
  updateNumericInput(session,"KNN_nbNeighbors", value = rv$widgets$proteinImput$POV_KNN_n)
  updateNumericInput(session, "MEC_detQuant_quantile", value = rv$widgets$proteinImput$MEC_detQuant_quantile)
  updateNumericInput(session, "MEC_detQuant_factor", value = rv$widgets$proteinImput$MEC_detQuant_factor)
  updateNumericInput(session, "MEC_fixedValue", value = rv$widgets$proteinImput$MEC_fixedValue)
  
  
  
  
  rvModProcess$moduleProtImputationDone = rep(FALSE, 3)
  
  ##update dataset to put the previous one
  rv$current.obj <- rv$dataset[[last(names(rv$dataset))]] 
  
})


########
observeEvent(input$POV_missing.value.algorithm, {
  rv$widgets$proteinImput$POV_algorithm <- input$POV_missing.value.algorithm
})

observeEvent(input$MEC_missing.value.algorithm, {
  rv$widgets$proteinImput$MEC_algorithm <- input$MEC_missing.value.algorithm
})

observeEvent(input$POV_detQuant_quantile, {
  rv$widgets$proteinImput$POV_detQuant_quantile <- input$POV_detQuant_quantile
})

observeEvent(input$POV_detQuant_factor, {
  rv$widgets$proteinImput$POV_detQuant_factor <- input$POV_detQuant_factor
})

observeEvent(input$KNN_nbNeighbors, {
  rv$widgets$proteinImput$POV_KNN_n <- input$KNN_nbNeighbors
})

observeEvent(input$MEC_detQuant_quantile, {
  rv$widgets$proteinImput$MEC_detQuant_quantile <- input$MEC_detQuant_quantile
})

observeEvent(input$MEC_fixedValue, {
  rv$widgets$proteinImput$MEC_detQuant_factor <- input$MEC_fixedValue
})

observeEvent(input$MEC_detQuant_factor, {
  rv$widgets$proteinImput$MEC_fixedValue <- input$MEC_detQuant_factor
})
#########



output$screenProtImput1 <- renderUI({
 
  tagList(
    tags$div(
      tags$div( style="display:inline-block; vertical-align: top; padding-right: 20px;",
              uiOutput("sidebar_imputation_step1")),
    tags$div( style="display:inline-block; vertical-align: top; padding-right: 20px;",
              uiOutput("POV_Params")),
    tags$div( style="display:inline-block; vertical-align: top; padding-right: 20px;",
              uiOutput("POV_showDetQuantValues"))
    ),
    tagList(
      tags$div( style="display:inline-block; vertical-align: top; padding-right: 20px;",
                         actionButton("perform.imputationClassical.button",
                                     "Perform imputation", class = actionBtnClass)),
      tags$div( style="display:inline-block; vertical-align: top; padding-right: 20px;",       
                uiOutput("ImputationStep1Done"))),
    
    htmlOutput("helpForImputation"),
    tags$hr(),
    moduleMVPlotsUI("mvImputationPlots_MV")
              )

})



output$screenProtImput2 <- renderUI({
  
 
  tagList(
    uiOutput("warningMECImputation"),
    tags$div(
      tags$div( style="display:inline-block; vertical-align: top; padding-right: 20px;",
                       uiOutput("MEC_chooseImputationMethod")),
      tags$div( style="display:inline-block; vertical-align: top; padding-right: 20px;",
                uiOutput("MEC_Params")),
      tags$div( style="display:inline-block; vertical-align: top; padding-right: 20px;",
                uiOutput("MEC_showDetQuantValues"))),
                
    tagList(
      tags$div( style="display:inline-block; vertical-align: top; padding-right: 20px;",
                actionButton("perform.imputationMEC.button","Perform imputation", class = actionBtnClass)),
      tags$div( style="display:inline-block; vertical-align: top; padding-right: 20px;",
                uiOutput("ImputationStep2Done"))),
    
      busyIndicator(WaitMsgCalc,wait = 0),
      tags$hr(),
      moduleMVPlotsUI("mvImputationPlots_MEC")
      )

})



output$screenProtImput3 <- renderUI({
  
  tagList(
    tags$div( style="display:inline-block; vertical-align: top; padding-right: 20px;",
             actionButton("ValidImputation","Save imputation", class = actionBtnClass)),
    tags$div( style="display:inline-block; vertical-align: top; padding-right: 20px;",
               uiOutput("ImputationSaved")),
    tags$hr(),
    moduleMVPlotsUI("mvImputationPlots_Valid")
              )
})







output$POV_showDetQuantValues <- renderUI({
  
  req(input$POV_missing.value.algorithm)
  
  if (input$POV_missing.value.algorithm == 'detQuantile')
  {
    tagList(
      h5("The MEC will be imputed by the following values :"),
      moduleDetQuantImpValuesUI("POV_DetQuantValues_DT")
    )
  }
})

output$MEC_showDetQuantValues <- renderUI({
  
  req(input$MEC_missing.value.algorithm)
  
  if (input$MEC_missing.value.algorithm == 'detQuantile')
  {
    tagList(
      h5("The MEC will be imputed by the following values :"),
      moduleDetQuantImpValuesUI("MEC_DetQuantValues_DT")
    )
  }
})



output$sidebar_imputation_step1 <- renderUI({
 # req(rv$current.obj)
  
  isolate({
  if (length(grep("Imputed", input$datasets))==0){
    rv$imputePlotsSteps[["step0"]] <- rv$dataset[[input$datasets]]
    shinyjs::enable("perform.imputationClassical.button")
    
  } else {
    shinyjs::disable("perform.imputationClassical.button")
  }
  
  algo <- imputationAlgorithmsProteins_POV
  
  tags$div( style="display:inline-block; vertical-align: top; padding-right: 40px;",
            selectInput("POV_missing.value.algorithm","Algorithm for POV",
                        choices = algo, 
                        selected=rv$widgets$proteinImput$POV_algorithm, 
                        width='150px')
  )
  
  })
})


output$MEC_chooseImputationMethod <- renderUI({
  algo <- imputationAlgorithmsProteins_MEC
  
  tags$div( style="display:inline-block; vertical-align: top; padding-right: 40px;",
            selectInput("MEC_missing.value.algorithm", "Algorithm for MEC", choices = algo,
              selected=rv$widgets$proteinImput$MEC_algorithm, width='150px')
  )
})






output$POV_Params <- renderUI({
  req(input$POV_missing.value.algorithm)
  
  isolate({
    switch(input$POV_missing.value.algorithm,
         detQuantile = {
           
           tagList(
               tags$div( style="display:inline-block; vertical-align: top; padding-right: 40px;",
                         numericInput("POV_detQuant_quantile", "Quantile", 
                            value = rv$widgets$proteinImput$POV_detQuant_quantile, 
                            step=0.5, min=0, max=100, width='100px')),
               tags$div( style="display:inline-block; vertical-align: top; padding-right: 40px;",
                         numericInput("POV_detQuant_factor", "Factor", 
                          value = rv$widgets$proteinImput$POV_detQuant_factor,
                          step=0.1, min=0, max=10, width='100px'))
           )
         },
         KNN = {
            numericInput("KNN_nbNeighbors", "Neighbors", 
                        value = rv$widgets$proteinImput$POV_KNN_n, step=1, min=0, 
                        max=max(nrow(rv$current.obj), rv$widgets$proteinImput$POV_KNN_n), 
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
                        numericInput("MEC_detQuant_quantile", "Quantile", 
                           value = rv$widgets$proteinImput$MEC_detQuant_quantile,
                           step=0.5, min=0, max=100,
                           width='100px')),
              tags$div( style="display:inline-block; vertical-align: top; padding-right: 40px;",
                        numericInput("MEC_detQuant_factor", "Factor", 
                           value = rv$widgets$proteinImput$MEC_detQuant_factor, 
                           step=0.1, min=0, max=10,
                           width='100px'))
            )
          },
          fixedValue = {

              numericInput("MEC_fixedValue", "Fixed value", 
                           value = rv$widgets$proteinImput$MEC_fixedValue, 
                           step=0.1, min=0, max=100,
                           width='100px')
 
          })
    
  })
})



observeEvent(input$perform.imputationClassical.button,{
  
  isolate({
    
    rv$MECIndex <-NULL
    rv$current.obj <- rv$imputePlotsSteps[["step0"]]
    nbMVBefore <- length(which(is.na(Biobase::exprs(rv$current.obj))==TRUE))
    
    withProgress(message = '',detail = '', value = 0, {
      incProgress(0.25, detail = 'Find MEC blocks')
      
    rv$MECIndex <- findMECBlock(rv$current.obj)
    busyIndicator(WaitMsgCalc,wait = 0)
    incProgress(0.5, detail = 'POV Imputation')
    switch(input$POV_missing.value.algorithm,
           slsa = {
             rv$current.obj <- wrapper.impute.slsa(rv$current.obj)
           },
           detQuantile = {
           rv$current.obj <- wrapper.impute.detQuant(rv$current.obj,
                                                       qval = input$POV_detQuant_quantile/100,
                                                       factor = input$POV_detQuant_factor)
           
           },
           KNN = {
             rv$current.obj <- wrapper.impute.KNN(rv$current.obj , input$KNN_nbNeighbors)
           }
    )
    incProgress(0.75, detail = 'Reintroduce MEC blocks')
    rv$current.obj <- reIntroduceMEC(rv$current.obj, rv$MECIndex)
    incProgress(1, detail = 'Finalize POV imputation')
    nbMVAfter <- length(which(is.na(Biobase::exprs(rv$current.obj))==TRUE))
    rv$nbPOVimputed <- nbMVAfter - nbMVBefore
    
    rv$impute_Step <- 1
    rv$imputePlotsSteps[["step1"]] <- rv$current.obj
    rvModProcess$moduleProtImputationDone[1] <- TRUE
    shinyjs::enable("perform.imputationMEC.button")
    shinyjs::enable("ValidImputation")
    
    })
  })
})


#################################################################################
#################################################################################
#################################################################################


observeEvent(input$perform.imputationMEC.button,{
  
     isolate({
    busyIndicator(WaitMsgCalc,wait = 0)
       withProgress(message = '',detail = '', value = 0, {
         incProgress(0.25, detail = 'Reintroduce MEC')
         
    rv$current.obj <- reIntroduceMEC(rv$current.obj, rv$MECIndex)
    incProgress(0.75, detail = 'MEC Imputation')
    switch(input$MEC_missing.value.algorithm,
           detQuantile = {
             rv$current.obj <- wrapper.impute.detQuant(rv$current.obj ,
                                                       qval = input$MEC_detQuant_quantile/100,
                                                       factor = input$MEC_detQuant_factor)
           },
           fixedValue = {
             rv$current.obj <- wrapper.impute.fixedValue(rv$current.obj,
                                                         fixVal = input$MEC_fixedValue)
           }
    )
    
    incProgress(1, detail = 'Finalize MEC imputation')
    rv$impute_Step <- 2
    rv$imputePlotsSteps[["step2"]] <- rv$current.obj
    rvModProcess$moduleProtImputationDone[2] <- TRUE
    })
     })
})







##' -- Validate and Save the imputation ---------------------------------------
##' @author Samuel Wieczorek
observeEvent(input$ValidImputation,{
  
  isolate({
    
    name <- paste0("Imputed", ".", rv$typeOfDataset)
    rv$current.obj <- saveParameters(rv$current.obj, name,"proteinImputation",build_ParamsList_ProteinImputation())
    
    rv$dataset[[name]] <- rv$current.obj
    
    updateSelectInput(session, "datasets",
                      #paste("Dataset versions of",rv$current.obj.name, sep=" "),
                      choices = names(rv$dataset),
                      selected = name)
    
       rv$ValidImputationClicked <- TRUE
    rvModProcess$moduleProtImputationDone[3] <- TRUE
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
  rv$impute_Step
  isolate({
    if (rv$impute_Step >= 1) {
      tagList(
        h5(paste0("POV imputation done.", rv$nbPOVimputed, " were imputed")),
        # br(),
        h5("Updated graphs can be seen on tab \"2 - Missing on the Entire Condition\".")
      )
    }
  })
})


output$ImputationStep2Done <- renderUI({
  rv$impute_Step
  isolate({
    if (rv$impute_Step >= 2) {
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
  rv$typeOfDataset
  
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



