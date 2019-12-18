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
           reactive({rv$widgets$proteinImput$POV_detQuant_quantile}), 
           reactive({rv$widgets$proteinImput$POV_detQuant_factor}))

callModule(moduleDetQuantImpValues, "MEC_DetQuantValues_DT", 
           reactive({rv$widgets$proteinImput$MEC_detQuant_quantile}), 
           reactive({rv$widgets$proteinImput$MEC_detQuant_factor}))


callModule(moduleProcess, "moduleProcess_ProtImputation", 
           isDone = reactive({rvModProcess$moduleProtImputationDone}), 
           pages = reactive({rvModProcess$moduleProtImputation}),
           rstFunc = resetModuleProtImputation,
           forceReset = reactive({rvModProcess$moduleProtImputationForceReset })  )



resetModuleProtImputation <- reactive({  
  ## update widgets values (reactive values)
  resetModuleProcess("ProtImputation")
    
  rv$widgets$proteinImput$POV_algorithm <-  "None"
  rv$widgets$proteinImput$POV_detQuant_quantile <-  2.5
  rv$widgets$proteinImput$POV_detQuant_factor <-  1
  rv$widgets$proteinImput$POV_KNN_n <-  10
  rv$widgets$proteinImput$MEC_algorithm <-  "None"
  rv$widgets$proteinImput$MEC_detQuant_quantile <-  2.5
  rv$widgets$proteinImput$MEC_detQuant_factor <-  1
  rv$widgets$proteinImput$MEC_fixedValue <- 0
  
  
   rv$current.obj <- rv$dataset[[input$datasets]]
   rv$imputePlotsSteps[["step0"]] <- rv$current.obj 
   rvModProcess$moduleProtImputationDone = rep(FALSE, 3)
  
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
  rv$widgets$proteinImput$MEC_fixedValue <- input$MEC_fixedValue
})

observeEvent(input$MEC_detQuant_factor, {
  rv$widgets$proteinImput$MEC_detQuant_factor <- input$MEC_detQuant_factor
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
  
  req(rv$widgets$proteinImput$POV_algorithm)
  
  if (rv$widgets$proteinImput$POV_algorithm == 'detQuantile')
  {
    tagList(
      h5("The MEC will be imputed by the following values :"),
      moduleDetQuantImpValuesUI("POV_DetQuantValues_DT")
    )
  }
})

output$MEC_showDetQuantValues <- renderUI({
  
  req(rv$widgets$proteinImput$MEC_algorithm)
  
  if (rv$widgets$proteinImput$MEC_algorithm == 'detQuantile')
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
  req(rv$widgets$proteinImput$POV_algorithm)
  
  isolate({
    switch(rv$widgets$proteinImput$POV_algorithm,
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
  req(rv$widgets$proteinImput$MEC_algorithm)
  isolate({
  switch (rv$widgets$proteinImput$MEC_algorithm,
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
      
    busyIndicator(WaitMsgCalc,wait = 0)
    incProgress(0.5, detail = 'POV Imputation')
    switch(rv$widgets$proteinImput$POV_algorithm,
           slsa = {
             rv$MECIndex <- findMECBlock(rv$current.obj)
             rv$current.obj <- wrapper.impute.slsa(rv$current.obj)
             rv$current.obj <- reIntroduceMEC(rv$current.obj, rv$MECIndex)
             
           },
           detQuantile = {
             rv$MECIndex <- findMECBlock(rv$current.obj)
             rv$current.obj <- wrapper.impute.detQuant(rv$current.obj,
                                                       qval = rv$widgets$proteinImput$POV_detQuant_quantile/100,
                                                       factor = rv$widgets$proteinImput$POV_detQuant_factor)
           rv$current.obj <- reIntroduceMEC(rv$current.obj, rv$MECIndex)
           
           },
           KNN = {
             rv$current.obj <- wrapper.impute.KNN(rv$current.obj , rv$widgets$proteinImput$POV_KNN_n)
           }
    )
    incProgress(0.75, detail = 'Reintroduce MEC blocks')
    incProgress(1, detail = 'Finalize POV imputation')
    nbMVAfter <- length(which(is.na(Biobase::exprs(rv$current.obj))==TRUE))
    rv$nbPOVimputed <-  nbMVBefore - nbMVAfter
    
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
    nbMVBefore <- length(which(is.na(Biobase::exprs(rv$current.obj))==TRUE))
    incProgress(0.75, detail = 'MEC Imputation')
    switch(rv$widgets$proteinImput$MEC_algorithm,
           detQuantile = {
             rv$current.obj <- wrapper.impute.detQuant(rv$current.obj ,
                                                       qval = rv$widgets$proteinImput$MEC_detQuant_quantile/100,
                                                       factor = rv$widgets$proteinImput$MEC_detQuant_factor)
           },
           fixedValue = {
             rv$current.obj <- wrapper.impute.fixedValue(rv$current.obj,
                                                         fixVal = rv$widgets$proteinImput$MEC_fixedValue)
           }
    )
    
    nbMVAfter <- length(which(is.na(Biobase::exprs(rv$current.obj))==TRUE))
    rv$nbMECimputed <-  nbMVBefore - nbMVAfter
    
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
    UpdateDatasetWidget(rv$current.obj, name)
    
    
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
  #isolate({
    if (isTRUE(rvModProcess$moduleProtImputationDone[1])) {
      tagList(
        h5(paste0("POV imputation done.", rv$nbPOVimputed, " were imputed")),
        # br(),
        h5("Updated graphs can be seen on step \"2 - Missing on the Entire Condition\".")
      )
    }
 # })
})


output$ImputationStep2Done <- renderUI({
  #isolate({
    if (isTRUE(rvModProcess$moduleProtImputationDone[2])) {
      tagList(
        h5("MEC imputation done.", rv$nbMECimputed, " were imputed"),
        h5("Updated graphs cans be seen on step \"3 - Save\"."))
    }
  #})
})

output$warningMECImputation<- renderUI({
  
  tags$p(tags$b("Warning:"),"Imputing MEC in a conservative way
  is a real issue as, in the given condition, there is no observed value to rely on.
   Thus, if imputation is not avoidable, imputed MEC must be very cautiously interpreted.")
})








