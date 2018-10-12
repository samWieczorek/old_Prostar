require(imp4p)


callModule(moduleMVPlots,"mvImputationPlots_MV", data=reactive(rv$imputePlotsSteps[["step0"]]))
callModule(moduleMVPlots,"mvImputationPlots_MEC", data=reactive(rv$imputePlotsSteps[["step1"]]))
callModule(moduleMVPlots,"mvImputationPlots_Valid", data=reactive(rv$imputePlotsSteps[["step2"]]))

callModule(moduleDetQuantImpValues, "POV_DetQuantValues_DT", 
           reactive({rv$widgets$proteinImput$POV_detQuant_quantile}), 
           reactive({rv$widgets$proteinImput$POV_detQuant_factor}))

callModule(moduleDetQuantImpValues, "MEC_DetQuantValues_DT", 
           reactive({rv$widgets$proteinImput$MEC_detQuant_quantile}), 
           reactive({rv$widgets$proteinImput$MEC_detQuant_factor}))





observeEvent(input$POV_missing.value.algorithm,{rv$widgets$proteinImput$POV_algorithm <- input$POV_missing.value.algorithm})
observeEvent(input$POV_detQuant_quantile,{rv$widgets$proteinImput$POV_detQuant_quantile <- input$POV_detQuant_quantile})
observeEvent(input$POV_detQuant_factor,{rv$widgets$proteinImput$POV_detQuant_factor <- input$POV_detQuant_factor})
observeEvent(input$KNN_nbNeighbors,{rv$widgets$proteinImput$POV_KNN_n <- input$KNN_nbNeighbors})
observeEvent(input$MEC_missing.value.algorithm,{rv$widgets$proteinImput$MEC_algorithm <- input$MEC_missing.value.algorithm})
observeEvent(input$MEC_detQuant_quantile,{rv$widgets$proteinImput$MEC_detQuant_quantile <- input$MEC_detQuant_quantile})
observeEvent(input$MEC_detQuant_factor,{rv$widgets$proteinImput$MEC_detQuant_factor <- input$MEC_detQuant_factor})
observeEvent(input$MEC_fixedValue,{rv$widgets$proteinImput$MEC_fixedValue <- input$MEC_fixedValue})


##########
#####  UI for the PROTEIN LEVEL Imputation process
##########
output$proteinLevelImputationPanel <- renderUI({
  
  tabsetPanel(
    id = "Imputation_tabSetPanel",
    
    tabPanel("1 - Partially Observed Values",
             value = "Classical_MV",
            splitLayout(cellWidths = c(widthLeftPanel, widthRightPanel),
                         wellPanel(id = "sidebar_Imputation1",
                                   height = "100%",
                                   br(),
                                   uiOutput("sidebar_imputation_step1"),
                                   actionButton("perform.imputationClassical.button",
                                                "Perform imputation", class = actionBtnClass)
                                   
                         ),
                         tagList(
                           uiOutput("ImputationStep1Done"),
                           htmlOutput("helpForImputation"),
                           uiOutput("POV_showDetQuantValues"),
                           moduleMVPlotsUI("mvImputationPlots_MV")
                         )
                         
             )
    ),
    tabPanel("2 - Missing on the Entire Condition",
             value = "MEC_MV",
             #sidebarCustom(),
             splitLayout(cellWidths = c(widthLeftPanel, widthRightPanel),
                         wellPanel(id = "sidebar_Imputation2",
                                   height = "100%",
                                   uiOutput("MEC_chooseImputationMethod"),
                                   uiOutput("MEC_Params"),
                                   actionButton("perform.imputationMEC.button","Perform imputation", class = actionBtnClass)
                         ),
                         tagList(
                           htmlOutput("warningMECImputation"),
                           busyIndicator(WaitMsgCalc,wait = 0),
                           uiOutput("ImputationStep2Done"),
                           uiOutput("MEC_showDetQuantValues")
                           ,moduleMVPlotsUI("mvImputationPlots_MEC")
                           
                         )
             )
    ),
    tabPanel("3 - Validate & save",
             value = "Imputation_ValidateAndSave",
             splitLayout(cellWidths = c(widthLeftPanel, widthRightPanel),
                         wellPanel(id = "sidebar_Imputation3",
                                   height = "100%",
                                   busyIndicator(WaitMsgCalc,wait = 0),
                                   actionButton("ValidImputation","Save imputation", class = actionBtnClass)
                         ),
                         tagList(
                            uiOutput("ImputationSaved")
                         )
             )
    ) # end tabPanel(title = "4 - Validate and Save",
  )
  
  
})




output$POV_showDetQuantValues <- renderUI({
  
  req(rv$widgets$proteinImput$POV_algorithm)
  
  if (rv$widgets$proteinImput$POV_algorithm == 'detQuantile')
  {
   tagList(
     h5("The POV will be imputed by the following values :"),
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





#observeEvent(rv$widgets$proteinImput$POV_algorithm,{
#  updateSelectInput(session, "MEC_missing.value.algorithm", selected = "None")
#})

output$sidebar_imputation_step1 <- renderUI({
  
  req(rv$current.obj)
  
  if (length(grep("Imputed", input$datasets))==0){
    isolate({rv$imputePlotsSteps[["step0"]] <- rv$dataset[[input$datasets]]})
    shinyjs::enable("perform.imputationClassical.button")
    
  } else {
    shinyjs::disable("perform.imputationClassical.button")
  }
  
  if (length(grep("Imputed", input$datasets))==0 && rv$ValidImputationClicked){
    updateSelectInput(session, "POV_missing.value.algorithm", selected= "None")
    isolate({
      rv$imputePlotsSteps[["step1"]] <- NULL
    rv$imputePlotsSteps[["step2"]] <- NULL
    rv$ValidImputationClicked <- FALSE
    })
    
  }
  
  
  
  algo <- imputationAlgorithmsProteins_POV
  
  tagList(
    selectInput("POV_missing.value.algorithm","Algorithm for POV",choices = algo, selected=rv$widgets$proteinImput$POV_algorithm),
    uiOutput("POV_Params")
  )
  
})


output$MEC_chooseImputationMethod <- renderUI({
  algo <- imputationAlgorithmsProteins_MEC
  
  selectInput("MEC_missing.value.algorithm", "Algorithm for MEC", choices = algo,
              selected=rv$widgets$proteinImput$MEC_algorithm)
})






output$POV_Params <- renderUI({
  req(rv$widgets$proteinImput$POV_algorithm)
  
  switch(rv$widgets$proteinImput$POV_algorithm,
         detQuantile = {
           
           tagList(
             #h4("Det quantile parameters"),
             numericInput("POV_detQuant_quantile", "Quantile", 
                          value = rv$widgets$proteinImput$POV_detQuant_quantile, 
                          step=0.5, min=0, max=100),
             numericInput("POV_detQuant_factor", "Factor", 
                          value = rv$widgets$proteinImput$POV_detQuant_factor,
                          step=0.1, min=0, max=10)
           )
         },
         KNN = {
           numericInput("KNN_nbNeighbors", "Nb neighbors", value = rv$widgets$proteinImput$POV_KNN_n, step=1, min=0, max=nrow(rv$current.obj))
           
         }
  )
})



output$MEC_Params <- renderUI({
  req(rv$widgets$proteinImput$MEC_algorithm)
  
  switch (rv$widgets$proteinImput$MEC_algorithm,
          detQuantile = {
            tagList(
              numericInput("MEC_detQuant_quantile", "Quantile", value = rv$widgets$proteinImput$MEC_detQuant_quantile,
                           step=0.5, min=0, max=100),
              numericInput("MEC_detQuant_factor", "Factor", value = rv$widgets$proteinImput$MEC_detQuant_factor, step=0.1, min=0, max=10)
            )
          },
          fixedValue = {
            tagList(
              numericInput("MEC_fixedValue", "Fixed value", value = rv$widgets$proteinImput$MEC_fixedValue, step=0.1, min=0, max=100)
            )
          })
})







observeEvent(input$perform.imputationClassical.button,{
  
  isolate({
    
    rv$MECIndex <-NULL
    rv$current.obj <- rv$imputePlotsSteps[["step0"]]
    nbMVBefore <- length(which(is.na(Biobase::exprs(rv$current.obj))==TRUE))
    rv$MECIndex <- findMECBlock(rv$current.obj)
    busyIndicator(WaitMsgCalc,wait = 0)
    switch(rv$widgets$proteinImput$POV_algorithm,
           slsa = {
             rv$current.obj <- wrapper.impute.slsa(rv$current.obj)
           },
           detQuantile = {
           rv$current.obj <- wrapper.impute.detQuant(rv$current.obj,
                                                       qval = rv$widgets$proteinImput$POV_detQuant_quantile/100,
                                                       factor = rv$widgets$proteinImput$POV_detQuant_factor)
           
           },
           KNN = {
             rv$current.obj <- wrapper.impute.KNN(rv$current.obj , rv$widgets$proteinImput$POV_KNN_n)
           }
    )
    rv$current.obj <- reIntroduceMEC(rv$current.obj, rv$MECIndex)
    nbMVAfter <- length(which(is.na(Biobase::exprs(rv$current.obj))==TRUE))
    rv$nbPOVimputed <- nbMVAfter - nbMVBefore
    
    rv$impute_Step <- 1
    rv$imputePlotsSteps[["step1"]] <- rv$current.obj
    
    updateSelectInput(session, "POV_missing.value.algorithm",  selected = rv$widgets$proteinImput$POV_algorithm)
    updateNumericInput(session,"POV_detQuant_quantile", "Quantile", value = rv$widgets$proteinImput$POV_detQuant_quantile)
    updateNumericInput(session,"POV_detQuant_factor", "Factor", value = rv$widgets$proteinImput$POV_detQuant_factor)
    updateNumericInput(session,"KNN_nbNeighbors",  value = rv$widgets$proteinImput$POV_KNN_n)
    
    shinyjs::enable("perform.imputationMEC.button")
    shinyjs::enable("ValidImputation")
    
  })
})


#################################################################################
#################################################################################
#################################################################################


observeEvent(input$perform.imputationMEC.button,{
  
     
    busyIndicator(WaitMsgCalc,wait = 0)
    rv$current.obj <- reIntroduceMEC(rv$current.obj, rv$MECIndex)
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
    
    updateSelectInput(session,"MEC_missing.value.algorithm",  selected = rv$widgets$proteinImput$MEC_algorithm)
    updateNumericInput(session,"MEC_detQuant_quantile", "Quantile", value = rv$widgets$proteinImput$MEC_detQuant_quantile)
    updateNumericInput(session,"MEC_detQuant_factor", "Factor", value = rv$widgets$proteinImput$MEC_detQuant_factor)
    rv$impute_Step <- 2
    isolate({
      rv$imputePlotsSteps[["step2"]] <- rv$current.obj
    })
 
})







##' -- Validate and Save the imputation ---------------------------------------
##' @author Samuel Wieczorek
observeEvent(input$ValidImputation,{
  
  isolate({
    
    l.params <- rv$widgets$proteinImput
    
    
    name <- paste0("Imputed", ".", rv$typeOfDataset)
    rv$current.obj <- saveParameters(rv$current.obj, name,"proteinImputation",l.params)
    
    rv$dataset[[name]] <- rv$current.obj
    
    updateSelectInput(session, "datasets",
                      #paste("Dataset versions of",rv$current.obj.name, sep=" "),
                      choices = names(rv$dataset),
                      selected = name)
    
    
    updateSelectInput(session, "POV_missing.value.algorithm",  selected = rv$widgets$proteinImput$POV_algorithm)
    updateNumericInput(session,"POV_detQuant_quantile", "Quantile", value = rv$widgets$proteinImput$POV_detQuant_quantile)
    updateNumericInput(session,"POV_detQuant_factor", "Factor", value = rv$widgets$proteinImput$POV_detQuant_factor)
    updateNumericInput(session,"KNN_nbNeighbors",  value = rv$widgets$proteinImput$POV_KNN_n)
    
    updateSelectInput(session,"MEC_missing.value.algorithm",  selected = rv$widgets$proteinImput$MEC_algorithm)
    updateNumericInput(session,"MEC_detQuant_quantile", "Quantile", value = rv$widgets$proteinImput$MEC_detQuant_quantile)
    updateNumericInput(session,"MEC_detQuant_factor", "Factor", value = rv$widgets$proteinImput$MEC_detQuant_factor)
    updateNumericInput(session,"MEC_fixedValue", "Fixed value", value = rv$widgets$proteinImput$MEC_fixedValue)
    
    #shinyjs::disable("perform.imputationClassical.button")
    #shinyjs::disable("perform.imputationMEC.button")
    #shinyjs::disable("ValidImputation")
    rv$ValidImputationClicked <- TRUE
    
    
    ## Add the necessary text to the Rmd file
    #txt2Rmd <- readLines("Rmd_sources/imputation_Rmd.Rmd")
    #filename <- paste(tempdir(), sessionID, 'report.Rmd',sep="/")
    #write(txt2Rmd, file = filename,append = TRUE, sep = "\n")
  })
})






output$ImputationSaved <- renderUI({
  input$datasets
  if (is.null(input$datasets)|| (length(grep("Imputed",input$datasets)) !=1) ) {
    return()  }
  else if (grep("Imputed",input$datasets) == 1 ) {
    h4("The imputed dataset has been saved.")
  }
})

output$ImputationStep1Done <- renderUI({
  rv$impute_Step
  if (rv$impute_Step >= 1) {
    tagList(
      h5(paste0("POV imputation done.", rv$nbPOVimputed, " were imputed")),
      # br(),
      h5("Updated graphs can be seen on tab \"2 - Missing on the Entire Condition\".")
    )
  }
})


output$ImputationStep2Done <- renderUI({
  rv$impute_Step
  if (rv$impute_Step >= 2) {
    tagList(
      h5("MEC imputation done."),
      h5("Updated graphs cans be seen on tab \"3 - Validate and save\"."))
  }
})

output$warningMECImputation<- renderText({
  t <- "<font color=\"red\"><strong>Warning:</strong> Warning: Imputing MEC in a conservative way
  <br>is a real issue as, in the given condition, there is no observed value to rely on.
  <br> Thus, if imputation is not avoidable, imputed MEC must be very cautiously interpreted.</font color=\"red\">"
  HTML(t)
})





output$helpForImputation <- renderText({
  input$missing.value.algorithm
  input$missing.value.basic.algorithm
  rv$typeOfDataset
  
  if (is.null(input$missing.value.algorithm) || (input$missing.value.algorithm == "None")) {return(NULL)}
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




#
# output$progressOne <- renderUI({
#     input$missing.value.algorithm
#     rv$current.obj
#     if (is.null(input$missing.value.algorithm)){return(NULL)}
#     if (!grepl( "imp4p",input$missing.value.algorithm)) {return(NULL)}
#     if (is.null(rv$current.obj)) { return(NULL)}
#
#     tagList(
#                      h5("This may take a while,"),
#                      h5("please be patient ..."),
#                      progressBar2("pb1",value=0, size="sm", color="aqua", striped=TRUE, active=TRUE, label=TRUE)
#     )
# })

