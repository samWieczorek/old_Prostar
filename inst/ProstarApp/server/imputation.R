

callModule(moduleMVPlots,"mvImputationPlots_MV", data=reactive(rv$imputePlotsSteps[["step0"]]))
callModule(moduleMVPlots,"mvImputationPlots_LAPALA", data=reactive(rv$imputePlotsSteps[["step1"]]))
callModule(moduleMVPlots,"mvImputationPlots_Valid", data=reactive(rv$imputePlotsSteps[["step2"]]))



observeEvent(input$ClassicalMV_missing.value.algorithm,{

    rv$impute_Step <- 0
    #if (is.null(input$ClassicalMV_missing.value.algorithm) || input$ClassicalMV_missing.value.algorithm == "None")
    #{
    #shinyjs::disable("perform.imputationClassical.button")
    shinyjs::disable("perform.imputationLAPALA.button")
    shinyjs::disable("ValidImputation")
        updateSelectInput(session, "Lapala_missing.value.algorithm", selected = "None")
    #}
    #else {
       # shinyjs::enable("perform.imputationLAPALA.button")
   # }

})

output$sidebar_imputation_step1 <- renderUI({
    
     rv$current.obj
     if (is.null(rv$current.obj)) {return(NULL)}
    
    m <- NULL
    tag <- rv$current.obj@experimentData@other$classicalMV_imputation.method
    if (!is.null(tag)){ m <- tag}
    
    if (length(grep("Imputed", input$datasets))==0){
        rv$imputePlotsSteps[["step0"]] <- rv$dataset[[input$datasets]]
        shinyjs::enable("perform.imputationClassical.button")
        
    } else {
        shinyjs::disable("perform.imputationClassical.button")
    }
    
    if (length(grep("Imputed", input$datasets))==0 && rv$ValidImputationClicked){
        updateSelectInput(session, "ClassicalMV_missing.value.algorithm", selected= "None")
        rv$imputePlotsSteps[["step1"]] <- NULL
        rv$imputePlotsSteps[["step2"]] <- NULL
        rv$ValidImputationClicked <- FALSE
    }
    
    
    
    algo <- NULL
    switch(rv$typeOfDataset,
           protein = {algo <- imputationAlgorithmsProteins_ClassicalMV},
           peptide = {algo <- imputationAlgorithmsPeptides_ClassicalMV}
    )
    .choice <- rv$current.obj@experimentData@other$Params[["Imputation"]]$POV_algorithm
    tagList(
                selectInput("ClassicalMV_missing.value.algorithm",
                "Choose algorithm for POV",
                choices = names(algo),
                selected = .choice
                # selected = names(which(algo == tag))
    ),
    uiOutput("ClassicalMV_Params")
    )
   
})


output$Lapala_chooseImputationMethod <- renderUI({
    rv$current.obj
    
    #if (is.null(rv$current.obj)) {return(NULL)}
    
    tag <-  rv$current.obj@experimentData@other$Params[["Imputation"]]$MEC_algorithm
    
    algo <- NULL
    switch(rv$typeOfDataset,
           protein = {algo <- imputationAlgorithmsProteins_Lapala},
           peptide = {algo <- imputationAlgorithmsPeptides_Lapala}
    )
    
    selectInput("Lapala_missing.value.algorithm",
                "Choose algorithm for MEC",
                choices = names(algo),
                selected = tag
    )
    
})






output$ClassicalMV_Params <- renderUI({
    
    #input$ClassicalMV_missing.value.algorithm
   if (is.null(input$ClassicalMV_missing.value.algorithm)) {return(NULL)}
    
  switch(input$ClassicalMV_missing.value.algorithm,
    detQuantile = {
            tmp <- rv$current.obj@experimentData@other$Params[["Imputation"]]$POV_detQuant_quantile
            qValue <- ifelse (is.null(tmp), 2.5, tmp)
            
            tmp <- rv$current.obj@experimentData@other$Params[["Imputation"]]$POV_detQuant_factor
            qFactor <- ifelse (is.null(tmp), 1, tmp)
            
            tagList(
                #h4("Det quantile parameters"),
                numericInput("ClassicalMV_detQuant_quantile", "Quantile", value = qValue, step=0.5, min=0, max=100),
                numericInput("ClassicalMV_detQuant_factor", "Factor", value = qFactor, step=0.1, min=0, max=10)
                )
    },
    KNN = {
      tmp <- rv$current.obj@experimentData@other$Params[["Imputation"]]$POV_KNN_n
      n <- ifelse (is.null(tmp), 10, tmp)
      numericInput("KNN_nbNeighbors", "Nb neighbors", value = n, step=1, min=0, max=nrow(rv$current.obj))
        
    }
  )
})



output$Lapala_Params <- renderUI({
    input$Lapala_missing.value.algorithm
    if (is.null(input$Lapala_missing.value.algorithm)) {return(NULL)}
    
    switch (input$Lapala_missing.value.algorithm,
            detQuantile = {
                            tmp <- rv$current.obj@experimentData@other$Params[["Imputation"]]$MEC_detQuant_quantile
                            qValue <- ifelse (is.null(tmp), 2.5, tmp)
              
                          tmp <- rv$current.obj@experimentData@other$Params[["Imputation"]]$MEC_detQuant_factor
                          qFactor <- ifelse (is.null(tmp), 1, tmp)
              
              
                    tagList(
                        numericInput("Lapala_detQuant_quantile", "Quantile", value = qValue, step=0.5, min=0, max=100),
                        numericInput("Lapala_detQuant_factor", "Factor", value = qFactor, step=0.1, min=0, max=10)
                        )
                    },
            fixedValue = {
              tmp <- rv$current.obj@experimentData@other$Params[["Imputation"]]$MEC_fixedValue
              fixVal <- ifelse (is.null(tmp), 0, tmp)
              
              tagList(
                  numericInput("Lapala_fixedValue", "Fixed value", value = fixVal, step=0.1, min=0, max=100)
                  )
            })
})



output$ClassicalMV_detQuant_impValues <- renderUI({
    #rv$current.obj
    input$ClassicalMV_missing.value.algorithm
    #if (is.null(rv$current.obj) ) {return (NULL)}
    if (is.null(input$ClassicalMV_missing.value.algorithm)){return (NULL)}
    
    if (input$ClassicalMV_missing.value.algorithm == 'detQuantile')
        h5("The classical missing values will be imputed by the following values :")

})

output$TAB_ClassicalMV_detQuant_impValues <- renderDataTable({
    #rv$current.obj
    input$ClassicalMV_detQuant_quantile
    input$ClassicalMV_detQuant_factor
    input$ClassicalMV_missing.value.basic.algorithm
   # if (is.null(rv$current.obj) ) {return (NULL)}
    if (is.null(input$ClassicalMV_missing.value.algorithm)){return (NULL)}
    
    
    if (input$ClassicalMV_missing.value.algorithm == 'detQuantile'){

        values <- getQuantile4Imp(Biobase::exprs(rv$current.obj), input$ClassicalMV_detQuant_quantile/100, input$ClassicalMV_detQuant_factor)
        DT::datatable(as.data.frame(t(values$shiftedImpVal)), options = list(dom = 't'))
}
})




output$Lapala_detQuant_impValues <- renderUI({
    #rv$current.obj
    input$Lapala_missing.value.algorithm
    
     #if (is.null(rv$current.obj) ) {return (NULL)}
    if (is.null(input$Lapala_missing.value.algorithm)){return (NULL)}
    
    if (input$Lapala_missing.value.algorithm == 'detQuantile')
        h5("MEC will be imputed as follows:")
    
})

output$TAB_Lapala_detQuant_impValues <- renderDataTable({
    #rv$current.obj
    input$Lapala_detQuant_quantile
    input$Lapala_detQuant_factor
    input$Lapala_missing.value.algorithm
    
    #if (is.null(rv$current.obj) ) {return (NULL)}
    if (is.null(input$Lapala_missing.value.algorithm)){return (NULL)}

    if (input$Lapala_missing.value.algorithm == 'detQuantile'){
        values <- getQuantile4Imp(Biobase::exprs(rv$current.obj), input$Lapala_detQuant_quantile/100, input$Lapala_detQuant_factor)
        DT::datatable(as.data.frame(t(values$shiftedImpVal)), options = list(dom = 't'))
    }
})




observeEvent(input$perform.imputationClassical.button,{
     #input$ClassicalMV_missing.value.algorithm
   # rv$current.obj
    
    isolate({

                rv$lapalaIndex <-NULL
                rv$current.obj <- rv$imputePlotsSteps[["step0"]]
                
                rv$lapalaIndex <- findLapalaBlock(rv$current.obj)
                busyIndicator(WaitMsgCalc,wait = 0)
                switch(input$ClassicalMV_missing.value.algorithm,
                           slsa = {
                               rv$current.obj <- wrapper.impute.slsa(rv$current.obj)
                               },
                           detQuantile = {
                               rv$current.obj <- wrapper.impute.detQuant(rv$current.obj,
                                                                     qval = input$ClassicalMV_detQuant_quantile/100,
                                                                     factor = input$ClassicalMV_detQuant_factor)
                               
                           },
                           KNN = {
                               rv$current.obj <- wrapper.impute.KNN(rv$current.obj , input$KNN_nbNeighbors)
                           }
                    )
                rv$current.obj <- reIntroduceLapala(rv$current.obj, rv$lapalaIndex)
                
                rv$impute_Step <- 1
                rv$imputePlotsSteps[["step1"]] <- rv$current.obj

                
                updateSelectInput(session, "ClassicalMV_missing.value.algorithm",  selected = input$ClassicalMV_missing.value.algorithm)
                updateNumericInput(session,"ClassicalMV_detQuant_quantile", "Quantile", value = input$ClassicalMV_detQuant_quantile)
                updateNumericInput(session,"ClassicalMV_detQuant_factor", "Factor", value = input$ClassicalMV_detQuant_factor)
                updateNumericInput(session,"KNN_nbNeighbors",  value = input$KNN_nbNeighbors)
                
                shinyjs::enable("perform.imputationLAPALA.button")
                shinyjs::enable("ValidImputation")

    })
})


#################################################################################
#################################################################################
#################################################################################


observeEvent(input$perform.imputationLAPALA.button,{
    # input$Lapala_missing.value.algorithm
   # rv$current.obj
    
    isolate({

                busyIndicator(WaitMsgCalc,wait = 0)
                rv$current.obj <- reIntroduceLapala(rv$current.obj, rv$lapalaIndex)
                    switch(input$Lapala_missing.value.algorithm,
                           detQuantile = {
                               rv$current.obj <- wrapper.impute.detQuant(rv$current.obj ,
                                                                         qval = input$Lapala_detQuant_quantile/100,
                                                                         factor = input$Lapala_detQuant_factor)
                           },
                           fixedValue = {
                               rv$current.obj <- wrapper.impute.fixedValue(rv$current.obj,
                                                                           fixVal = input$Lapala_fixedValue)
                           }
                    )

                updateSelectInput(session,"Lapala_missing.value.algorithm",  selected = input$Lapala_missing.value.algorithm)
                updateNumericInput(session,"Lapala_detQuant_quantile", "Quantile", value = input$Lapala_detQuant_quantile)
                updateNumericInput(session,"Lapala_detQuant_factor", "Factor", value = input$Lapala_detQuant_factor)
                rv$impute_Step <- 2
                rv$imputePlotsSteps[["step2"]] <- rv$current.obj

})
    
})







##' -- Validate and Save the imputation ---------------------------------------
##' @author Samuel Wieczorek
observeEvent(input$ValidImputation,{ 
    if (is.null(input$ValidImputation) || (input$ValidImputation == 0)) 
    {return(NULL)}
    
    isolate({
        
                l.params <- list(POV_algorithm = input$ClassicalMV_missing.value.algorithm,
                                 POV_detQuant_quantile = input$ClassicalMV_detQuant_quantile,
                                 POV_detQuant_factor = input$ClassicalMV_detQuant_factor,
                                 POV_KNN_n = input$KNN_nbNeighbors,
                                 MEC_algorithm = input$Lapala_missing.value.algorithm,
                                 MEC_detQuant_quantile = input$Lapala_detQuant_quantile,
                                 MEC_detQuant_factor = input$Lapala_detQuant_factor,
                                 MEC_fixedValue= input$Lapala_fixedValue)

                rv$current.obj <- saveParameters(rv$current.obj, "Imputation",l.params)
                
                name <- paste ("Imputed", " - ", rv$typeOfDataset, sep="")
                
                rv$dataset[[name]] <- rv$current.obj
                #write command log file
                writeToCommandLogFile(
                    paste("dataset[['",name,"']] <- current.obj", sep="")
                )
                
                updateSelectInput(session, "datasets", 
                                  paste("Dataset versions of",rv$current.obj.name, sep=" "),
                                  choices = names(rv$dataset),
                                  selected = name)
                UpdateLog(paste("Imputation with" ,
                                input$missing.value.algorithm,sep=" "),
                          name)
                
                
                updateSelectInput(session, "ClassicalMV_missing.value.algorithm",  selected = input$ClassicalMV_missing.value.algorithm)
                updateNumericInput(session,"ClassicalMV_detQuant_quantile", "Quantile", value = input$ClassicalMV_detQuant_quantile)
                updateNumericInput(session,"ClassicalMV_detQuant_factor", "Factor", value = input$ClassicalMV_detQuant_factor)
                updateNumericInput(session,"KNN_nbNeighbors",  value = input$KNN_nbNeighbors)
                
                updateSelectInput(session,"Lapala_missing.value.algorithm",  selected = input$Lapala_missing.value.algorithm)
                updateNumericInput(session,"Lapala_detQuant_quantile", "Quantile", value = input$Lapala_detQuant_quantile)
                updateNumericInput(session,"Lapala_detQuant_factor", "Factor", value = input$Lapala_detQuant_factor)
                updateNumericInput(session,"Lapala_fixedValue", "Fixed value", value = input$Lapala_fixedValue)
                
                #shinyjs::disable("perform.imputationClassical.button")
                shinyjs::disable("perform.imputationLAPALA.button")
                shinyjs::disable("ValidImputation")
                rv$ValidImputationClicked <- TRUE
                
                
                ## Add the necessary text to the Rmd file
                #txt2Rmd <- readLines("Rmd_sources/imputation_Rmd.Rmd")
                #filename <- paste(tempdir(), sessionID, 'report.Rmd',sep="/")
                #write(txt2Rmd, file = filename,append = TRUE, sep = "\n")
    })
})






output$ImputationSaved <- renderUI({
    input$datasets
   # rv$current.obj
    if (is.null(input$datasets) 
        || (length(grep("Imputed",input$datasets)) !=1) ) {
        return()  }
    else if (grep("Imputed",input$datasets) == 1 ) {
        h4("The imputed dataset has been saved.")
    }
})

output$ImputationStep1Done <- renderUI({
    rv$impute_Step
    if (rv$impute_Step >= 1) {
       tagList(
           h4("Classical missing values imputation done."),
           # br(),
            h4("Updated graphs can be seen on tab \"2 - Missing on the Entire Condition\".")
        )
}
})


output$ImputationStep2Done <- renderUI({
    rv$impute_Step
    if (rv$impute_Step >= 2) {
        tagList(
        h4("MEC imputation done."),
        h4("Updated graphs cans be seen on tab \"3 - Validate and save\"."))
    }
})

output$warningLapalaImputation<- renderText({
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


viewNAbyMean <- function(data){
      req(data)
    #if (is.null(data)) {return(NULL)}
         wrapper.hc_mvTypePlot2(data)
    
}



showImageNA <- function(data){
    req(data)
    #if (is.null(data)) {return(NULL)}
    
    wrapper.mvImage(data)
    
}
