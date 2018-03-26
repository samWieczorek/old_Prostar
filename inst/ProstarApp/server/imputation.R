

callModule(moduleMVPlots,"mvImputationPlots_MV", rv$current.obj)
callModule(moduleMVPlots,"mvImputationPlots_LAPALA", rv$imputePlotsSteps[["step1"]])
callModule(moduleMVPlots,"mvImputationPlots_Valid", rv$imputePlotsSteps[["step2"]])




# observe({
#     input$perform.imputationClassical.button
#     if (is.null(input$perform.imputationClassical.button)) {return(NULL)}
#     shinyjs::enable("perform.imputationLAPALA.button")
#     shinyjs::enable("ValidImputation")
#     
# })
# 

observeEvent(input$ClassicalMV_missing.value.algorithm,{

    rv$impute_Step <- 0
    #if (is.null(input$ClassicalMV_missing.value.algorithm) || input$ClassicalMV_missing.value.algorithm == "None")
    #{
        shinyjs::disable("perform.imputationLAPALA.button")
        shinyjs::disable("ValidImputation")
        updateSelectInput(session, "Lapala_missing.value.algorithm", selected = "None")
    #}
    #else {
       # shinyjs::enable("perform.imputationLAPALA.button")
   # }

})

# 
 # observeEvent(input$Lapala_missing.value.algorithm,{
 #     rv$lapalaIndex
 #     if (is.null(rv$lapalaIndex)) {return(NULL)}
 #     if (is.null(input$Lapala_missing.value.algorithm)){return(NULL)}
 #     rv$current.obj <- reIntroduceLapala(rv$current.obj, rv$lapalaIndex)
 #     
 #     #updateSelectInput(session, "ClassicalMV_missing.value.algorithm",  selected = input$ClassicalMV_missing.value.algorithm)
 #     #updateSelectInput(session, "Lapala_missing.value.algorithm",  selected = input$input$Lapala_missing.value.algorithm)
 # })



output$sidebar_imputation_step1 <- renderUI({
    
     rv$current.obj
     if (is.null(rv$current.obj)) {return(NULL)}
    
    m <- NULL
    tag <- rv$current.obj@experimentData@other$classicalMV_imputation.method
    if (!is.null(tag)){ m <- tag}
    
    algo <- NULL
    switch(rv$typeOfDataset,
           protein = {algo <- imputationAlgorithmsProteins_ClassicalMV},
           peptide = {algo <- imputationAlgorithmsPeptides_ClassicalMV}
    )
    .choice <- NULL
    if(!is.null(input$ClassicalMV_missing.value.algorithm)){.choice <- input$ClassicalMV_missing.value.algorithm}
    tagList(
                selectInput("ClassicalMV_missing.value.algorithm",
                "Choose algorithm for classical MV",
                choices = names(algo),
                selected = .choice
                # selected = names(which(algo == tag))
    ),
    uiOutput("ClassicalMV_Params"),
    actionButton("perform.imputationClassical.button",
                 "Perform classical MV imputation")
)
})


output$ClassicalMV_chooseImputationMethod <- renderUI({
    
})


output$Lapala_chooseImputationMethod <- renderUI({
    rv$current.obj
    
    #if (is.null(rv$current.obj)) {return(NULL)}
    
     m <- NULL
    tag <- rv$current.obj@experimentData@other$classicalMV_imputation.method
    if (!is.null(tag)){ m <- tag}
    
    algo <- NULL
    switch(rv$typeOfDataset,
           protein = {algo <- imputationAlgorithmsProteins_Lapala},
           peptide = {algo <- imputationAlgorithmsPeptides_Lapala}
    )
    
    selectInput("Lapala_missing.value.algorithm",
                "Choose algorithm for Lapala MV",
                choices = names(algo),
                selected = names(which(algo == tag))
    )
    
})






output$ClassicalMV_Params <- renderUI({
    
    #input$ClassicalMV_missing.value.algorithm
   if (is.null(input$ClassicalMV_missing.value.algorithm)) {return(NULL)}
    
  switch(input$ClassicalMV_missing.value.algorithm,
    detQuantile = {
            tagList(
                h4("Det quantile parameters"),
                numericInput("ClassicalMV_detQuant_quantile", "Quantile", value = 2.5, step=0.5, min=0, max=100),
                numericInput("ClassicalMV_detQuant_factor", "Factor", value = 1, step=0.1, min=0, max=10)
                )
    },
    KNN = {
        h4("KNN parameters")
        numericInput("KNN_nbNeighbors", "# neighbors", value = 10, step=1, min=0, max=nrow(rv$current.obj))
        
    }
  )
})



output$Lapala_Params <- renderUI({
    input$Lapala_missing.value.algorithm
    if (is.null(input$Lapala_missing.value.algorithm)) {return(NULL)}
    
    switch (input$Lapala_missing.value.algorithm,
            detQuantile = {
                    tagList(
                        h4("Det quantile parameters"),
                        numericInput("Lapala_detQuant_quantile", "Quantile", value = 2.5, step=0.5, min=0, max=100),
                        numericInput("Lapala_detQuant_factor", "Factor", value = 1, step=0.1, min=0, max=10)
                        )
                    },
            fixedValue = {
                tagList(
                    h4("Fixed value"),
                    numericInput("Lapala_fixedValue", "Fixed value", value = 0, step=0.1, min=0, max=100)
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
        h5("The Lapala will be imputed by the following values :")
    
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
       # result = tryCatch(
    #        {
                rv$lapalaIndex <-NULL
                rv$current.obj <- rv$dataset[[input$datasets]]
                rv$imputePlotsSteps[["step0"]] <- rv$dataset[[input$datasets]]
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
                
                shinyjs::enable("perform.imputationLAPALA.button")
                shinyjs::enable("ValidImputation")
                
                
           # }
            #, warning = function(w) {
            #    print("warning while processing MV imputation step 1")
            #}, error = function(e) {
            #    shinyjs::info(paste("Perform missing values imputation",":",conditionMessage(e), sep=" "))
            #}, finally = {
                #cleanup-code
            #}
            
        #)
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







##' -- Validate the imputation ---------------------------------------
##' @author Samuel Wieczorek
observeEvent(input$ValidImputation,{ 
    if (is.null(input$ValidImputation) || (input$ValidImputation == 0)) 
    {return(NULL)}
    
    isolate({
        
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
                updateSelectInput(session,"Lapala_missing.value.algorithm",  selected = input$Lapala_missing.value.algorithm)
                updateNumericInput(session,"Lapala_detQuant_quantile", "Quantile", value = input$Lapala_detQuant_quantile)
                updateNumericInput(session,"Lapala_detQuant_factor", "Factor", value = input$Lapala_detQuant_factor)
                updateNumericInput(session,"Lapala_fixedValue", "Fixed value", value = input$Lapala_fixedValue)
                
                
                ## Add the necessary text to the Rmd file
                #txt2Rmd <- readLines("Rmd_sources/imputation_Rmd.Rmd")
                #filename <- paste(tempdir(), sessionID, 'report.Rmd',sep="/")
                #write(txt2Rmd, file = filename,append = TRUE, sep = "\n")
    })
})







# histoMV_Image <- reactive({
#     rv$current.obj
#     if (is.null(rv$current.obj)) {return(NULL)}
#     result = tryCatch(
#         {
#             if (!is.null(rv$current.obj)){wrapper.mvHisto_HC(rv$current.obj)}
#         }
#         , warning = function(w) {
#             shinyjs::info(conditionMessage(w))
#         }, error = function(e) {
#             shinyjs::info(paste(match.call()[[1]],":",conditionMessage(e), sep=" "))
#         }, finally = {
#             #cleanup-code 
#         })
#     
# })

# 
# output$histoMV_Image <- renderHighchart({
#    
#     histoMV_Image()
#     
# })



##' xxxxxxxxxxxxxxxxxxxxxxxx
##' @author Samuel Wieczorek
# output$showImageNA <- renderPlot({
#     showImageNA()
# })


##' xxxxxxxxxxxxxxxxxxxxxxxx
##' @author Samuel Wieczorek
# output$showImageNA_LAPALA <- renderPlot({
#     showImageNA()
# })


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
    
    
    t <- "<br> <strong>Lapala</strong> (from French \"là/pas-là\", meaning \"here/not-here\") refers 
    to analytes (peptides or proteins) <br>that are entirely missing in some 
    conditions while they are (partially or totally) <br>visible in others. There 
    specific accounting in a conservative way is a real issue as the imputation <br>
    cannot rely on any observed value in a given condition.
    <br> The parameter \"Upper LAPALA bound\" defines the maximum imputed 
    value as a centile of the observed <br>
    distribution (a tuning between 0% and 10% is advised). <br>
    <font color=\"red\"><strong>Warning:</strong> Imputed lapala values must be very cautiously interpreted.</font color=\"red\">"
    HTML(t)
    

})




###################


# 
# output$warningLapala <- renderUI({
#     input$imp4p_withLapala
#     if (is.null(input$imp4p_withLapala) || (input$imp4p_withLapala == FALSE)){return(NULL)}
#     
#     
#     t <- "<br> <strong>Lapala</strong> (from French \"là/pas-là\", meaning \"here/not-here\") refers 
#         to analytes (peptides or proteins) <br>that are entirely missing in some 
#         conditions while they are (partially or totally) <br>visible in others. There 
#         specific accounting in a conservative way is a real issue as the imputation <br>
#         cannot rely on any observed value in a given condition.
#         <br> The parameter \"Upper LAPALA bound\" defines the maximum imputed 
#         value as a centile of the observed
#         distribution (a tuning between 0% and 10% is advised). <br>
#         Warning: imputed lapala values must be very cautiously interpreted"
#     HTML(t)
# })





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
    #rv$current.obj
    
    if (is.null(data)) {return(NULL)}
    
    isolate({
        result = tryCatch(
            {
                wrapper.hc_mvTypePlot2(data)
            }
            , warning = function(w) {
                shinyjs::info(conditionMessage(w))
            }, error = function(e) {
                shinyjs::info(paste(match.call()[[1]],":",conditionMessage(e), sep=" "))
            }, finally = {
                #cleanup-code 
            })
        
        
    })
    
}



showImageNA <- function(data){
    if(is.null(data)){return()}
    # rv$current.obj
    
    isolate({
        
        #if (is.null(rv$current.obj)) {return(NULL)}
        result = tryCatch(
            {
                wrapper.mvImage(data)
            }
            , warning = function(w) {
                shinyjs::info(conditionMessage(w))
            }, error = function(e) {
                shinyjs::info(paste(match.call()[[1]],":",conditionMessage(e), sep=" "))
            }, finally = {
                #cleanup-code 
            })
        
    })
    
}
##' boxplot of intensities in current.obj
##' @author Samuel Wieczorek
# output$viewNAbyMean <- renderPlot({
#     rv$current.obj
#     viewNAbyMean()
# })




# output$viewNAbyMean_LAPALA <- renderPlot({
#     rv$current.obj
#     viewNAbyMean()
# })

