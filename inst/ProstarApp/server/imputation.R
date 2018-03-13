



observeEvent(input$ClassicalMV_missing.value.algorithm,{

    rv$current.obj <- rv$dataset[[input$datasets]]
    if (is.null(input$ClassicalMV_missing.value.algorithm) || input$ClassicalMV_missing.value.algorithm == "None")
    {
        shinyjs::disable("perform.imputationLAPALA.button")
    }
    else {
        shinyjs::enable("perform.imputationLAPALA.button")
    }
    updateSelectInput(session, "ClassicalMV_missing.value.algorithm",  selected = input$ClassicalMV_missing.value.algorithm)

})

# 
 observeEvent(input$Lapala_missing.value.algorithm,{
     input$imputeLapala
     rv$lapalaIndex
     if (is.null(rv$lapalaIndex)) {return(NULL)}
     if (is.null(input$imputeLapala) || !input$imputeLapala) {return(NULL)}
     if (is.null(input$Lapala_missing.value.algorithm)){return(NULL)}
     rv$current.obj <- reIntroduceLapala(rv$current.obj, rv$lapalaIndex)
     
     updateCheckboxInput(session,"imputeLapala", value = input$imputeLapala)
     updateSelectInput(session, "ClassicalMV_missing.value.algorithm",  selected = input$ClassicalMV_missing.value.algorithm)
     #updateSelectInput(session, "Lapala_missing.value.algorithm",  selected = input$input$Lapala_missing.value.algorithm)
 })



output$ClassicalMV_chooseImputationMethod <- renderUI({
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
    
    selectInput("ClassicalMV_missing.value.algorithm",
                "Choose algorithm for classical MV",
                choices = names(algo),
                selected = names(which(algo == tag))
    )
    
})


output$Lapala_chooseImputationMethod <- renderUI({
    rv$current.obj
    
    if (is.null(rv$current.obj)) {return(NULL)}
    
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
  rv$current.obj
    input$ClassicalMV_missing.value.algorithm
  if (is.null(rv$current.obj) ) {return (NULL)}
    if (is.null(input$ClassicalMV_missing.value.algorithm)) {return(NULL)}
    
  switch(input$ClassicalMV_missing.value.algorithm,
    detQuantile = {
            tagList(
                h4("Det quantile parameters"),
                numericInput("ClassicalMV_detQuant_quantile", "Quantile", value = 2.5, step=1, min=0, max=100),
                numericInput("ClassicalMV_detQuant_factor", "Factor", value = 1, step=1, min=0, max=10)
                )
    }
  )
})



output$Lapala_Params <- renderUI({
    rv$current.obj
    input$Lapala_missing.value.algorithm

    if (is.null(rv$current.obj) ) {return (NULL)}
    if (is.null(input$Lapala_missing.value.algorithm)) {return(NULL)}
    
    switch (input$Lapala_missing.value.algorithm,
            detQuantile = {
                    tagList(
                        h4("Det quantile parameters"),
                        numericInput("Lapala_detQuant_quantile", "Quantile", value = 2.5, step=1, min=0, max=100),
                        numericInput("Lapala_detQuant_factor", "Factor", value = 1, step=1, min=0, max=10)
                        )
                    },
            fixedValue = {
                tagList(
                    #h4("Fixed value"),
                    numericInput("Lapala_fixedValue", "Fixed value", value = 2.5, step=1, min=0, max=100)
                    )
                 })
})






output$ClassicalMV_detQuant_impValues <- renderUI({
    rv$current.obj
    input$ClassicalMV_detQuant_quantile
    input$ClassicalMV_detQuant_factor
    input$ClassicalMV_missing.value.algorithm
    if (is.null(rv$current.obj) ) {return (NULL)}
    if (is.null(input$ClassicalMV_missing.value.algorithm)){return (NULL)}
    
    if (input$ClassicalMV_missing.value.algorithm == 'detQuantile')
        h5("The classical missing values will be imputed by the following values :")

})

output$TAB_ClassicalMV_detQuant_impValues <- renderDataTable({
    rv$current.obj
    input$ClassicalMV_detQuant_quantile
    input$ClassicalMV_detQuant_factor
    input$ClassicalMV_missing.value.basic.algorithm
    if (is.null(rv$current.obj) ) {return (NULL)}
    if (is.null(input$ClassicalMV_missing.value.algorithm)){return (NULL)}
    
    
    if (input$ClassicalMV_missing.value.algorithm == 'detQuantile'){

        values <- getQuantile4Imp(Biobase::exprs(rv$current.obj), input$ClassicalMV_detQuant_quantile/100, input$ClassicalMV_detQuant_factor)
        DT::datatable(as.data.frame(t(values$shiftedImpVal)), options = list(dom = 't'))

}
})




output$Lapala_detQuant_impValues <- renderUI({
    rv$current.obj
    input$Lapala_detQuant_quantile
    input$Lapala_detQuant_factor
    input$Lapala_missing.value.algorithm
    
     if (is.null(rv$current.obj) ) {return (NULL)}
    if (is.null(input$Lapala_missing.value.algorithm)){return (NULL)}
    
    if (input$Lapala_missing.value.algorithm == 'detQuantile')
        h5("The Lapala will be imputed by the following values :")
    
})

output$TAB_Lapala_detQuant_impValues <- renderDataTable({
    rv$current.obj
    input$Lapala_detQuant_quantile
    input$Lapala_detQuant_factor
    input$Lapala_missing.value.algorithm
    
    if (is.null(rv$current.obj) ) {return (NULL)}
    if (is.null(input$Lapala_missing.value.algorithm)){return (NULL)}
   
    
    
    if (input$Lapala_missing.value.algorithm == 'detQuantile'){
        values <- getQuantile4Imp(Biobase::exprs(rv$current.obj), input$Lapala_detQuant_quantile/100, input$Lapala_detQuant_factor)
        DT::datatable(as.data.frame(t(values$shiftedImpVal)), options = list(dom = 't'))
    }
})




observeEvent(input$perform.imputationClassical.button,{
    input$Lapala_missing.value.algorithm
    input$ClassicalMV_missing.value.algorithm
    rv$current.obj
    
    isolate({
        result = tryCatch(
            {
                rv$lapalaIndex <-NULL
                rv$current.obj <- rv$dataset[[input$datasets]]
                    
                    
                busyIndicator(WaitMsgCalc,wait = 0)
                switch(input$ClassicalMV_missing.value.algorithm,
                           slsa = {
                               rv$lapalaIndex <- findLapalaBlock(rv$current.obj)
                               rv$current.obj <- wrapper.impute.slsa(rv$current.obj)
                               rv$current.obj <- reIntroduceLapala(rv$current.obj, rv$lapalaIndex)
                           },
                           detQuantile = {
                               rv$lapalaIndex <- findLapalaBlock(rv$current.obj)
                               rv$current.obj <- wrapper.impute.detQuant(rv$current.obj,
                                                                     qval = input$ClassicalMV_detQuant_quantile/100,
                                                                     factor = input$ClassicalMV_detQuant_factor)
                               rv$current.obj <- reIntroduceLapala(rv$current.obj, rv$lapalaIndex)
                           },
                           KNN = {
                               
                           }
                    )

                
                updateSelectInput(session, "ClassicalMV_missing.value.algorithm",  selected = input$ClassicalMV_missing.value.algorithm)
                #updateSelectInput(session,"Lapala_missing.value.algorithm",  selected = input$Lapala_missing.value.algorithm)
               shinyjs::enable("perform.imputationLAPALA.button")

            }
            , warning = function(w) {
                print(w)
            }, error = function(e) {
                shinyjs::info(paste("Perform missing values imputation",":",conditionMessage(e), sep=" "))
            }, finally = {
                #cleanup-code
            }
            
        )
    })
})


#################################################################################
#################################################################################
#################################################################################


observeEvent(input$perform.imputationLAPALA.button,{
    input$Lapala_missing.value.algorithm
    input$ClassicalMV_missing.value.algorithm
    input$Lapala_fixedValue
    rv$current.obj
    
    isolate({
        result = tryCatch(
            {
               
                busyIndicator(WaitMsgCalc,wait = 0)
                
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

                #updateSelectInput(session, "ClassicalMV_missing.value.algorithm",  selected = input$ClassicalMV_missing.value.algorithm)
                updateSelectInput(session,"Lapala_missing.value.algorithm",  selected = input$Lapala_missing.value.algorithm)
                
                
            }
            , warning = function(w) {
                print(w)
            }, error = function(e) {
                shinyjs::info(paste("Perform missing values imputation",":",conditionMessage(e), sep=" "))
            }, finally = {
                #cleanup-code
            }
            
        )
    })
})







##' -- Validate the imputation ---------------------------------------
##' @author Samuel Wieczorek
observeEvent(input$ValidImputation,{ 
    
    input$missing.value.algorithm
    if (is.null(input$ValidImputation) || (input$ValidImputation == 0)) 
    {return(NULL)}
    
    isolate({
        
        result = tryCatch(
            {
                
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
                
                
                ## Add the necessary text to the Rmd file
                #txt2Rmd <- readLines("Rmd_sources/imputation_Rmd.Rmd")
                #filename <- paste(tempdir(), sessionID, 'report.Rmd',sep="/")
                #write(txt2Rmd, file = filename,append = TRUE, sep = "\n")
            }
            , warning = function(w) {
                shinyjs::info(conditionMessage(w))
            }, error = function(e) {
                shinyjs::info(paste("Validate the imputation",":",conditionMessage(e), sep=" "))
            }, finally = {
                #cleanup-code 
            })
        
        
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


showImageNA <- reactive({
    rv$current.obj
    
    isolate({
        
        if (is.null(rv$current.obj)) {return(NULL)}
        result = tryCatch(
            {
                wrapper.mvImage(rv$current.obj)
            }
            , warning = function(w) {
                shinyjs::info(conditionMessage(w))
            }, error = function(e) {
                shinyjs::info(paste(match.call()[[1]],":",conditionMessage(e), sep=" "))
            }, finally = {
                #cleanup-code 
            })
        
    })
    
})

##' xxxxxxxxxxxxxxxxxxxxxxxx
##' @author Samuel Wieczorek
output$showImageNA <- renderPlot({
    showImageNA()
})


##' xxxxxxxxxxxxxxxxxxxxxxxx
##' @author Samuel Wieczorek
output$showImageNA_LAPALA <- renderPlot({
    showImageNA()
})


output$ImputationSaved <- renderUI({
    input$datasets
    rv$current.obj
    if (is.null(input$datasets) 
        || (length(grep("Imputed",input$datasets)) !=1) ) {
        return()  }
    else if (grep("Imputed",input$datasets) == 1 ) {
        h4("The imputed dataset has been saved.")
    }
})




output$warningImputationMethod <- renderText({
    input$missing.value.algorithm
    input$imp4p_withLapala
    
    if (is.null(input$missing.value.algorithm)) {return (NULL)}
    
    if (is.null(input$imp4p_withLapala) || (input$imp4p_withLapala == FALSE)){return(NULL)}
    
    var <- ((input$missing.value.algorithm == "imp4p") && (input$imp4p_withLapala == TRUE)) ||
        (input$missing.value.basic.algorithm ==  "dummy censored")
    
    if (var){
    t <- "<br> <strong>Lapala</strong> (from French \"là/pas-là\", meaning \"here/not-here\") refers 
    to analytes (peptides or proteins) <br>that are entirely missing in some 
    conditions while they are (partially or totally) <br>visible in others. There 
    specific accounting in a conservative way is a real issue as the imputation <br>
    cannot rely on any observed value in a given condition.
    <br> The parameter \"Upper LAPALA bound\" defines the maximum imputed 
    value as a centile of the observed <br>
    distribution (a tuning between 0% and 10% is advised). <br>
    <font color=\"red\"><strong>Warning:</strong> Imputed lapala values must be very cautiously interpreted.</font color=\"red\">"
    HTML(t)}
    
    
    # if (input$missing.value.algorithm == "imp4p with LAPALA")
    #     {
    #     text <- "<font color=\"red\"> Warning ! <br> You are about to impute the <br> LAPALA with small 
    #     arbitrary values. <br> This is not an optimal way <br> 
    #     to impute such values. <br> 
    #     You do it at your own risk."
    #      HTML(text)
    # } else if (input$missing.value.algorithm == "dummy censored") {
    #     text <- "<font color=\"red\"> Warning ! <br> You are about to impute the LAPALA with small 
    #     arbitrary values. This is not an optimal way to impute such values. 
    #     You do it at your own risk."
    #     HTML(text)
    # }
    
})


observe({
    rv$current.obj
    if (is.null(rv$current.obj)) {return(NULL)}
        
    nbEmptyLines <- getNumberOfEmptyLines(Biobase::exprs(rv$current.obj))
    if (nbEmptyLines > 0) {
        shinyjs::disable("perform.imputationClassical.button")
        shinyjs::disable("perform.imputationLAPALA.button")
        shinyjs::disable("ValidImputation")
    } else {
        shinyjs::enable("perform.imputationClassical.button")
        shinyjs::enable("perform.imputationLAPALA.button")
        shinyjs::enable("ValidImputation")
    }
})



observe({
    input$perform.imputationClassical.button
    if (is.null(input$perform.imputationClassical.button)) {return(NULL)}
    shinyjs::enable("perform.imputationLAPALA.button")
    
    
})

output$showImputationPanel <- renderUI({
    rv$current.obj
    
    if (is.null(rv$current.obj)) {return (NULL)}
    
    nbEmptyLines <- getNumberOfEmptyLines(exprs(rv$current.obj))
    if (nbEmptyLines == 0)
    {
        tagList(
            htmlOutput("helpForImputation"),
            htmlOutput("warningImputationMethod")
        )

    }
    else{
        text <- "<br> <br> <font color=\"red\">
        Warning ! Your dataset contains empty lines so that the imputation cannot be proceed.
        <br> <br> Please filter your data first."
        HTML(text)
    }
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





output$progressOne <- renderUI({
    input$missing.value.algorithm
    rv$current.obj
    if (is.null(input$missing.value.algorithm)){return(NULL)}
    if (!grepl( "imp4p",input$missing.value.algorithm)) {return(NULL)}
    if (is.null(rv$current.obj)) { return(NULL)}
    
    tagList(
                     h5("This may take a while,"),
                     h5("please be patient ..."),
                     progressBar2("pb1",value=0, size="sm", color="aqua", striped=TRUE, active=TRUE, label=TRUE)
    )
})


viewNAbyMean <- reactive({
    rv$current.obj
    
    if (is.null(rv$current.obj)) {return(NULL)}
    
    isolate({
        result = tryCatch(
            {
                wrapper.mvTypePlot(rv$current.obj)
            }
            , warning = function(w) {
                shinyjs::info(conditionMessage(w))
            }, error = function(e) {
                shinyjs::info(paste(match.call()[[1]],":",conditionMessage(e), sep=" "))
            }, finally = {
                #cleanup-code 
            })
        
        
    })
    
})

##' boxplot of intensities in current.obj
##' @author Samuel Wieczorek
output$viewNAbyMean <- renderPlot({
    rv$current.obj
    viewNAbyMean()
})




output$viewNAbyMean_LAPALA <- renderPlot({
    rv$current.obj
    viewNAbyMean()
})

