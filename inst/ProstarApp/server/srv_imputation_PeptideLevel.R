
callModule(moduleMVPlots,"mvImputationPlots_PeptideLevel", data=reactive(rv$current.obj))

##########
#####  UI for the PEPTIDE LEVEL Imputation process
##########
output$peptideLevelImputationPanel <- renderUI({
    
    tabPanel("Miss. values imputation",
             id = "tabPanelImputation",
             value = "imputation",
             sidebarCustom(),
             splitLayout(cellWidths = c(widthLeftPanel, widthRightPanel),
                         wellPanel(id = "sidebar_imputation",
                                   height = "100%"
                                   ,h4("Miss. values imputation options")
                                   ,br(),
                                   selectInput("peptideLevel_missing.value.algorithm",
                                                "Choose algorithm",
                                                choices = names(imputationAlgorithms)),
                                   uiOutput("peptideLevel_chooseBasicImputationMethod"),
                                   uiOutput("peptideLevel_detQuantileParams"),
                                   uiOutput("peptideLevel_MVI_options"),
                                   uiOutput("peptideLevel_MVI_qmin_option"),
                                   uiOutput("peptideLevel_imp4pLAPALA_distribution_option"),
                                   uiOutput("peptideLevel_OnlyLAPALA_qmin_option"),
                                   uiOutput("peptideLevel_OnlyLAPALA_distribution_option"),
                                   actionButton("peptideLevel_perform.imputation.button", "Perform imputation"),
                                   actionButton("peptideLevel_ValidImputation", "Save imputation",styleclass = "primary"),
                                   br(), br(), br()
                                   #uiOutput("warningImputationMethod"),
                                   
                                   ## progress bar
                                   #br(),
                                   #br(),
                                   #uiOutput(outputId = "progressOne")
                         ),
                         tagList(
                             uiOutput("peptideLevel_showImputationPanel"),
                             uiOutput("peptideLevel_detQuant_impValues"),
                             dataTableOutput("peptideLevel_TAB_detQuant_impValues"),
                             busyIndicator(WaitMsgPlot,wait = 0),
                             moduleMVPlotsUI("mvImputationPlots_PeptideLevel")
                         )      
                         
             )
    )
    
})



output$peptideLevel_detQuantileParams <- renderUI({
    rv$current.obj
    input$peptideLevel_missing.value.basic.algorithm
    if (is.null(rv$current.obj) ) {return (NULL)}
    if ((input$peptideLevel_missing.value.algorithm != "Basic methods") 
        || is.null(input$peptideLevel_missing.value.algorithm)
        || is.null(input$peptideLevel_missing.value.basic.algorithm)) {return(NULL)}
    
    if (input$peptideLevel_missing.value.basic.algorithm == "detQuantile"){
        tagList(
            numericInput("peptideLevel_detQuant_quantile", "Quantile", value = 2.5, step=1, min=0, max=100),
            numericInput("peptideLevel_detQuant_factor", "Factor", value = 1, step=1, min=0, max=10)
        )
    }
    
    
    
})



output$peptideLevel_detQuant_impValues <- renderUI({
    rv$current.obj
    input$peptideLevel_detQuant_quantile
    input$peptideLevel_detQuant_factor
    input$peptideLevel_missing.value.basic.algorithm
    if (is.null(rv$current.obj) ) {return (NULL)}
    if (is.null(input$peptideLevel_missing.value.basic.algorithm)){return (NULL)}
    
    if (input$peptideLevel_missing.value.basic.algorithm == 'detQuantile')
        h5("The missing values will be imputed by the following values :")
    
})

output$TAB_detQuant_impValues <- renderDataTable({
    rv$current.obj
    input$peptideLevel_detQuant_quantile
    input$peptideLevel_detQuant_factor
    input$peptideLevel_missing.value.basic.algorithm
    if (is.null(rv$current.obj) ) {return (NULL)}
    if (is.null(input$peptideLevel_missing.value.basic.algorithm)){return (NULL)}
    
    
    values <- getQuantile4Imp(Biobase::exprs(rv$current.obj), input$detQuant_quantile/100, input$detQuant_factor)
    if (input$peptideLevel_missing.value.basic.algorithm == 'detQuantile'){
        DT::datatable(as.data.frame(t(values$shiftedImpVal)), options = list(dom = 't'))
    }
})

output$peptideLevel_MVI_options <- renderUI({
    
    rv$current.obj
    if (is.null(rv$current.obj) ) {return (NULL)}
    if (is.null(input$peptideLevel_missing.value.algorithm)){return (NULL)}
    
    if (input$peptideLevel_missing.value.algorithm == "imp4p"){
        tagList(
            numericInput("peptideLevel_imp4p_nbiter", "Number of iterations", value = 10, step=1, min=1),
            checkboxInput("peptideLevel_imp4p_withLapala", "with MEC", value = FALSE)
        )
    }
    
})



output$peptideLevel_imp4pLAPALA_distribution_option <- renderUI({
    rv$current.obj
    input$peptideLevel_missing.value.algorithm
    input$peptideLevel_imp4p_withLapala
    if (is.null(input$peptideLevel_imp4p_withLapala) ) {return (NULL)}
    if (is.null(rv$current.obj) ) {return (NULL)}
    if (is.null(input$peptideLevel_missing.value.algorithm)){return (NULL)}
    
    if ((input$peptideLevel_missing.value.algorithm == "imp4p") && (input$peptideLevel_imp4p_withLapala == TRUE)){
        radioButtons("peptideLevel_imp4pLAPALA_distrib", "Distribution type", choices = G_imp4PDistributionType_Choices)
    }
    
})




output$peptideLevel_OnlyLAPALA_distribution_option <- renderUI({
    rv$current.obj
    input$peptideLevel_missing.value.basic.algorithm
    input$peptideLevel_missing.value.algorithm
    if (is.null(rv$current.obj) ) {return (NULL)}
    if (is.null(input$peptideLevel_missing.value.algorithm) || is.null(input$peptideLevel_missing.value.basic.algorithm)){return (NULL)}

    if ((input$peptideLevel_missing.value.algorithm == "Basic methods") && (input$peptideLevel_missing.value.basic.algorithm == "dummy censored")){
        radioButtons("peptideLevel_OnlyLAPALA_distrib", "Distribution type", choices = c("unif" = "unif", "beta" = "beta"))
    }

})



output$peptideLevel_OnlyLAPALA_qmin_option <- renderUI({
    rv$current.obj
    input$peptideLevel_missing.value.basic.algorithm
    input$peptideLevel_missing.value.algorithm
    if (is.null(rv$current.obj) ) {return (NULL)}
    if (is.null(input$peptideLevel_missing.value.algorithm) || is.null(input$peptideLevel_missing.value.basic.algorithm)){return (NULL)}

    if ((input$peptideLevel_missing.value.algorithm == "Basic methods") && (input$peptideLevel_missing.value.basic.algorithm == "dummy censored")){
        numericInput("peptideLevel_OnlyLAPALA_qmin", "Upper LAPALA bound", value = 2.5, step=0.1, min=0, max=100)
    }

})




output$peptideLevel_MVI_qmin_option <- renderUI({
    
    rv$current.obj
    if (is.null(rv$current.obj) ) {return (NULL)}
    if (is.null(input$peptideLevel_missing.value.algorithm)){return (NULL)}
    if (is.null(input$peptideLevel_imp4p_withLapala)){return(NULL)}
    
    if ((input$peptideLevel_missing.value.algorithm == "imp4p") && (input$peptideLevel_imp4p_withLapala==TRUE)){
        numericInput("peptideLevel_imp4p_qmin", "Upper lapala bound", value = 2.5, step=0.1, min=0, max=100)
    }
    
})



# 
#------------------------------------------
##' Missing values imputation - reactivity behavior
##' @author Samuel Wieczorek
observeEvent(input$peptideLevel_perform.imputation.button,{
    input$peptideLevel_missing.value.algorithm
    input$peptideLevel_missing.value.basic.algorithm
    input$peptideLevel_imp4p_withLapala
    input$peptideLevel_OnlyLAPALA_qmin
    input$peptideLevel_OnlyLAPALA_distrib
    input$peptideLevel_imp4pLAPALA_distrib
    
    isolate({
        result = tryCatch(
            {
                
                if (input$peptideLevel_missing.value.algorithm == "None"){
                    rv$current.obj <- rv$dataset[[input$datasets]]
                } else {
                    #createPNG_BeforeImputation()
                    
                    if (input$peptideLevel_missing.value.algorithm == "imp4p")
                    {
                        if (input$peptideLevel_imp4p_withLapala) {
                            
                            rv$current.obj <- wrapper.dapar.impute.mi(rv$dataset[[input$datasets]],
                                                                      #eps = input$imp4p_eps,
                                                                      nb.iter = input$peptideLevel_imp4p_nbiter,
                                                                      lapala = input$peptideLevel_imp4p_withLapala,
                                                                      q.min = input$peptideLevel_imp4p_qmin / 100,
                                                                      distribution = as.character(input$peptideLevel_imp4pLAPALA_distrib))
                            #write log command file
                            #if (input$showCommandLog){
                            writeToCommandLogFile(
                                paste("current.obj <- wrapper.dapar.impute.mi(",
                                      "dataset[['",input$datasets,"']], nb.iter=",input$peptideLevel_imp4p_nbiter,
                                      ", lapala = ", input$peptideLevel_imp4p_withLapala, ", q.min = ", input$peptideLevel_imp4p_qmin / 100, ", distribution = ", input$peptideLevel_imp4pLAPALA_distrib, ")",sep=""))
                            #}
                            
                            
                        } else {
                            rv$current.obj <- wrapper.dapar.impute.mi(rv$dataset[[input$datasets]],
                                                                      #eps = input$imp4p_eps,
                                                                      nb.iter = input$peptideLevel_imp4p_nbiter,
                                                                      lapala = input$peptideLevel_imp4p_withLapala)
                            #write log command file
                            #if (input$showCommandLog){
                            writeToCommandLogFile(
                                paste("current.obj <- wrapper.dapar.impute.mi(",
                                      "dataset[['",input$datasets,"']] nb.iter=",input$peptideLevel_imp4p_nbiter,
                                      ", lapala = ", input$peptideLevel_imp4p_withLapala, ")",sep=""))
                            # }
                        }
                        
                        updateSelectInput(session, "peptideLevel_missing.value.algorithm", "peptideLevel_imp4p_nbiter", 
                                          selected = input$peptideLevel_imp4p_nbiter)
                        
                    } else if (input$peptideLevel_missing.value.algorithm == "Basic methods"){
                        if (input$peptideLevel_missing.value.basic.algorithm %in% c("KNN", "MLE")) 
                        {
                            
                            busyIndicator(WaitMsgCalc,wait = 0)
                            rv$current.obj <- wrapper.mvImputation(rv$dataset[[input$datasets]],
                                                                   input$peptideLevel_missing.value.basic.algorithm)
                            
                            #write log command file
                            #if (input$showCommandLog){
                            writeToCommandLogFile(
                                paste("current.obj <- wrapper.mvImputation(",
                                      "dataset[['",input$datasets, "']],'",input$peptideLevel_missing.value.basic.algorithm,"')", sep="")
                            )
                            #}
                            
                        } 
                        else if (input$peptideLevel_missing.value.basic.algorithm ==  "detQuantile")
                        {
                            
                            rv$current.obj <- wrapper.impute.detQuant(rv$dataset[[input$datasets]],
                                                                      qval = (input$peptideLevel_detQuant_quantile/100),
                                                                      factor = input$peptideLevel_detQuant_factor)
                            #write log command file
                            #if (input$showCommandLog){
                            writeToCommandLogFile(
                                paste("current.obj <- wrapper.impute.detQuant(",
                                      "dataset[['", input$datasets,"']])",sep="")
                            )
                            #}
                            
                            
                        }
                    }
                }
                
                updateSelectInput(session,"peptideLevel_missing.value.algorithm",selected = input$peptideLevel_missing.value.algorithm)
                updateSelectInput(session,"peptideLevel_missing.value.basic.algorithm",selected = input$peptideLevel_missing.value.basic.algorithm)
                updateSelectInput(session,"peptideLevel_detQuant_quantile",selected = input$peptideLevel_detQuant_quantile)
                updateSelectInput(session,"peptideLevel_detQuant_factor",selected = input$peptideLevel_detQuant_factor)
                updateSelectInput(session, "missing.value.algorithm", selected = input$peptideLevel_missing.value.algorithm)
                updateSelectInput(session,"missing.value.basic.algorithm", selected = input$peptideLevel_missing.value.basic.algorithm)
                updateSelectInput(session,"peptideLevel_imp4p_withLapala", selected = input$peptideLevel_imp4p_withLapala)
                updateSelectInput(session, "peptideLevel_imp4pLAPALA_distrib", selected = input$peptideLevel_imp4pLAPALA_distrib)
                updateSelectInput(session, "peptideLevel_imp4p_qmin", selected = input$peptideLevel_imp4p_qmin)
                
                
                
                
                #createPNG_AfterImputation()
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
observeEvent(input$peptideLevel_ValidImputation,{ 
    
    input$peptideLevel_missing.value.algorithm
    if (is.null(input$peptideLevel_ValidImputation) || (input$peptideLevel_ValidImputation == 0)) 
    {return(NULL)}
    
    isolate({
        
        result = tryCatch(
            {
                l.params <- list(pepLevel_algorithm = input$peptideLevel_missing.value.algorithm,
                                 pepLevel_basicAlgorithm = input$peptideLevel_missing.value.basic.algorithm,
                                 pepLevel_detQuantile = input$peptideLevel_detQuant_quantile,
                                 pepLevel_detQuant_factor = input$peptideLevel_detQuant_factor,
                                 pepLevel_imp4p_nbiter = input$peptideLevel_imp4p_nbiter,
                                 pepLevel_imp4p_withLapala = input$peptideLevel_imp4p_withLapala,
                                 pepLevel_imp4p_qmin = input$peptideLevel_imp4p_qmin,
                                 pepLevel_imp4pLAPALA_distrib = input$peptideLevel_imp4pLAPALA_distrib)
                
                rv$current.obj <- saveParameters(rv$current.obj, "Imputation",l.params)
                
                name <- paste ("Imputed", " - ", rv$typeOfDataset, sep="")

                rv$dataset[[name]] <- rv$current.obj
                UpdateLog("Imputation", l.params)
                
                
                updateSelectInput(session,"peptideLevel_missing.value.algorithm", selected=input$peptideLevel_missing.value.algorithm)
                updateSelectInput(session,"peptideLevel_missing.value.basic.algorithm", selected=input$peptideLevel_missing.value.basic.algorithm)
                updateNumericInput(session,"peptideLevel_detQuant_quantile", value=input$peptideLevel_detQuant_quantile)
                updateNumericInput(session,"peptideLevel_detQuant_factor", value=input$peptideLevel_detQuant_factor)
                updateNumericInput(session,"peptideLevel_imp4p_nbiter", value=input$peptideLevel_imp4p_nbiter)
                updateCheckboxInput(session, "peptideLevel_imp4p_withLapala", value=input$peptideLevel_imp4p_withLapala)
                updateNumericInput(session,"peptideLevel_imp4p_qmin", value=input$peptideLevel_imp4p_qmin)
                updateRadioButtons(session,"peptideLevel_imp4pLAPALA_distrib", selected=input$peptideLevel_imp4pLAPALA_distrib)
                
                
                
                #write command log file
                writeToCommandLogFile(
                    paste("dataset[['",name,"']] <- current.obj", sep="")
                )
                
                updateSelectInput(session, "datasets", 
                                  paste("Dataset versions of",rv$current.obj.name, sep=" "),
                                  choices = names(rv$dataset),
                                  selected = name)

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







output$peptideLevel_chooseImputationMethod <- renderUI({
    if (is.null(rv$current.obj)) {return(NULL)}
    m <- NULL
    selectInput("peptideLevel_missing.value.algorithm",
                "Choose algorithm",
                choices = names(imputationAlgorithms))

})


output$peptideLevel_chooseBasicImputationMethod <- renderUI({
    input$peptideLevel_missing.value.algorithm
    if (is.null(rv$current.obj)) {return(NULL)}
    if ((input$peptideLevel_missing.value.algorithm != "Basic methods") || is.null(input$peptideLevel_missing.value.algorithm)) {return(NULL)}
    
    selectInput("peptideLevel_missing.value.basic.algorithm",
                "Choose algorithm",
                choices = names(basicMethodsImputationAlgos))
    
})




output$peptideLevel_warningImputationMethod <- renderText({
    input$peptideLevel_missing.value.algorithm
    input$peptideLevel_imp4p_withLapala
    
    if (is.null(input$peptideLevel_missing.value.algorithm)) {return (NULL)}
    
    if (is.null(input$peptideLevel_imp4p_withLapala) || (input$peptideLevel_imp4p_withLapala == FALSE)){return(NULL)}
    
    var <- ((input$peptideLevel_missing.value.algorithm == "imp4p") && (input$peptideLevel_imp4p_withLapala == TRUE)) ||
        (input$peptideLevel_missing.value.basic.algorithm ==  "dummy censored")
    
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
        shinyjs::disable("peptideLevel_perform.imputation.button")
        shinyjs::disable("peptideLevel_ValidImputation")
    } else {
        shinyjs::enable("peptideLevel_perform.imputation.button")
        shinyjs::enable("peptideLevel_ValidImputation")
    }
})



output$peptideLevel_showImputationPanel <- renderUI({
    rv$current.obj
    
    if (is.null(rv$current.obj)) {return (NULL)}
    
    nbEmptyLines <- getNumberOfEmptyLines(exprs(rv$current.obj))
    if (nbEmptyLines == 0)
    {
        tagList(
            htmlOutput("peptideLevel_helpForImputation"),
            htmlOutput("peptideLevel_warningImputationMethod")
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





output$peptideLevel_helpForImputation <- renderText({
    input$peptideLevel_missing.value.algorithm
    input$peptideLevel_missing.value.basic.algorithm
    rv$typeOfDataset
    
    if (is.null(input$peptideLevel_missing.value.algorithm) || (input$peptideLevel_missing.value.algorithm == "None")) {return(NULL)}
    if ((input$peptideLevel_missing.value.algorithm == "Basic methods") && is.null(input$peptideLevel_missing.value.basic.algorithm == "None")) {return(NULL)}
    
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
    
    
    if (input$peptideLevel_missing.value.algorithm == "Basic methods") {
        name <- input$peptideLevel_missing.value.basic.algorithm}
    else {name <- input$peptideLevel_missing.value.algorithm}
    
    if (!is.null(name)) {
        HTML(helpTextImputation[[name]])
        
    }
})





output$peptideLevel_progressOne <- renderUI({
    input$peptideLevel_missing.value.algorithm
    rv$current.obj
    if (is.null(input$peptideLevel_missing.value.algorithm)){return(NULL)}
    if (!grepl( "imp4p",input$peptideLevel_missing.value.algorithm)) {return(NULL)}
    if (is.null(rv$current.obj)) { return(NULL)}
    
    tagList(
        h5("This may take a while,"),
        h5("please be patient ..."),
        progressBar2("peptideLevel_pb1",value=0, size="sm", color="aqua", striped=TRUE, active=TRUE, label=TRUE)
    )
})

