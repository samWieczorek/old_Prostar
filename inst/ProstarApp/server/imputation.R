output$detQuantileParams <- renderUI({
  rv$current.obj
    input$missing.value.basic.algorithm
  if (is.null(rv$current.obj) ) {return (NULL)}
    if ((input$missing.value.algorithm != "Basic methods") 
        || is.null(input$missing.value.algorithm)
        || is.null(input$missing.value.basic.algorithm)) {return(NULL)}
    
  if (input$missing.value.basic.algorithm == "detQuantile"){
    tagList(
      h4("Det quantile parameters"),
      numericInput("detQuant_quantile", "Quantile", value = 2.5, step=1, min=0, max=100),
      numericInput("detQuant_factor", "Factor", value = 1, step=1, min=0, max=10)
    )
  }
  
  
  
})



output$detQuant_impValues <- renderUI({
    rv$current.obj
    input$detQuant_quantile
    input$detQuant_factor
    input$missing.value.basic.algorithm
    if (is.null(rv$current.obj) ) {return (NULL)}
    if (is.null(input$missing.value.basic.algorithm)){return (NULL)}
    
    if (input$missing.value.basic.algorithm == 'detQuantile')
        h5("The missing values will be imputed by the following values :")

})

output$TAB_detQuant_impValues <- renderDataTable({
    rv$current.obj
    input$detQuant_quantile
    input$detQuant_factor
    input$missing.value.basic.algorithm
    if (is.null(rv$current.obj) ) {return (NULL)}
    if (is.null(input$missing.value.basic.algorithm)){return (NULL)}
    
    
    values <- getQuantile4Imp(Biobase::exprs(rv$current.obj), input$detQuant_quantile/100, input$detQuant_factor)
    if (input$missing.value.basic.algorithm == 'detQuantile'){
    DT::datatable(as.data.frame(t(values$shiftedImpVal)), options = list(dom = 't'))
}
})

output$MVI_options <- renderUI({
    
    rv$current.obj
    if (is.null(rv$current.obj) ) {return (NULL)}
    if (is.null(input$missing.value.algorithm)){return (NULL)}
    
    if (input$missing.value.algorithm == "imp4p"){
        tagList(
            h4("imp4p options"),
            numericInput("imp4p_nbiter", "Number of iterations", value = 10, step=1, min=1),
            checkboxInput("imp4p_withLapala", "with Lapala", value = FALSE)
        )
    }
    
})



output$imp4pLAPALA_distribution_option <- renderUI({
    rv$current.obj
    input$missing.value.algorithm
    input$imp4p_withLapala
    if (is.null(input$imp4p_withLapala) ) {return (NULL)}
    if (is.null(rv$current.obj) ) {return (NULL)}
    if (is.null(input$missing.value.algorithm)){return (NULL)}
    
    if ((input$missing.value.algorithm == "imp4p") && (input$imp4p_withLapala == TRUE)){
        radioButtons("imp4pLAPALA_distrib", "Distribution type", choices = G_imp4PDistributionType_Choices)
    }
    
})



# 
# output$OnlyLAPALA_distribution_option <- renderUI({
#     rv$current.obj
#     input$missing.value.basic.algorithm
#     input$missing.value.algorithm
#     if (is.null(rv$current.obj) ) {return (NULL)}
#     if (is.null(input$missing.value.algorithm) || is.null(input$missing.value.basic.algorithm)){return (NULL)}
#     
#     if ((input$missing.value.algorithm == "Basic methods") && (input$missing.value.basic.algorithm == "dummy censored")){
#         radioButtons("OnlyLAPALA_distrib", "Distribution type", choices = c("unif" = "unif", "beta" = "beta"))
#     }
#     
# })


# 
# output$OnlyLAPALA_qmin_option <- renderUI({
#     rv$current.obj
#     input$missing.value.basic.algorithm
#     input$missing.value.algorithm
#     if (is.null(rv$current.obj) ) {return (NULL)}
#     if (is.null(input$missing.value.algorithm) || is.null(input$missing.value.basic.algorithm)){return (NULL)}
#     
#     if ((input$missing.value.algorithm == "Basic methods") && (input$missing.value.basic.algorithm == "dummy censored")){
#         numericInput("OnlyLAPALA_qmin", "Upper LAPALA bound", value = 2.5, step=0.1, min=0, max=100)
#     }
#     
# })




output$MVI_qmin_option <- renderUI({
    
    rv$current.obj
    if (is.null(rv$current.obj) ) {return (NULL)}
    if (is.null(input$missing.value.algorithm)){return (NULL)}
    if (is.null(input$imp4p_withLapala)){return(NULL)}
    
    if ((input$missing.value.algorithm == "imp4p") && (input$imp4p_withLapala==TRUE)){
        numericInput("imp4p_qmin", "Upper lapala bound", value = 2.5, step=0.1, min=0, max=100)
    }
    
})



# 
#------------------------------------------
##' Missing values imputation - reactivity behavior
##' @author Samuel Wieczorek
observeEvent(input$perform.imputation.button,{
    input$missing.value.algorithm
    input$missing.value.basic.algorithm
    input$imp4p_withLapala
    input$OnlyLAPALA_qmin
    input$OnlyLAPALA_distrib
    input$imp4pLAPALA_distrib
    
    isolate({
        result = tryCatch(
            {
                
                if (input$missing.value.algorithm == "None"){
                    rv$current.obj <- rv$dataset[[input$datasets]]
                } else {
                    #createPNG_BeforeImputation()
                    
                    if (input$missing.value.algorithm == "imp4p")
                {
                    if (input$imp4p_withLapala) {
                        
                        rv$current.obj <- wrapper.dapar.impute.mi(rv$dataset[[input$datasets]],
                                                                  #eps = input$imp4p_eps,
                                                                  nb.iter = input$imp4p_nbiter,
                                                                  lapala = input$imp4p_withLapala,
                                                                  q.min = input$imp4p_qmin / 100,
                                                                  distribution = as.character(input$imp4pLAPALA_distrib))
                        #write log command file
                        #if (input$showCommandLog){
                            writeToCommandLogFile(
                            paste("current.obj <- wrapper.dapar.impute.mi(",
                                  "dataset[['",input$datasets,"']], nb.iter=",input$imp4p_nbiter,
                                  ", lapala = ", input$imp4p_withLapala, ", q.min = ", input$imp4p_qmin / 100, ", distribution = ", input$imp4pLAPALA_distrib, ")",sep=""))
                        #}
                        
                        updateSelectInput(session, 
                                          "imp4p_withLapala", 
                                          selected = input$imp4p_withLapala)
                        updateSelectInput(session, 
                                          "imp4pLAPALA_distrib", 
                                          selected = input$imp4pLAPALA_distrib)
                        
                        updateSelectInput(session, 
                                          "imp4p_qmin", 
                                          selected = input$imp4p_qmin)
                        
                        } else {
                        rv$current.obj <- wrapper.dapar.impute.mi(rv$dataset[[input$datasets]],
                                                                  #eps = input$imp4p_eps,
                                                                  nb.iter = input$imp4p_nbiter,
                                                                  lapala = input$imp4p_withLapala)
                        #write log command file
                        #if (input$showCommandLog){
                            writeToCommandLogFile(
                            paste("current.obj <- wrapper.dapar.impute.mi(",
                                  "dataset[['",input$datasets,"']] nb.iter=",input$imp4p_nbiter,
                                  ", lapala = ", input$imp4p_withLapala, ")",sep=""))
                       # }
                    }
                    
                    updateSelectInput(session, 
                                      "missing.value.algorithm", 
                                      selected = input$missing.value.algorithm)
                    updateSelectInput(session, 
                                      "imp4p_nbiter", 
                                      selected = input$imp4p_nbiter)
                    
                } else if (input$missing.value.algorithm == "Basic methods"){
                    if (input$missing.value.basic.algorithm %in% c("KNN", "MLE")) 
                    {
                        
                        busyIndicator(WaitMsgCalc,wait = 0)
                        rv$current.obj <- wrapper.mvImputation(rv$dataset[[input$datasets]],
                                                               input$missing.value.basic.algorithm)
                        
                        #write log command file
                        #if (input$showCommandLog){
                            writeToCommandLogFile(
                            paste("current.obj <- wrapper.mvImputation(",
                                  "dataset[['",input$datasets, "']],'",input$missing.value.basic.algorithm,"')", sep="")
                        )
                        #}
                        
                        updateSelectInput(session, "missing.value.algorithm", 
                                          selected = input$missing.value.algorithm)
                        updateSelectInput(session,"missing.value.basic.algorithm",
                                          selected = input$missing.value.basic.algorithm)
                        
                    } 
                    else if (input$missing.value.basic.algorithm ==  "detQuantile")
                    {

                        rv$current.obj <- wrapper.impute.detQuant(rv$dataset[[input$datasets]],
                                                             qval = (input$detQuant_quantile/100),
                                                             factor = input$detQuant_factor)
                        #write log command file
                        #if (input$showCommandLog){
                            writeToCommandLogFile(
                            paste("current.obj <- wrapper.impute.detQuant(",
                                  "dataset[['", input$datasets,"']])",sep="")
                        )
                        #}

                        updateSelectInput(session,
                                          "missing.value.algorithm",
                                          selected = input$missing.value.algorithm)
                        updateSelectInput(session,"missing.value.basic.algorithm",
                                          selected = input$missing.value.basic.algorithm)
                        updateSelectInput(session,"detQuant_quantile",
                                          selected = input$detQuant_quantile)
                        updateSelectInput(session,"detQuant_factor",
                                          selected = input$detQuant_factor)

                    }
                }
                }
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







output$chooseImputationMethod <- renderUI({
    if (is.null(rv$current.obj)) {return(NULL)}
    m <- NULL
    tag <- rv$current.obj@experimentData@other$imputation.method
    if (!is.null(tag)){ m <- tag}
    selectInput("missing.value.algorithm",
                "Choose algorithm",
                choices = names(imputationAlgorithms),
                selected = names(which(imputationAlgorithms == tag))
    )
    
})


output$chooseBasicImputationMethod <- renderUI({
    input$missing.value.algorithm
    if (is.null(rv$current.obj)) {return(NULL)}
    if ((input$missing.value.algorithm != "Basic methods") || is.null(input$missing.value.algorithm)) {return(NULL)}
    
    selectInput("missing.value.basic.algorithm",
                "Choose algorithm",
                choices = names(basicMethodsImputationAlgos)
                #, selected = names(which(basicMethodsImputationAlgos == tag))
    )
    
})




histoMV_Image <- reactive({
    rv$current.obj
    if (is.null(rv$current.obj)) {return(NULL)}
    result = tryCatch(
        {
            if (!is.null(rv$current.obj)){wrapper.mvHisto_HC(rv$current.obj)}
        }
        , warning = function(w) {
            shinyjs::info(conditionMessage(w))
        }, error = function(e) {
            shinyjs::info(paste(match.call()[[1]],":",conditionMessage(e), sep=" "))
        }, finally = {
            #cleanup-code 
        })
    
})


output$histoMV_Image <- renderHighchart({
   
    histoMV_Image()
    
})


showImageNA <- reactive({
    rv$current.obj
    #input$toto
    
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
        shinyjs::disable("perform.imputation.button")
        shinyjs::disable("ValidImputation")
    } else {
        shinyjs::enable("perform.imputation.button")
        shinyjs::enable("ValidImputation")
    }
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
    
    viewNAbyMean()
})





