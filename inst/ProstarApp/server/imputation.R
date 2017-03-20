
output$MVI_options <- renderUI({
    
    rv$current.obj
    if (is.null(rv$current.obj) ) {return (NULL)}
    if (is.null(input$missing.value.algorithm)){return (NULL)}
    
    if (input$missing.value.algorithm == "imp4p"){
        conditionalPanel(
            condition = 'true',
            h4("imp4p options"),
            numericInput("imp4p_nbiter", "Number of iterations", value = 10, step=1, min=1),
            checkboxInput("imp4p_withLapala", "with Lapala", value = FALSE)
        )
    }
    
})


output$OnlyLAPALA_distribution_option <- renderUI({
    rv$current.obj
    input$missing.value.basic.algorithm
    input$missing.value.algorithm
    if (is.null(rv$current.obj) ) {return (NULL)}
    if (is.null(input$missing.value.algorithm) || is.null(input$missing.value.basic.algorithm)){return (NULL)}
    
    if ((input$missing.value.algorithm == "Basic methods") && (input$missing.value.basic.algorithm == "dummy censored")){
        radioButtons("OnlyLAPALA_distrib", "Distribution type", choices = c("unif" = "unif", "beta" = "beta"))
    }
    
})



output$OnlyLAPALA_qmin_option <- renderUI({
    rv$current.obj
    input$missing.value.basic.algorithm
    input$missing.value.algorithm
    if (is.null(rv$current.obj) ) {return (NULL)}
    if (is.null(input$missing.value.algorithm) || is.null(input$missing.value.basic.algorithm)){return (NULL)}
    
    if ((input$missing.value.algorithm == "Basic methods") && (input$missing.value.basic.algorithm == "dummy censored")){
        numericInput("OnlyLAPALA_qmin", "Quantile (%)", value = 2.5, step=0.1, min=0, max=100)
    }
    
})




output$MVI_qmin_option <- renderUI({
    
    rv$current.obj
    if (is.null(rv$current.obj) ) {return (NULL)}
    if (is.null(input$missing.value.algorithm)){return (NULL)}
    if (is.null(input$imp4p_withLapala)){return(NULL)}
    
    if ((input$missing.value.algorithm == "imp4p") && (input$imp4p_withLapala==TRUE)){
        numericInput("imp4p_qmin", "upper lapala limit (as a centile of observed distribution)", value = 2.5, step=0.1, min=0, max=100)
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
    
    isolate({
        result = tryCatch(
            {
                if (input$missing.value.algorithm == "None"){
                    rv$current.obj <- rv$dataset[[input$datasets]]
                } else if (input$missing.value.algorithm == "imp4p")
                {
                    
                    if (input$imp4p_withLapala) {
                        rv$current.obj <- wrapper.dapar.impute.mi(rv$dataset[[input$datasets]],
                                                                  #eps = input$imp4p_eps,
                                                                  nb.iter = input$imp4p_nbiter,
                                                                  lapala = input$imp4p_withLapala,
                                                                  q.min = input$imp4p_qmin / 100)
                        #write log command file
                        writeToCommandLogFile(
                            paste("current.obj <- wrapper.dapar.impute.mi(",
                                  "dataset[['",input$datasets,"']] nb.iter=",input$imp4p_nbiter,
                                  ", lapala = ", input$imp4p_withLapala, ", q.min = ", input$imp4p_qmin / 100, ")",sep=""))
                    } else {
                        rv$current.obj <- wrapper.dapar.impute.mi(rv$dataset[[input$datasets]],
                                                                  #eps = input$imp4p_eps,
                                                                  nb.iter = input$imp4p_nbiter,
                                                                  lapala = input$imp4p_withLapala)
                        #write log command file
                        writeToCommandLogFile(
                            paste("current.obj <- wrapper.dapar.impute.mi(",
                                  "dataset[['",input$datasets,"']] nb.iter=",input$imp4p_nbiter,
                                  ", lapala = ", input$imp4p_withLapala, ")",sep=""))
                    }
                    
                    updateSelectInput(session, 
                                      "missing.value.algorithm", 
                                      selected = input$missing.value.algorithm)
                    
                } else if (input$missing.value.algorithm == "Basic methods"){
                    if (input$missing.value.basic.algorithm %in% c("KNN", "MLE")) 
                    {
                        
                        busyIndicator("Calculation in progress",wait = 0)
                        rv$current.obj <- wrapper.mvImputation(rv$dataset[[input$datasets]],
                                                               input$missing.value.basic.algorithm)
                        
                        #write log command file
                        writeToCommandLogFile(
                            paste("current.obj <- wrapper.mvImputation(",
                                  "dataset[['",input$datasets, "']],'",input$missing.value.basic.algorithm,"')", sep="")
                        )
                        
                        updateSelectInput(session, "missing.value.algorithm", 
                                          selected = input$missing.value.algorithm)
                        updateSelectInput(session,"missing.value.basic.algorithm",
                                          selected = input$missing.value.basic.algorithm)
                        
                    } else if (input$missing.value.basic.algorithm ==  "dummy censored")
                    {
                        
                        
                        rv$current.obj <- wrapper.impute.pa2(rv$dataset[[input$datasets]], 
                                                             q.min = (input$OnlyLAPALA_qmin/100),
                                                             distribution = input$OnlyLAPALA_distrib)
                        #write log command file
                        writeToCommandLogFile(
                            paste("current.obj <- wrapper.impute.pa2(",
                                  "dataset[['", input$datasets,"']])",sep="")
                        )
                        
                        updateSelectInput(session, 
                                          "missing.value.algorithm", 
                                          selected = input$missing.value.algorithm)
                        updateSelectInput(session,"missing.value.basic.algorithm",
                                          selected = input$missing.value.basic.algorithm)
                        updateSelectInput(session,"OnlyLAPALA_distrib",
                                          selected = input$OnlyLAPALA_distrib)
                        
                    }
                }
                
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







output$histoMV_Image_DS <- renderPlot({
    rv$current.obj
    if (is.null(rv$current.obj)) {return(NULL)}
    
    result = tryCatch(
        {
            wrapper.mvHisto(rv$current.obj)
        }
        , warning = function(w) {
            shinyjs::info(conditionMessage(w))
        }, error = function(e) {
            shinyjs::info(paste(match.call()[[1]],":",conditionMessage(e), sep=" "))
        }, finally = {
            #cleanup-code 
        })
    
})


output$histoMV_Image <- renderPlot({
    rv$current.obj
    if (is.null(rv$current.obj)) {return(NULL)}
    result = tryCatch(
        {
            if (!is.null(rv$current.obj)){wrapper.mvHisto(rv$current.obj)}
        }
        , warning = function(w) {
            shinyjs::info(conditionMessage(w))
        }, error = function(e) {
            shinyjs::info(paste(match.call()[[1]],":",conditionMessage(e), sep=" "))
        }, finally = {
            #cleanup-code 
        })
    
    
    
})




##' xxxxxxxxxxxxxxxxxxxxxxxx
##' @author Samuel Wieczorek
output$showImageNA <- renderPlot({
    
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





output$warningImputationMethod <- renderUI({
    input$missing.value.algorithm
    if (is.null(input$missing.value.algorithm)) {return (NULL)}
    
    
    uiOutput("warningLapala")
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

output$showImputationPanel <- renderUI({
    rv$current.obj
    
    if (is.null(rv$current.obj)) {return (NULL)}
    
    nbEmptyLines <- getNumberOfEmptyLines(exprs(rv$current.obj))
    if (nbEmptyLines == 0)
    {
        conditionalPanel(id = "wellPanel_Imputation",
                         condition = "true",
                         helpText("Select an imputation method before 
                                  performing the imputation of missing values."),
                         
                         uiOutput("helpForImputation"),
                         
                         busyIndicator("Calculation in progress",wait = 0),
                         #imageOutput("viewNAbyMean"),
                         fluidRow(
                             column(width = 5, plotOutput("viewNAbyMean"
                                                          , height = "600px", width = "400px"))
                             ,column(width = 7, plotOutput("showImageNA"
                             ))
                         ),
                         uiOutput("Ref_imputation")
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



output$warningLapala <- renderUI({
    input$imp4p_withLapala
    if (is.null(input$imp4p_withLapala) || (input$imp4p_withLapala == FALSE)){return(NULL)}
    
    
    t <- "warning: imputed lapala values must be very cautiously interpreted"
    h2(t)
})


output$Ref_imputation <- renderUI({
    
    txt <- "<strong><font size=\"4\">References:</font></strong>
    <ol>
    <li> Bolstad B.M. (2007) preprocessCore: a collection of 
    pre-processing functions. R package version 1.32.0
    </li>
    <li> Q. GIAI GIANETTO, C. LAZAR, S. WIECZOREK, C. BRULEY, Y. COUTE AND 
    T. BURGER. Multiple imputation strategy for mass <br> spectrometry-based 
    proteomic data (under preparation)
    </li>
    <li> Hastie T. et al.  . (2001). impute: imputation for microarray 
    data. R package version 1.44.0.
    </li>
    <li> Schafer J.L. (2008). NORM: Analysis of Incomplete Multivariate Data 
    under a Normal Model, Version 3. Software package for R.
    </li>
    </ol>"
    
    HTML(txt)
    
})


output$helpForImputation <- renderUI({
    input$missing.value.algorithm
    input$missing.value.basic.algorithm
    rv$typeOfDataset
    
    if (is.null(input$missing.value.algorithm) || (input$missing.value.algorithm == "None")) {return(NULL)}
    if ((input$missing.value.algorithm == "Basic methods") && is.null(input$missing.value.basic.algorithm == "None")) {return(NULL)}
    
    name <- NULL
    t <- "Lapala (from French \"là/pas-là\", meaning \"here/not-here\") refers 
    to analytes (peptides or proteins) <br>that are entirely missing in some 
    conditions while they are (partially or totally) <br>visible in others. There 
    specific accounting in a conservative way is a real issue as the imputation <br>
    cannot rely on any observed value in a given condition."
    helpTextImputation <- list("imp4p" = "imp4p [ref2] is a proteomic-specific multiple imputation 
                               method that operates on peptide-level datasets and which proposes <br>
                               to impute each missing value according to its nature (censored 
                               or random). <br> The more iterations, the more accurate the results, 
                               yet the more time-consuming.",
                               "dummy censored" = "each missing value is supposed to be a censored value and 
                               is replaced by the XXX quantile <br> of the corresponding sample 
                               abundance distribution",
                               "KNN" = "K- nearest neighbors, see [Ref3]",
                               "MLE" = "Maximum likelihood estimation, see [ref4]")
    
    
    if (input$missing.value.algorithm == "Basic methods") {
        name <- input$missing.value.basic.algorithm}
    else {name <- input$missing.value.algorithm}
    
    print(name)
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
    
    conditionalPanel(condition='true',
                     h5("This may take a while,"),
                     h5("please be patient ..."),
                     progressBar2("pb1",value=0, size="sm", color="aqua", striped=TRUE, active=TRUE, label=TRUE)
    )
})




##' boxplot of intensities in current.obj
##' @author Samuel Wieczorek
output$viewNAbyMean <- renderPlot({
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





