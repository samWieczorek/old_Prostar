###########################################################################
###########################################################################
###########################################################################
##            NORMALIZATION FUNCTIONS                                    ##
###########################################################################
###########################################################################

output$helpForNormalizationMethods <- renderUI({
    input$normalization.method
    input$normalization.type
    rv$typeOfDataset
    if (is.null(input$normalization.method) || (input$normalization.method == "None")) {return(NULL)}
    toto <- input$normalization.method
    
    
    helpNormalization <- matrix(rep("", 12),nrow = 3, 
                                dimnames=list(c("Global Alignment", "Quantile Centering", "Mean Centering"),
                                              c("sum by columns", "Alignment on all quantiles", "overall", "within conditions")))
    
    
    helpNormalization["Global Alignment"] <- "These methods propose 
    normalizations of important magnitude that should be cautiously used:<br>
    <ul>
    <li>
    <strong>sum by columns</strong> operates on the original scale (not the log2 one) and propose to 
    normalize each abundance <br> by the total abundance of the sample (so as to focus 
    on the analyte proportions among each sample).
    </li>
    <li>
    <strong>Alignment on all quantiles</strong> proposes to align the quantiles of all the 
    replicates as described in [6]; <br> practically it amounts to replace 
    abundances by order statistics.
    </li>
    </ul>"
    
    
    
    helpNormalization["Quantile Centering"] <- "These methods propose 
    to shift the sample distributions (either all of them at once, or within 
    each condition at a time) to align <br> a specific quantile: the median (under 
    the assumption that up-regulations and down-regulations are equally frequent), <br>
    the 15% quantile <br> (under the assumption that the signal/noise ratio is 
    roughly the same in all the samples), or any other user's choice."
    
    
    helpNormalization["Mean Centering"] <- "These methods propose to shift the 
    sample distributions (either all of them at once, or within each condition 
    at a time) to align their means. <br> It is also possible to force unit variance 
    (or not)."
    
    
    
    HTML(helpNormalization[input$normalization.method])
})




output$choose_normalizationQuantile <- renderUI({
    rv$current.obj
    input$normalization.method
    if (is.null(rv$current.obj)) { return (NULL)}
    if (is.null(input$normalization.method)) { return (NULL)}
    if (input$normalization.method != "Quantile Centering") { return (NULL)}
    
    #    if (input$normalization.method == "Quantile Centering"){
    # check if the normalisation has already been performed
    quantileChoices <- list("0.15 (lower limit / noise)"="0.15", "0.5 (median)" = "0.5", "Other"="Other")
    quantileSelected <- 0.15
    if(!is.null(rv$current.obj@experimentData@other$normalizationQuantile)) { 
        quantileSelected <- as.numeric(rv$current.obj@experimentData@other$normalizationQuantile)
    }
    radioButtons("normalization.quantile", "Choose normalization quantile",  choices = quantileChoices, selected=quantileSelected)
    
    
})


output$choose_normalizationQuantileOther <- renderUI({
    input$normalization.quantile
    input$normalization.method
    if (is.null(input$normalization.quantile)){return(NULL)}
    if (input$normalization.method != "Quantile Centering") { return (NULL)}
    
    quantileOther <- 0.15
    if(!is.null(rv$current.obj@experimentData@other$normalizationQuantileOther)) { 
        quantileOther <- rv$current.obj@experimentData@other$normalizationQuantileOther
    }
    
    if (input$normalization.quantile == "Other"){
        numericInput("normalization.quantileOther", "Choose normalization quantile other",
                     min=0, max = 1 , value = quantileOther,
                     step = 0.1)
        
    }
    
})


output$choose_normalizationScaling <- renderUI({
    rv$current.obj
    input$normalization.method
    if (is.null(rv$current.obj)) { return (NULL)}
    if (is.null(input$normalization.method)) { return (NULL)}
    
    
    if (input$normalization.method %in% c("Mean Centering")){
        # check if the normalisation has already been performed
        varreduction <- FALSE
        if(!is.null(rv$current.obj@experimentData@other$normalizationScaling)) { 
            varreduction <- rv$current.obj@experimentData@other$normalizationScaling
        }
        checkboxInput("normalization.variance.reduction", "Include variance reduction",  value = varreduction)
    }
    
})

output$choose_normalizationType <- renderUI({
    rv$current.obj
    input$normalization.method
    if (is.null(rv$current.obj)) { return (NULL)}
    if (is.null(input$normalization.method)) { return (NULL)}
    if (input$normalization.method == "None") { return (NULL)}
    
    
    
    if (input$normalization.method %in% c("Quantile Centering", "Mean Centering")){
        
        # check if the normalisation has already been performed
        type <- c("overall", "within conditions")
        typeSelected <- NULL
        if(!is.null(rv$current.obj@experimentData@other$normalizationType)) { 
            typeSelected <- rv$current.obj@experimentData@other$normalizationType
        }
        # if (GetNbNA() == 0){
        #     choices <- c("overall", "within conditions")
        # } else {
        #     choices <- c("overall", "within conditions")
        # } 
        # 
        
    } else if (input$normalization.method %in% c("Global Alignment")){
        type <- c("sum by columns", "Alignment on all quantiles")
        typeSelected <- NULL
        if(!is.null(rv$current.obj@experimentData@other$normalizationType)) { 
            typeSelected <- rv$current.obj@experimentData@other$normalizationType
        }
    }
    selectInput("normalization.type", "Choose normalization type",  choices = type, selected = typeSelected)
})



output$choose_Normalization_Test <- renderUI({
    rv$current.obj
    if (is.null(rv$current.obj)) { return (NULL)}
    
    # check if the normalisation has already been performed
    method <- normMethods
    methodSelected <- NULL
    if( !is.null(rv$current.obj@experimentData@other$normalizationMethod)) { 
        methodSelected <- rv$current.obj@experimentData@other$normalizationMethod
    }
    selectInput("normalization.method","Choose normalization method", method, selected = methodSelected)
})


# Check boxes
# output$choose_Normalization_2 <- renderUI({
#     input$normalization.family
#     if(is.null(input$normalization.family) || 
#         ( input$normalization.family == "None"))
#     return()
#     
#     outVar <- normalization.methods[[which(names(normalization.methods) == 
#                                             input$normalization.family)]]
#     selectInput("normalization.method", "Choose normalization method",
#                 choices  = outVar)
# })



##' Reactive behavior : Normalization of data
##' @author Samuel Wieczorek
observeEvent(input$perform.normalization,{
    input$perform.normalization
    input$normalization.method
    input$normalization.type
    input$normalization.quantile
    if (is.null(input$perform.normalization) ){return(NULL)}
    #if (input$perform.normalization == 0){return(NULL)}
    
    
    isolate({
        result = tryCatch(
            {
                # input$normalization.quantile
                # input$normalization.quantileOther
                # input$normalization.scaling
                # input$normalization.type
                # input$normalization.method
                # 
                
                #.temp <- unlist(strsplit(input$normalization.method, " - "))
                
                createPNG_BeforeNormalization()
                
                if (input$normalization.method == G_noneStr){
                    rv$current.obj <- rv$dataset[[input$datasets]]
                } else {
                    
                    if (input$normalization.method == "Global Alignment"){
                        rv$current.obj <- wrapper.normalizeD2(rv$dataset[[input$datasets]], 
                                                              input$normalization.method, 
                                                              input$normalization.type)
                        updateSelectInput(session, "normalization.method", selected = input$normalization.method)
                        updateSelectInput(session, "normalization.type", selected = input$normalization.type)
                        
                        ## Write command log file
                        writeToCommandLogFile(
                            paste("current.obj <- wrapper.normalizeD2(",
                                  "dataset[['",
                                  input$datasets, 
                                  "']],'",input$normalization.method, "','", input$normalization.type,"')",
                                  sep="")
                        )
                    }
                    else if (input$normalization.method =="Quantile Centering"){
                        
                        
                        quant <-NA
                        #print(input$normalization.quantile)
                        if (!is.null(input$normalization.quantile) && (input$normalization.quantile != "Other"))
                        {quant <- as.numeric(input$normalization.quantile)}
                        else {quant <- as.numeric(input$normalization.quantileOther)}
                        print(quant)
                        rv$current.obj <- wrapper.normalizeD2(rv$dataset[[input$datasets]], 
                                                              input$normalization.method, 
                                                              input$normalization.type, 
                                                              quantile = quant)
                        updateSelectInput(session, "normalization.method", selected = input$normalization.method)
                        updateSelectInput(session, "normalization.type", selected = input$normalization.type)
                        if (!is.null(input$normalization.quantile)){
                            updateRadioButtons(session, "normalization.quantile", selected = input$normalization.quantile)}
                        
                        if (!is.null(input$normalization.quantileOther)){
                            updateNumericInput(session, "normalization.quantileOther", value = input$normalization.quantileOther)}
                        
                        ## Write command log file
                        writeToCommandLogFile(
                            paste("current.obj <- wrapper.normalizeD2(",
                                  "dataset[['",
                                  input$datasets, 
                                  "']],'",input$normalization.method, "','", input$normalization.type,
                                  "', quant =", quant,")",
                                  sep="")
                        )
                        
                    }   
                    else if (input$normalization.method =="Mean Centering"){
                        rv$current.obj <- wrapper.normalizeD2(rv$dataset[[input$datasets]], 
                                                              input$normalization.method, 
                                                              input$normalization.type, 
                                                              scaling=input$normalization.variance.reduction)
                        updateSelectInput(session, "normalization.method", selected = input$normalization.method)
                        updateSelectInput(session, "normalization.type", selected = input$normalization.type)
                        scale <- FALSE
                        if( !is.null(input$normalization.variance.reduction)){scale <- input$normalization.variance.reduction}
                        
                        ## Write command log file
                        writeToCommandLogFile(
                            paste("current.obj <- wrapper.normalizeD2(",
                                  "dataset[['",
                                  input$datasets, 
                                  "']],'",input$normalization.method, "','", input$normalization.type,
                                  "', scaling =", input$normalization.variance.reduction,")",
                                  sep="")
                        )
                    } 
                    
                    createPNG_Normalization()
                    
                }
            }
            , warning = function(w) {
                shinyjs::info(conditionMessage(w))
            }, error = function(e) {
                shinyjs::info(paste("Perform normalization",":",conditionMessage(e), sep=" "))
            }, finally = {
                #cleanup-code 
            })
        
        
    })
})


##' -- Validate the normalization ---------------------------------------
##' @author Samuel Wieczorek
observeEvent(input$valid.normalization,{ 
    
    input$normalization.method
    if (is.null(input$valid.normalization) || (input$valid.normalization == 0)) 
    {return(NULL)}
    
    isolate({
        result = tryCatch(
            {
                if (input$normalization.method != G_noneStr) {
                    
                    rv$typeOfDataset <-rv$current.obj@experimentData@other$typeOfData
                    name <- paste ("Normalized", " - ", rv$typeOfDataset, sep="")
                    rv$dataset[[name]] <- rv$current.obj
                    
                    
                    #write command log file
                    writeToCommandLogFile(
                        paste("dataset[['",name,"']] <- current.obj", sep="")
                    )
                    
                    updateSelectInput(session, "datasets", 
                                      paste("Dataset versions of",rv$current.obj.name, sep=" "),
                                      choices = names(rv$dataset),
                                      selected = name)
                    UpdateLog(paste("Normalization : data normalized with the method",
                                    input$normalization.method, sep=" "), name)
                    
                    
                    ## Add the necessary text to the Rmd file
                    txt2Rmd <- readLines("Rmd_sources/normalization_Rmd.Rmd")
                    filename <- paste(tempdir(), sessionID, 'report.Rmd',sep="/")
                    write(txt2Rmd, file = filename,append = TRUE, sep = "\n")
                    createPNG_Normalization()
                    
                }
            }
            , warning = function(w) {
                shinyjs::info(conditionMessage(w))
            }, error = function(e) {
                shinyjs::info(info("Validate the normalization",":",conditionMessage(e), sep=" "))
            }, finally = {
                #cleanup-code 
            })
        
    } )
})


###########################################################################
###########################################################################
###########################################################################



##########################
output$ChooseLegendForNormTabPanel <- renderUI({
    rv$current.obj
    if (is.null(rv$current.obj)){return(NULL)}
    .names <- colnames(Biobase::pData(rv$current.obj))[-1]
    checkboxGroupInput("legendXAxisNormTabPanel",
                       label = "Choose data to show in legend",
                       choices = .names,
                       selected = .names[1])
})

output$choose_Normalization_1 <- renderUI({
    isolate({
        selectInput("normalization.family", 
                    "Choose normalization family", 
                    names(normalization.methods))
    })
})




viewBoxPlotNorm <- reactive({
    
    rv$current.obj
    input$whichGroup2Color
    input$legendXAxis
    input$normalization.method
    input$perform.normalization
    
    if (is.null(rv$current.obj) || is.null(rv$dataset[[input$datasets]]) ||
        is.null(input$normalization.method)) {return(NULL)}
    
    print(input$legendXAxis)
    gToColorNorm <- NULL
    
    if (is.null(input$whichGroup2Color)){
        gToColorNorm <- "Condition"
    }else{gToColorNorm <- input$whichGroup2Color}
    
    leg <- NULL
    if (is.null(input$legendXAxis)){
        .names <- colnames(Biobase::pData(rv$current.obj))[-1]
        leg <- .names[1]}
    else{leg <- input$legendXAxis}
    
    result = tryCatch(
        {
            wrapper.boxPlotD(rv$current.obj, leg )
        }
        , warning = function(w) {
            shinyjs::info(conditionMessage(w))
        }, error = function(e) {
            shinyjs::info(paste(match.call()[[1]],":",conditionMessage(e), sep=" "))
        }, finally = {
            #cleanup-code 
        })
    
    
    
})

##' boxplot of intensities in current.obj
##' @author Samuel Wieczorek
output$viewBoxPlotNorm <- renderPlot({
    viewBoxPlotNorm()
    
})


viewDensityplotNorm <- reactive({
    
    rv$dataset[[input$datasets]]
    rv$current.obj
    input$legendXAxis
    input$whichGroup2Color
    input$lab2Show
    input$normalization.method
    input$perform.normalization
    
    if (is.null(rv$current.obj) || is.null(rv$dataset[[input$datasets]]) ||
        is.null(input$normalization.method)) {return(NULL)}
    
    
    leg <- NULL
    grp <- NULL
    
    labelsNorm <- NULL
    labelsToShowNorm <- NULL
    gToColorNorm <- NULL
    if (is.null(input$lab2Show)) { 
        labelsToShowNorm <- c(1:nrow(Biobase::pData(rv$current.obj)))
    }
    else { labelsToShowNorm <- input$lab2Show}
    
    if (is.null(input$whichGroup2Color)){
        gToColorNorm <- "Condition"
    }else{gToColorNorm <- input$whichGroup2Color}
    
    
    if (is.null(input$whichGroup2Color) 
        || (input$whichGroup2Color == "Condition")){
        labelsNorm <- Biobase::pData(rv$current.obj)[,"Label"]
    }else {
        labelsNorm <- paste(Biobase::pData(rv$current.obj)[,"Label"],
                            Biobase::pData(rv$current.obj)[,"Bio.Rep"],
                            Biobase::pData(rv$current.obj)[,"Tech.Rep"],
                            Biobase::pData(rv$current.obj)[,"Analyt.Rep"],
                            sep= "_")
    }
    
    result = tryCatch(
        {
            wrapper.densityPlotD_HC(rv$current.obj, labelsNorm, as.numeric(labelsToShowNorm), 
                                    gToColorNorm)
        }
        , warning = function(w) {
            shinyjs::info(conditionMessage(w))
        }, error = function(e) {
            shinyjs::info(paste(match.call()[[1]],":",conditionMessage(e), sep=" "))
        }, finally = {
            #cleanup-code 
        })
    
    
    
    
})

##' Distribution of intensities in current.obj
##' @author Samuel Wieczorek
output$viewDensityplotNorm<- renderHighchart({
    viewDensityplotNorm()
})   



#######################
output$viewComparisonNorm<- renderPlot({
    
    rv$dataset[[input$datasets]]
    rv$current.obj
    input$legendXAxis
    input$whichGroup2Color
    input$lab2Show
    input$normalization.method
    input$perform.normalization
    
    if (is.null(rv$current.obj) 
        || is.null(rv$dataset[[input$datasets]]) 
        || is.null(input$normalization.method)
        || 
        (rv$typeOfDataset != rv$current.obj@experimentData@other$typeOfData)) {return(NULL)}
    
    
    leg <- NULL
    grp <- NULL
    
    labelsNorm <- NULL
    labelsToShowNorm <- NULL
    gToColorNorm <- NULL
    if (is.null(input$lab2Show)) { 
        labelsToShowNorm <- c(1:nrow(Biobase::pData(rv$current.obj)))
    }
    else { labelsToShowNorm <- input$lab2Show}
    
    if (is.null(input$whichGroup2Color)){
        gToColorNorm <- "Condition"
    }else{gToColorNorm <- input$whichGroup2Color}
    
    
    if (is.null(input$whichGroup2Color) 
        || (input$whichGroup2Color == "Condition")){
        labelsNorm <- Biobase::pData(rv$current.obj)[,"Label"]
    }else {
        labelsNorm <- paste(Biobase::pData(rv$current.obj)[,"Label"],
                            Biobase::pData(rv$current.obj)[,"Bio.Rep"],
                            Biobase::pData(rv$current.obj)[,"Tech.Rep"],
                            Biobase::pData(rv$current.obj)[,"Analyt.Rep"],
                            sep= "_")
    }
    
    result = tryCatch(
        {
            compareNormalizationD(Biobase::exprs(rv$dataset[[input$datasets]]),
                                  Biobase::exprs(rv$current.obj),
                                  labelsNorm,
                                  as.numeric(labelsToShowNorm),
                                  gToColorNorm)
        }
        #, warning = function(w) {
        #   shinyjs::info(conditionMessage(w))
        #}
        , error = function(e) {
            shinyjs::info(paste(match.call()[[1]],":",conditionMessage(e), sep=" "))
        }, finally = {
            #cleanup-code 
        })
    
})




output$AbsShowOptions <- renderUI({
    input$plotOptions
    if (!input$plotOptions) {return(NULL)}
    
    tagList(
                     uiOutput("ChooseLegendForAxis"),
                     uiOutput("nShow"),
                     uiOutput("nGroup")
    )
})



#------------------------------------------------------
output$ChooseLegendForAxis <- renderUI({
    rv$current.obj
    if (is.null(rv$current.obj)){return(NULL)}
    isolate(rv$current.obj)
    .names <- colnames(Biobase::pData(rv$current.obj))[-1]
    tags$head(tags$link(rel="stylesheet", type="text/css", 
                        href="css/overrides.css"))
    
    checkboxGroupInput("legendXAxis",
                       label = "Choose data to show in legend",
                       choices = .names,
                       selected = .names[1])
})


##' Select the labels to show in densityplots
##' @author Samuel Wieczorek
output$nShow <- renderUI({
    rv$current.obj
    if (is.null(rv$current.obj) ) {return(NULL) }
    
    isolate({
        rv$current.obj
        labs <- paste(Biobase::pData(rv$current.obj)[,"Label"],
                      Biobase::pData(rv$current.obj)[,"Bio.Rep"],
                      Biobase::pData(rv$current.obj)[,"Tech.Rep"],
                      Biobase::pData(rv$current.obj)[,"Analyt.Rep"],
                      sep= "_")
        
        label.names <- setNames(as.list(c(1:length(labs))),labs)
        
        
        checkboxGroupInput("lab2Show"
                           , label = "Select data to show"
                           , choices = label.names
                           , selected = unlist(label.names))
        
    })
})


##' Select the labels to be highlighted in densityplots
##' @author Samuel Wieczorek
output$nGroup <- renderUI({
    rv$current.obj
    if (is.null(rv$current.obj) ) {return(NULL) }
    
    radioButtons("whichGroup2Color",
                 "Plot to show",
                 choices=list("By condition" = "Condition",
                              "By replicate" = "Replicate"))
    
})


