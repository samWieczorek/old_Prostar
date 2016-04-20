options(shiny.maxRequestSize=30*1024^2) 
options(shiny.trace=TRUE)
options(shiny.reactlog=TRUE)

library(shiny)
library(rhandsontable)
library(data.table)
library(reshape2)
library(DT)


# initialize data with colnames
df <- data.frame(matrix(c("0","0"), 1, 2))
colnames(df) <- c("Input1", "Input2")

port <- data.table(Experiment=list(),
                    Label=list(),
                    Bio.Rep=list(),
                    Tech.Rep=list(),
                    Analyt.Rep=list())

shinyServer(function(input, output, session) {
cat(file=stderr())

    
    


#-------------------------------------------------------------
rv <- reactiveValues(
    # variable to handle the current object that will be showed
    current.obj = NULL,
    current.obj.name = NULL,
    deleted.mvLines = NULL,
    deleted.contaminants = NULL,
    deleted.reverse = NULL,
    # variable to keep memory of previous datasets before 
    # transformation of the data
    dataset = list(),
    # Variable that contains the log for the current R session
    text.log = data.frame(Date="", Dataset="", History="", stringsAsFactors=F),
    seuilLogFC = 0,
    seuilPVal = 1e-60,
    tab1 = NULL,
    dirname = "",
    dirnameforlink = "",
    conditions = list(cond1 = NULL, cond2 = NULL),
    temp.aggregate = NULL,
    hot = port, 
    calibrationRes = NULL,
    errMsgcalibrationPlot = NULL,
    errMsgcalibrationPlotALL = NULL,
    typeOfDataset = "",
    widthSidebar = 3,
    commandLog = NULL)

env <- environment()

writeToCommandLogFile <- function(txt){
    
    cat(rv$commandLog,
        file = commandLogFile,
        txt,
        sep = "\n",
        append = TRUE)
}

if (file.exists(commandLogFile)){
    file.remove(commandLogFile)
}




output$CurrentDataset <- renderUI({
    txt <- paste("Current dataset :",input$datasets, sep=" ")
    txt
})


#-------------------------------------------------------------
output$hot <- renderRHandsontable({
    input$eData.box
    if (is.null(input$eData.box)) {
    DT <- rv$hot
    } else {
    DT <- data.table(Experiment = as.character(input$eData.box),
                        Label = rep(" ",length(input$eData.box)),
                        Bio.Rep = rep(" ",length(input$eData.box)),
                        Tech.Rep = rep(" ",length(input$eData.box)),
                        Analyt.Rep = rep(" ",length(input$eData.box)))

    #rownames(DT) <- input$eData.box
    rv$hot <- DT

    }

    if (!is.null(DT))
    rhandsontable(DT) %>% 
    hot_cols(colWidths = c(200, 100, 100, 100, 100) ) %>%
    hot_rows(rowHeights = 30) %>%
    hot_col(col = "Experiment", readOnly = TRUE)
})




output$diffAnalysis_sidebarPanelTab1 <- renderUI({
    
    method <- NULL
    threshold.logFC <- 0
    if ("logFC" %in% names(fData(rv$current.obj) )){
        
        method <- rv$current.obj@experimentData@other$method
        threshold.logFC <- rv$current.obj@experimentData@other$threshold.logFC
        #cond2 <- rv$current.obj@experimentData@other$condition2
    }
    
    conditionalPanel(condition=TRUE,
                    uiOutput("RenderLimmaCond1"),
                    uiOutput("RenderLimmaCond2"),
                    selectInput("diffAnaMethod","Choose the statistical test",
                                choices = c("None","Limma", "Welch"),
                                selected = method),
                    numericInput("seuilLogFC", "Define log(FC) threshold",
                                min = 0,value = threshold.logFC,step=0.1),
HTML("This corresponds to the ratio: <br>Condition 2 / Condition 1.")
) })


output$diffAnalysis_sidebarPanelTab2 <- renderUI({
    calibMethod <- "pounds"
    if ("logFC" %in% names(fData(rv$current.obj) )){
        calibMethod <- rv$current.obj@experimentData@other$calibrationMethod
        if (is.null(calibMethod)) calibMethod <- "pounds"
    }
    
    conditionalPanel(condition=TRUE,
                    selectInput("calibrationMethod", 
                                "Choose the calibration method",
                                choices = c("st.boot", "st.spline", 
                                            "langaas","jiang", "histo", 
                                            "pounds", "abh","slim", 
                                            "Benjamini-Hochberg", 
                                            "numeric value"),
                                selected = calibMethod),
                    uiOutput("numericalValForCalibrationPlot"))
    })

output$diffAnalysis_sidebarPanelTab3 <- renderUI({
    threshold.PVal <- 0
    if ("logFC" %in% names(fData(rv$current.obj) )){
        threshold.PVal <- rv$current.obj@experimentData@other$threshold.p.value
        #cond2 <- rv$current.obj@experimentData@other$condition2
    }
    
    
    conditionalPanel(condition=TRUE,
                    numericInput("seuilPVal", 
                                "Define the -log10(p.value) threshold",
                                min = 0,value = threshold.PVal,step=0.1)
) })




output$DP_sidebar_FilterTab1 <- renderUI({
    
    filter <- NULL
    print(rv$current.obj@experimentData@other$mvFilter.method)
    tag <- rv$current.obj@experimentData@other$mvFilter.method
    if (!is.null(tag)) { filter <- tag}
    conditionalPanel(condition=TRUE
                    ,h4("Missing values filtering options")
                    ,hr()
                    ,radioButtons("ChooseFilters","", 
                                choices = gFiltersList,
                                selected = filter)
                    ,conditionalPanel(
                        condition='input.ChooseFilters != "None"',
                        uiOutput("seuilNADelete"))
                    )
})

output$DP_sidebar_FilterTab2 <- renderUI({
    conditionalPanel(condition=TRUE
                    ,h4("String based filtering options")
                    ,hr()
                    ,h4("Filter contaminants"),
                    uiOutput("id_Contaminants"),
                    uiOutput("choosePrefixContaminants"),
                    br(),
                    h4("Filter reverse"),
                    uiOutput("id_Reverse"),
                    uiOutput("choosePrefixReverse")
                    )
})


output$DP_sidebar_FilterTab3 <- renderUI({
    conditionalPanel(condition=TRUE
                    ,h4("Filtered data display")
                    ,hr()
                    ,radioButtons("ChooseTabAfterFiltering", 
                                "Choose the data to display",
                                choices=
                                    list("Quantitative data" = "quantiData",
                                        "Meta data" = "MetaData"))
                    ,radioButtons("ChooseViewAfterFiltering", 
                                "Choose the type of filtered data", 
                                choices=
                        list("Deleted on missing values" = "MissingValues",
                        "Deleted contaminants" = "Contaminants",
                        "Deleted reverse" = "Reverse"))
                    )
})





#----------------------------------------------
output$VizualizeFilteredData <- DT::renderDataTable({
    rv$current.obj
    input$nDigitsMV
    input$ChooseViewAfterFiltering
    input$ChooseTabAfterFiltering
    
    if (is.null(input$ChooseTabAfterFiltering)) {return(NULL)}
    if (is.null(input$ChooseViewAfterFiltering)) {return(NULL)}
    
    
    if (is.null(rv$current.obj)) {return(NULL)}
    
    
    if (input$nDigitsMV){nDigits = 1e100}else {nDigitsMV = 3}
    
    data <- NULL
    if ((input$ChooseViewAfterFiltering == "MissingValues") 
        && !is.null(rv$deleted.mvLines))
        {
        obj <- rv$deleted.mvLines
        if(input$ChooseTabAfterFiltering == "quantiData" )
        {
        data <- cbind(ID = rownames(fData(obj)),
                    round(exprs(obj), digits=nDigitsMV))
        }else {data <- cbind(ID = rownames(fData(obj)),fData(obj))}
    } else if ((input$ChooseViewAfterFiltering == "Contaminants") 
                && !is.null(rv$deleted.contaminants)) { 
    obj <- rv$deleted.contaminants
    if(input$ChooseTabAfterFiltering == "quantiData" )
        {data <- cbind(ID = rownames(fData(obj)),
                        round(exprs(obj), digits=nDigitsMV))
        }else {data <- cbind(ID = rownames(fData(obj)),fData(obj))}
    } else if ((input$ChooseViewAfterFiltering == "Reverse") 
                && !is.null(rv$deleted.reverse)){
        obj <- rv$deleted.reverse
        if(input$ChooseTabAfterFiltering == "quantiData" )
        {data <- cbind(ID = rownames(fData(obj)),
                        round(exprs(obj), digits=nDigitsMV))
        }else {data <- cbind(ID = rownames(fData(obj)),fData(obj))}
    }
    
    
    dat <- DT::datatable(data, 
                        options=list(pageLength=DT_pagelength,
                                    orderClasses = TRUE,
                                    autoWidth=FALSE)
    )
    
    return(dat)
    
})


output$AbsShowOptions <- renderUI({
    input$plotOptions
    if (!input$plotOptions) {return(NULL)}
    
    conditionalPanel(id = "condPanelShowOptions",
    condition=TRUE,
    uiOutput("ChooseLegendForAxis"),
    uiOutput("nShow"),
    uiOutput("nGroup")
    )
})


output$DS_sidebarPanel_tab <- renderUI({
    input$DS_tabSetPanel
    rv$typeOfDataset
    
    .choices<- NULL
    if (rv$typeOfDataset == "protein") {
    .choices <- list("Quantitative data" = "tabExprs",
                        "Proteins metadata" = "tabfData",
                        "Replicate metadata" = "tabpData",
                        "Dataset history" = "processingData")
    } else if (rv$typeOfDataset == "peptide"){
    .choices <- list("Quantitative data" = "tabExprs",
                        "Peptides metadata" = "tabfData",
                        "Replicate metadata" = "tabpData",
                        "Dataset history" = "processingData")
    } else if (rv$typeOfDataset == ""){
    .choices <- list("Quantitative data" = "tabExprs",
                        "Analyte metadata" = "tabfData",
                        "Replicate metadata" = "tabpData",
                        "Dataset history" = "processingData")
    }
    
    conditionalPanel(condition="true",
                    radioButtons("DS_TabsChoice", "Choose the tab to display",
                                choices = .choices),
                    br(),
                    checkboxInput("nDigits", 
                                "Show full length intensities", 
                                value = FALSE)
    )

})


output$DS_sidebarPanel_heatmap <- renderUI({

conditionalPanel(condition='TRUE',
                    h3("Clustering Options"),
                    radioButtons("distance","Distance",
                                choices = list(euclidean ="euclidean",
                                                manhattan="manhattan")),
                    br(),
                    radioButtons("linkage","Linkage for clustering",
                                choices=list(average="average",
                                            ward.D="ward.D")))
})



output$DS_sidebarPanel_Densityplot <- renderUI({
conditionalPanel(condition='TRUE',
                    uiOutput("nGroup_DS"),
                    br(),
                    uiOutput("nShow_DS"))

})



output$DS_sidebarPanel_Boxplot <- renderUI({
conditionalPanel(condition='TRUE',
                    uiOutput("ChooseLegendForAxis_DS"))

})





#----------------------------------------------
output$tabToShow <- renderUI({
input$DS_TabsChoice

if (input$DS_TabsChoice == "tabExprs"){DT::dataTableOutput("viewExprs")}
else if (input$DS_TabsChoice == "tabfData"){DT::dataTableOutput("viewfData")}
else if (input$DS_TabsChoice == "tabpData"){DT::dataTableOutput("viewpData")}
else if (input$DS_TabsChoice == "processingData"){
    helpText("Previous operations made on the original dataset :")
    DT::dataTableOutput("viewProcessingData")
    }

})



# 
ComputeMVTags <- reactive({
    tags <- TaggingMissingValues(rv$current.obj, 
                                input$type.of.missvalues, 
                                input$seuilMNAR)
    return(tags)
})

########################################################
ComputeAdjacencyMatrix <- reactive({
    #       input$proteinId
    #       rv$current.obj
    #       if (is.null(input$proteinId)){return(NULL)}
    #       if (is.null(rv$current.obj)){return(NULL)}
    #       
    matSharedPeptides <- BuildAdjacencyMatrix(rv$current.obj, 
                                            input$proteinId,
                                            FALSE)
    matUniquePeptides <- BuildAdjacencyMatrix(rv$current.obj, 
                                            input$proteinId,
                                            TRUE)
    
    
    #write command log file
    writeToCommandLogFile(
        paste(
            "matSharedPeptides <- BuildAdjacencyMatrix(current.obj, '",
            input$proteinId, "', FALSE)"
            ,sep=""
        )
    )
        
        writeToCommandLogFile(
            paste(
            "matUniquePeptides <- BuildAdjacencyMatrix(current.obj, '",
            input$proteinId, "', TRUE)"
            ,sep=""
        )
        
        )
        writeToCommandLogFile(
            "mat <- list(matWithSharedPeptides=matSharedPeptides,
                matWithUniquePeptides=matUniquePeptides)"
        )
        
    return(list(matWithSharedPeptides=matSharedPeptides,
                matWithUniquePeptides=matUniquePeptides))
})

########################################################
RunAggregation <- reactive({
    mat <- ComputeAdjacencyMatrix()
    n <- NULL
    if (input$aggregationMethod == gAgregateMethod[["sum on top n"]]) { n <- input$nTopn}
    
    
    tryCatch (
    {
        if (input$checkSharedPeptides){
        data <- pepAgregate(rv$current.obj, 
                            input$proteinId,
                            input$aggregationMethod, 
                            mat$matWithSharedPeptides, 
                            n)
        writeToCommandLogFile(
            paste(
                "data <- pepAgregate(current.obj, '",
                input$proteinId, "', '",
                input$aggregationMethod, "', mat$matWithSharedPeptides)",
                sep=""
            )
        )
        
        
        }else{
        data <- pepAgregate(rv$current.obj, 
                            input$proteinId,
                            input$aggregationMethod, 
                            mat$matWithUniquePeptides
                            , n)
        writeToCommandLogFile(
            paste(
                "data <- pepAgregate(current.obj, '",
                input$proteinId, "', '",
                input$aggregationMethod, "', mat$matWithUniquePeptides)",
                sep=""
            )
        )
        }
    },
    err=function(errorCondition) {
        cat("in err handler")
        message(errorCondition)
    })
    
    
    
    return(data)
})

########################################################
RunDiffAna <- reactive({
    input$diffAnaMethod
    rv$current.obj
    input$condition1
    input$condition2
    
    data <- NULL
    
    
    result = tryCatch(
    {
        writeToCommandLogFile(paste("cond1 <- '", input$condition1, "'", sep=""))
        writeToCommandLogFile(paste("cond2 <- '", input$condition2, "'", sep=""))
        writeToCommandLogFile(paste("method <- '", input$diffAnaMethod, "'", sep=""))
        
        if (input$diffAnaMethod == "Limma"){
        data <- wrapper.diffAnaLimma(rv$current.obj, 
                                    input$condition1, 
                                    input$condition2)
        writeToCommandLogFile(
            "data <- wrapper.diffAnaLimma(current.obj, cond1, cond2)"
            )
        
        } else if (input$diffAnaMethod == "Welch"){
        data <- wrapper.diffAnaWelch(rv$current.obj, 
                                    input$condition1, 
                                    input$condition2)
        writeToCommandLogFile(
            "data <- wrapper.diffAnaWelch(current.obj, cond1, cond2)"
        )
        }
    }
    , warning = function(w) {
        shinyjs::info(w)
    }, error = function(e) {
        shinyjs::info(e)
    }, finally = {
        #cleanup-code
        
    }
    
)
    return(data)
})


########################################################
# Update the global variable log
UpdateLog <- function(text, name){
    rv$text.log <- rbind(c(Date=date(), 
                            Dataset=name, History=text), 
                        rv$text.log)
}

######################################
GetNbNA <- reactive({
    nb <- sum(is.na(exprs(rv$current.obj))==TRUE)
    return(nb)
})




######################################
loadObjectInMemoryFromConverter <- reactive({
    
    rv$typeOfDataset <- rv$current.obj@experimentData@other$typeOfData
    if (is.null(rv$typeOfDataset)) {rv$typeOfDataset <- ""}
    
    name <- paste ("Original", " - ", rv$typeOfDataset, sep="")
    rv$dataset[[name]] <- rv$current.obj
    writeToCommandLogFile("dataset <- list()")
    
    writeToCommandLogFile(
        paste("dataset[['",
              name,
              "']] <- current.obj",
              sep="")
        )

    UpdateFilterWidgets()

    updateSelectInput(session, "datasets", 
                    label = "Dataset versions",
                    choices = names(rv$dataset),
                    selected = name)

    #log update
    UpdateLog(paste("Open : file ",input$file$name, " opened"),name)
})

#---------------------------------------------------- 
ClearMemory <- function(){
    obj2remove <- c(
    "rv$nameOfDataset",
    "session",
    "input",
    "output")
    
    
    rv$text.log <- list()
    rv$tab1 <- NULL
    rv$current.obj <- NULL
    rv$dataset <- list()
    
    updateSelectInput(session, "datasets",  "", choices = "none")
    #UpdateLog("Memory has been cleared","none")
    updateCheckboxInput(session, "replaceAllZeros",value = TRUE)
    updateRadioButtons(session,
                        inputId = "ChooseFilters", 
                        selected = gFilterNone)
    #updateSelectInput(session, "normalization.method",selected = "None")
    # updateSelectInput(session,"type.of.missvalues", selected= "Majoritary" )
    #updateSelectInput(session,"typeImputationMNAR",selected= "QRILC" )
    
}



##-- Open a MSnset File --------------------------------------------
observe({ 
    input$file
    if (is.null(input$file)) {return(NULL)}
    
    isolate({
    exts <- c("MSnset","MSnSet")
    if( is.na(match(GetExtension(input$file$name), exts))) {
        shinyjs::info("Warning : this file is not a MSnset file ! 
                    Please choose another one.")
        }
    else {
        ClearMemory()
        rv$current.obj <- readRDS(input$file$datapath)
        rv$current.obj.name <- DeleteFileExtension(input$file$name)
        rv$typeOfDataset <- rv$current.obj@experimentData@other$typeOfData
        
        writeToCommandLogFile(
            paste("current.obj <- readRDS('",
                                input$file$name,
                                "')", sep="")
        )
       
        loadObjectInMemoryFromConverter()
}
    })
})



##' -- Validate the normalization ---------------------------------------
##' @author Samuel Wieczorek
observe({ 
    input$valid.normalization
    input$normalization.method
    if (is.null(input$valid.normalization) || (input$valid.normalization == 0)) 
        {return(NULL)}
    
    isolate({
    if (input$normalization.method != "None") {
        
        rv$typeOfDataset <-rv$current.obj@experimentData@other$typeOfData
        name <- paste ("Normalized", " - ", rv$typeOfDataset, sep="")
        rv$dataset[[name]] <- rv$current.obj
        
        
        #write command log file
        writeToCommandLogFile(
            paste("rv$dataset[['",name,"']] <- current.obj", sep="")
        )
        
        updateSelectInput(session, "datasets", 
                        choices = names(rv$dataset),
                        selected = name)
        UpdateLog(paste("Normalization : data normalized with the method",
                        input$normalization.method, sep=" "), name)
    }
    } )
})

##' -- Validate the aggregation ---------------------------------------
##' @author Samuel Wieczorek
observe({ 
    input$valid.aggregation
    rv$temp.aggregate
    input$aggregationMethod
    input$columnsForProteinDataset.box
    
    if (is.null(input$valid.aggregation) || (input$valid.aggregation == 0)) 
    {return(NULL)}
    #if (is.null(input$aggregationMethod)) {return(NULL)}
    if (is.null(rv$temp.aggregate)) {return(NULL)}
    
    isolate({
    input$aggregationMethod
    input$proteinId
    input$checkSharedPeptides
    input$columnsForProteinDataset.box
    
    ##concatenation des informations
    mat <- ComputeAdjacencyMatrix()
    m <- NULL
    if (input$checkSharedPeptides){ 
        m <- mat$matWithSharedPeptides
        writeToCommandLogFile("m <- mat$matWithSharedPeptides")
    }else{ m <-mat$matWithUniquePeptides
    writeToCommandLogFile("m <- mat$matWithUniquePeptides")
    }
        
    
    
    for(c in input$columnsForProteinDataset.box){
        newCol <- BuildColumnToProteinDataset(fData(rv$current.obj), m, c)
        cnames <- colnames(fData(rv$temp.aggregate))
        fData(rv$temp.aggregate) <- 
            data.frame(fData(rv$temp.aggregate), newCol)
        colnames(fData(rv$temp.aggregate)) <- c(cnames, c)
    }
    
    rv$current.obj <- rv$temp.aggregate
    rv$typeOfDataset <-rv$current.obj@experimentData@other$typeOfData
    name <- paste ("Aggregated", " - ", rv$typeOfDataset, sep="")
    rv$dataset[[name]] <- rv$current.obj
    
    
    
    
    ######
   # print(input$columnsForProteinDataset.box)
    l <- NULL
    for (i in input$columnsForProteinDataset.box)
        {
       l <- paste(l, paste("'",i, "',", sep=""))
        }
    writeToCommandLogFile(
        paste("columnsForProteinDataset <- c(",substr(l, 1, nchar(l)-1),")", sep="")
    )
    
    writeToCommandLogFile("for (c in columnsForProteinDataset) {")
    writeToCommandLogFile(
        "newCol <- BuildColumnToProteinDataset(fData(current.obj), m, c)")
    writeToCommandLogFile("cnames <- colnames(fData(temp.aggregate))")
    writeToCommandLogFile("fData(temp.aggregate) <- 
            data.frame(fData(temp.aggregate), newCol)")
    writeToCommandLogFile("colnames(fData(temp.aggregate)) <- c(cnames, c)")
    writeToCommandLogFile("}")
    writeToCommandLogFile("current.obj <- temp.aggregate")
    writeToCommandLogFile(
        paste("rv$dataset[['",name, "']] <- current.obj", sep="")
    )
    
    
    updateSelectInput(session, "datasets", 
                        choices = names(rv$dataset),
                        selected = name)
    UpdateLog(
        paste("Aggregation : peptides were aggregated into 
            proteins with method =",
            input$aggregationMethod,
            ", include Shared Peptides = ", input$checkSharedPeptides,
            ", protein id = ", input$proteinId, sep=" "),
        name)
    rv$temp.aggregate <- NULL
    
    } )
})

##' -- Validate the imputation ---------------------------------------
##' @author Samuel Wieczorek
observe({ 
    input$ValidImputation
    input$missing.value.algorithm
    if (is.null(input$ValidImputation) || (input$ValidImputation == 0)) 
    {return(NULL)}
    
    isolate({
    rv$typeOfDataset <-rv$current.obj@experimentData@other$typeOfData
    name <- paste ("Imputed", " - ", rv$typeOfDataset, sep="")
    
    rv$dataset[[name]] <- rv$current.obj
    #write command log file
    writeToCommandLogFile(
        paste("rv$dataset[['",name,"']] <- current.obj", sep="")
    )
    
    updateSelectInput(session, "datasets", 
                        choices = names(rv$dataset),
                        selected = name)
    UpdateLog(paste("Imputation with" ,
                    input$missing.value.algorithm,sep=" "),
                name)
    })
})

#-------------------------------------------------------------
output$showFDR <- renderText({
    rv$current.obj
    input$diffAnaMethod
    input$condition1
    input$condition2
    rv$seuilPVal
    rv$seuilLogFC
    input$numericValCalibration
    input$calibrationMethod
    
    
    if (is.null(input$diffAnaMethod) || (input$diffAnaMethod == "None")) 
    {return(NULL)}
    if (is.null(rv$current.obj)) {return(NULL)}
    if (is.null(input$condition1) || is.null(input$condition2) ) 
    {return(NULL)}
    if (is.null(rv$seuilLogFC) ||is.na(rv$seuilLogFC)  ) 
    {return(NULL)}
    if (is.null(rv$seuilPVal) || is.na(rv$seuilPVal)) { return (NULL)}
    
    
    if ((input$condition1 == input$condition2)) {return(NULL)}
    
    isolate({
    rv$current.obj
    
    if (  !(("logFC" %in% names(rv$current.obj@experimentData@other) ) && 
            ("P.Value"  %in% names(rv$current.obj@experimentData@other))))
    {
        data <- RunDiffAna()
        if (is.null(data)) {return (NULL)}
        m <- NULL
        if (input$calibrationMethod == "Benjamini-Hochberg") { m <- 1}
        else if (input$calibrationMethod == "numeric value") {
            m <- input$numericValCalibration} 
        else {m <- input$calibrationMethod }
        
        fdr <- diffAnaComputeFDR(data, rv$seuilPVal, rv$seuilLogFC, m)
        HTML(paste("<h4>FDR = ", round(100*fdr, digits=2)," % </h4>", sep=""))
    }
    })
})



output$histPValue <- renderPlot({
    
    if (is.null(rv$seuilPVal) ||
        is.null(rv$seuilLogFC) ||
        is.null(input$diffAnaMethod)
    ) {return(NULL)}
    if (input$condition1 == input$condition2) {return(NULL)}
    
    t <- NULL
    # Si on a deja des pVal, alors, ne pas recalculer avec ComputeWithLimma
    if (isContainedIn(c("logFC","P.Value"),names(fData(rv$current.obj)) ) ){
    t <- fData(rv$current.obj)[,"P.Value"]
    } else{
    data <- RunDiffAna()
    if (is.null(data)) {return (NULL)}
    t <- data$P.Value
    }
    
    
    hist(sort(1-t), breaks=80, col="grey")
    
})



output$numericalValForCalibrationPlot <- renderUI({
    input$calibrationMethod
    if (is.null(input$calibrationMethod)) {return(NULL)}
    
    if (input$calibrationMethod == "numeric value"){
    numericInput( "numericValCalibration","Proportion of TRUE null hypohtesis", 
                value = 0, min=0, max=1, step=0.05)
    }
})


output$calibrationResults <- renderUI({
    rv$calibrationRes
    rv$seuilLogFC
    input$condition1
    input$condition2
    input$diffAnaMethod
    rv$current.obj
    
    if (is.null( rv$calibrationRes)){return(NULL)}
    
    txt <- paste("Non-DA protein proportion = ", 
            round(100*rv$calibrationRes$pi0, digits = 2),"%<br>",
            "DA protein concentration = ", 
            round(100*rv$calibrationRes$h1.concentration, digits = 2),"%<br>",
            "Uniformity underestimation = ", 
            rv$calibrationRes$unif.under,"<br><br><hr>", sep="")
    HTML(txt)
    
})





catchToList <- function(expr) {
    val <- NULL
    myWarnings <- NULL
    myErrors <- NULL
    wHandler <- function(w) {
    myWarnings <<- c(myWarnings, w$message)
    invokeRestart("muffleWarning")
    }
    myError <- NULL
    eHandler <- function(e) {
    myError <<- c(myErrors, e$message)
    NULL
    }
    val <- tryCatch(withCallingHandlers(expr, warning = wHandler), 
                    error = eHandler)
    list(value = val, warnings = myWarnings, error=myError)
} 




output$calibrationPlot <- renderPlot({
    input$calibrationMethod
    input$numericValCalibration
    rv$seuilLogFC
    
    if (
        is.null(rv$seuilPVal) ||
        is.null(rv$seuilLogFC) ||
        is.null(input$diffAnaMethod)
    ) {return(NULL)}
    if (input$condition1 == input$condition2) {return(NULL)}
    
    
    t <- NULL
    method <- NULL
    # Si on a deja des pVal, alors, ne pas recalculer avec ComputeWithLimma
    if (isContainedIn(c("logFC","P.Value"),names(fData(rv$current.obj)) ) ){
    t <- fData(rv$current.obj)$P.Value
    t <- t[which(abs(fData(rv$current.obj)$logFC) >= 
                    rv$current.obj@experimentData@other$threshold.logFC)]
    method <- NULL
    } else{
    data <- RunDiffAna()
    if (is.null(data)) {return (NULL)}
    t <- data$P.Value
    t <- t[which(abs(data$logFC) >= rv$seuilLogFC)]
    method <- NULL
    }
    
    
    ll <- NULL

        if ((input$calibrationMethod == "numeric value") 
            && !is.null(input$numericValCalibration)) {
        #print("methode numeric value")
        ll <-catchToList(
            wrapperCalibrationPlot(t, input$numericValCalibration))
        rv$errMsgCalibrationPlot <- ll$warnings[grep( "Warning:", ll$warnings)]
        }
        else if (input$calibrationMethod == "Benjamini-Hochberg") {
        #print("methode BH")
        ll <-catchToList(wrapperCalibrationPlot(t, 1))
        rv$errMsgCalibrationPlot <- ll$warnings[grep( "Warning:", ll$warnings)]
        }else { 
        ll <-catchToList(wrapperCalibrationPlot(t, input$calibrationMethod))
        rv$errMsgCalibrationPlot <- ll$warnings[grep( "Warning:", ll$warnings)]
        }

})



output$errMsgCalibrationPlot <- renderUI({
    rv$errMsgCalibrationPlot
    if (is.null(rv$errMsgCalibrationPlot) ) {return(NULL)}
    
    txt <- NULL
    
    for (i in 1:length(rv$errMsgCalibrationPlot)) {
    txt <- paste(txt, "toto",rv$errMsgCalibrationPlot[i], "<br>", sep="")
    }
    
    div(HTML(txt), style="color:red")
    
})


output$errMsgCalibrationPlotAll <- renderUI({
    rv$errMsgCalibrationPlotAll
    if (is.null(rv$errMsgCalibrationPlotAll) ) {return(NULL)}
    
    txt <- NULL
    for (i in 1:length(rv$errMsgCalibrationPlotAll)) {
    txt <- paste(txt, rv$errMsgCalibrationPlotAll[i], "<br>", sep="")
    }

div(HTML(txt), style="color:red")
})


output$calibrationPlotAll <- renderPlot({
    if (
    is.null(rv$seuilPVal) ||
    is.null(rv$seuilLogFC) ||
    is.null(input$diffAnaMethod)
    ) {return(NULL)}
    if (input$condition1 == input$condition2) {return(NULL)}
    
    t <- NULL
    # Si on a deja des pVal, alors, ne pas recalculer avec ComputeWithLimma
    if (isContainedIn(c("logFC","P.Value"),names(fData(rv$current.obj)) ) ){
    t <- fData(rv$current.obj)$P.Value
    t <- t[which(abs(fData(rv$current.obj)$logFC) 
                >= rv$current.obj@experimentData@other$threshold.logFC)]
    
    } else{
    data <- RunDiffAna()
    if (is.null(data)) {return (NULL)}
    t <- data$P.Value
    t <- t[which(abs(data$logFC) >= rv$seuilLogFC)]
    
    }
    
    
    l <- NULL
    l <-catchToList(wrapperCalibrationPlot(t, "ALL")  )
    rv$errMsgCalibrationPlotAll <- l$warnings[grep( "Warning:", l$warnings)]

})



##' Get back to a previous object ---------------------------------------
##' @author Samuel Wieczorek
observe({ 
    input$datasets
    
    isolate({
    if (!is.null(input$datasets)) {
        rv$current.obj <- rv$dataset[[input$datasets]]
        if (!is.null( rv$current.obj))
            rv$typeOfDataset <- rv$current.obj@experimentData@other$typeOfData
        UpdateLog(
        paste("Current dataset has changed. Now, it is ",
            input$datasets, 
            sep=" "),
        input$datasets)
    }
    })
    
})


##' show intensity values of the MSnset object in a table
##' @author Samuel Wieczorek
output$viewExprs <- DT::renderDataTable({
    rv$current.obj
    input$nDigits
    if (is.null(rv$current.obj)) {return(NULL)}
    if (input$nDigits == T){nDigits = 1e100}else {nDigits = 3}
    data <- cbind(ID = rownames(fData(rv$current.obj)),
                        round(exprs(rv$current.obj), 
                                digits=nDigits))
    dat <- DT::datatable(data, 
                    options=list(pageLength=DT_pagelength,
                                orderClasses = TRUE,
                                autoWidth=FALSE)
                    )
    
    # %>% formatStyle(
    #                              colnames(data)[1:3],
    #                              valueColumns = 4,
    # backgroundColor = styleInterval( 0, c('orange','white'))
    #                            )
    return(dat)
} )



#----------------------------------------------
observe({ 
    input$ValidDiffAna
    # input$diffAnaMethod
    # input$condition1
    # input$condition2
    
    if ((input$ValidDiffAna == 0) ||  is.null(input$ValidDiffAna) ) {
        return(NULL)}
    if (input$condition1 == input$condition2) {return(NULL)}
    
    isolate({
    
    data <- RunDiffAna()

    if (is.null(data)) {return (NULL)}
    m <- NULL
    if (input$calibrationMethod == "Benjamini-Hochberg") { m <- 1}
    else {m <- input$calibrationMethod }



    fdr <- diffAnaComputeFDR(data, rv$seuilPVal, rv$seuilLogFC, m)

    temp <- diffAnaSave(rv$dataset[[input$datasets]],
                        data,
                        input$diffAnaMethod,
                        input$condition1,
                        input$condition2,
                        rv$seuilPVal, 
                        rv$seuilLogFC, 
                        fdr,
                        input$calibrationMethod)
    
    
    #rv$typeOfDataset <- rv$current.obj@experimentData@other$typeOfData
    name <- paste("DiffAnalysis.", input$diffAnaMethod, " - ", 
                rv$typeOfDataset, 
                sep="")
    
    rv$dataset[[name]] <- temp
    rv$current.obj <- temp
    updateSelectInput(session, "datasets", 
                    choices = names(rv$dataset),
                    selected = name)
    
    
    
    ####write command Log file
   writeToCommandLogFile(paste("pvalThresh <- ", rv$seuilPVal, sep=""))
    writeToCommandLogFile(paste("logFCthres <- ", rv$seuilLogFC,sep=""))
    writeToCommandLogFile(paste("calibMethod <- '", m,"'",sep=""))
    writeToCommandLogFile(
        "fdr <- diffAnaComputeFDR(data, pvalThresh, logFCthres, calibMethod)"
        )
    writeToCommandLogFile(
        paste("temp <- diffAnaSave(dataset[['", input$datasets, "']],",
              "data, method, cond1, cond2, pvalThresh, logFCthres, fdr, calibMethod)", sep=""
        )
    )
    writeToCommandLogFile("current.obj <- temp")
    writeToCommandLogFile(paste("dataset[['", name, "']] <- current.obj", sep=""))
    
    
    
    cMethod <- NULL
    if (input$calibrationMethod == "numeric value"){
cMethod <- paste("The proportion of true null
hypotheses was set to", input$numericValCalibration, sep= " ")}
    else {cMethod <-input$calibrationMethod }
    
    text <- paste("Dataset of ", 
                rv$typeOfDataset,
                ": differential analysis with", 
                input$diffAnaMethod, 
                "Selection with the following threshold values :logFC =",
                rv$seuilLogFC,
                    "The calibration was made with the method", cMethod,
                ", -log10(p-value) = ",
                rv$seuilPVal,
                "corresponding to a FDR = ", round(100*fdr, digits=2),
                sep=" ")
    UpdateLog(text,name)
    
    updateTabsetPanel(session, "abc", selected = "ValidateAndSaveAnaDiff")
    }) 
    
})


output$DiffAnalysisSaved <- renderUI({
    input$datasets
    rv$current.obj
    if (is.null(input$datasets) 
        || (length(grep("DiffAnalysis.",input$datasets)) !=1) ) {
        return(NULL)  }
    else if (grep("DiffAnalysis.",input$datasets) == 1 ) {
    h4("The differential analysis has been saved.")
    }
})


output$viewProcessingData <- DT::renderDataTable({
    rv$current.obj
    if (is.null(rv$current.obj)) {return(NULL)}
    
    data.frame(History=(rv$current.obj)@processingData@processing
                [-grep("Subset", (rv$current.obj)@processingData@processing)])
    
    
},
option=list(pageLength=DT_pagelength,
            orderClasses = TRUE,
            autoWidth=FALSE,
            dom = 'R<"clear">lfrtip',
            columnDefs = list(list(columns.width=c("60px"),
                        columnDefs.targets= c(list(0),list(1),list(2)))))
)


##' show pData of the MSnset object
##' @author Samuel Wieczorek
output$viewpData <- DT::renderDataTable({
    rv$current.obj
    if (is.null(rv$current.obj)) {return(NULL)}
    as.data.frame(pData(rv$current.obj))
},
option=list(pageLength=DT_pagelength,
            orderClasses = TRUE,
            autoWidth=FALSE,
            columnDefs = list(list(columns.width=c("60px"),
                            columnDefs.targets= c(list(0),list(1),list(2)))))
)

##' show fData of the MSnset object in a table
##' @author Samuel Wieczorek
output$viewfData <- DT::renderDataTable({
    rv$current.obj
    if (is.null(rv$current.obj)) {return(NULL)}
    as.data.frame(fData(rv$current.obj))
},
option=list(pageLength=DT_pagelength,
            orderClasses = TRUE,
            autoWidth=FALSE,
            columns.searchable=F,
            columnDefs = list(list(columns.width=c("60px"),
                            columnDefs.targets=c(list(0),list(1),list(2)))))
)



##' Visualisation of missing values table
##' @author Samuel Wieczorek
output$viewExprsMissValues <- DT::renderDataTable({
    rv$current.obj
    if (is.null(rv$current.obj)) {return(NULL)}
    as.data.frame(cbind(ID = rownames(fData(rv$current.obj)),
                        exprs(rv$current.obj)))
},

option=list(orderClasses = TRUE,
            autoWidth=FALSE,
            columns.searchable=F,
            pageLength = DT_pagelength,
            columnDefs = list(list(columns.width=c("60px"),
                            columnDefs.targets=c(list(0),list(1),list(2)))))
)


output$RenderLimmaCond1 <- renderUI({
    rv$current.obj
    if (is.null(rv$current.obj) ) {return(NULL)  }
    
    
    labels <- unique(pData(rv$current.obj)[,"Label"])
    labels <- setNames(as.list(labels),labels)
    condition1 <- labels[[1]]
    if ("logFC" %in% names(fData(rv$current.obj) )){
        condition1 <- rv$current.obj@experimentData@other$condition1
    }
    
    
    radioButtons("condition1", label = h4("Choose condition 1"), 
                choices = labels, 
                selected = condition1, 
                inline=F)

})



output$RenderLimmaCond2 <- renderUI({
    rv$current.obj
    if (is.null(rv$current.obj) ) {return(NULL)  }
    
    isolate({
    labels <- unique(pData(rv$current.obj)[,"Label"])
    labels <- setNames(as.list(labels),labels)
    condition2 <- labels[[2]]
    if ("logFC" %in% names(fData(rv$current.obj) )){
        condition2 <- rv$current.obj@experimentData@other$condition2
    }
    
    radioButtons("condition2", label = h4("Choose condition 2"), 
                    choices = labels , 
                    selected = condition2,
                    inline=F)
    })
})

##' @author Samuel Wieczorek
output$selectIDforExcelExport <- renderUI({
    rv$current.obj
    if (is.null(rv$current.obj) ) {return(NULL)  }
    selectInput("ID2XLS", "ID for XLS", 
                choices = colnames(fData(rv$current.obj)))
})


output$downloadMSnSet <- downloadHandler(
    filename = function() { 
    #input$nameExport
    if (input$fileformatExport == gFileFormatExport$excel) {
        paste(input$nameExport,gFileExtension$excel,  sep="")}
    else if (input$fileformatExport == gFileFormatExport$msnset)
    {
        paste(input$nameExport,gFileExtension$msnset,  sep="")}
    },
    content = function(file) {
    
    if (input$fileformatExport == gFileFormatExport$excel) {
        fname <- paste(input$nameExport,gFileExtension$excel,  sep="")
        
        writeMSnsetToExcel(rv$current.obj,input$nameExport, input$ID2XLS)
        file.copy(fname, file)
        file.remove(fname)
    }
    
    else if  (input$fileformatExport == gFileFormatExport$msnset) {
        fname <- paste(input$nameExport,gFileExtension$msnset,  sep="")
        saveRDS(rv$current.obj,file=fname)
        file.copy(fname, file)
        file.remove(fname)
    }
    }
)

# --- Shows in the sidebar panel the name of the opened file
output$fileopened <- renderUI({
    rv$current.obj
    rv$current.obj.name
    input$datasets
    
    if (is.null(rv$current.obj) || is.null(input$datasets)) {
    w <- paste(" ") }
    else {
    w <- paste("Current dataset is ", input$datasets, sep = "")
    }
    w
})

#########################################################
##' Show the widget for filters
##' @author Samuel Wieczorek
output$choixFiltres <- renderUI({
    input$file
    if (is.null(input$file)) {return(NULL)}
    rv$current.obj
    radioButtons("ChooseFilters","Filtering options",choices = gFiltersList)
    
})



output$chooseImputationMethod <- renderUI({
    if (is.null(rv$current.obj)) {return(NULL)}
    m <- NULL
    tag <- rv$current.obj@experimentData@other$imputation.method
    if (!is.null(tag)){ m <- tag}
    #print(m)
    #print(tag)
    #print(imputationAlgorithms[[m]])
    selectInput("missing.value.algorithm",
                "Choose algorithm",
                choices = names(imputationAlgorithms),
                selected = names(which(imputationAlgorithms == tag))
    )
    
})
#########################################################
##' Show the widget (slider input) for filtering
##' @author Samuel Wieczorek
output$seuilNADelete <- renderUI({ 
    input$ChooseFilters
    
    if (is.null(rv$current.obj)) {return(NULL)   }
    if (input$ChooseFilters==gFilterNone) {return(NULL)   }
    
    choix <- list()
    vMax <- GetMaxValueThresholdFilter()
    choix[[1]] <- 0
    for (i in 2:(vMax+1)){
        choix[[i]] <- i-1
    }
    ch <- NULL
    tag <- rv$current.obj@experimentData@other$mvFilter.threshold
    print(tag)
    if (!is.null(tag)) { ch <- tag}
    else {ch <- choix[[1]]}
    selectInput(inputId = "seuilNA", 
                label = "Keep lines with at least x intensity values", 
                choices = choix, 
                selected = ch)
    
})

#########################################################
output$MSnsetView <- renderPrint({
    rv$current.obj
    if (is.null(rv$current.obj)) {return(NULL)  }
    rv$current.obj
})




output$warningNonUniqueID <- renderUI({
    input$idBox
    rv$tab1
if (is.null(rv$tab1)) {return(NULL)  }
    if (is.null(input$idBox) || (input$idBox =="")) {return(NULL)  }
    
    t <- (length(rv$tab1[, input$idBox]) 
        == length(unique(rv$tab1[, input$idBox])))
    
    if (!t){
    text <- "<font color=\"red\">
            Warning ! Your ID contains duplicate data.
            Please choose another one."

    HTML(text)
    }
    
})


#########################################################
output$id <- renderUI({
    rv$tab1
    if (is.null(rv$tab1)) {return(NULL)  }
    
    .choices <- c("",colnames(rv$tab1))
    names(.choices) <- c("",colnames(rv$tab1))
    selectInput("idBox", label = "", choices = .choices , selected = NULL)
    
})

#######################################
observe({
    input$createMSnsetButton
    if(is.null(input$createMSnsetButton) || (input$createMSnsetButton == 0)) 
        {return(NULL)}
    
    isolate({
    input$hot
    input$filenameToCreate
    # input$file1
    #inFile1 <- input$file1
    rv$tab1
    
    indexForEData <- match(input$eData.box, colnames(rv$tab1))
    indexForFData <- seq(1,ncol(rv$tab1))[-indexForEData]
    
    indexForIDBox <- match(input$idBox, colnames(rv$tab1))
    if (is.na(indexForIDBox) || length(indexForIDBox) == 0) {
        indexForIDBox <- NULL}
    
    metadata <- hot_to_r(input$hot)
    logData <- (input$checkDataLogged == "no")
    
    rv$current.obj <- createMSnset(rv$tab1, 
                                    metadata, 
                                    indexForEData, 
                                    indexForFData, 
                                    indexForIDBox,
                                    logData, 
                                    input$replaceAllZeros,
                                    pep_prot_data = input$typeOfData
                                    )
    rv$current.obj.name <- input$filenameToCreate
    loadObjectInMemoryFromConverter()
    
    
    updateTabsetPanel(session, "tabImport", selected = "Convert")
    
    })
})

#########################################################
output$eData <- renderUI({
    input$file1
    rv$tab1
    #if (is.null(input$file1)) {return(NULL)  }
    if (is.null(rv$tab1)) {return(NULL)  }
    
    choices <- colnames(rv$tab1)
    names(choices) <- colnames(rv$tab1)
    selectizeInput("eData.box",
                    label = "",
                    choices = choices,
                    multiple = TRUE, width='500px')
    
})


######################################################### 
output$columnsForProteinDataset <- renderUI({
    rv$current.obj
    if (is.null(rv$current.obj)) {return(NULL)  }
    
    choices <- colnames(fData(rv$current.obj))
    names(choices) <- colnames(fData(rv$current.obj))
    selectizeInput("columnsForProteinDataset.box",
                    label = "",
                    choices = choices,
                    multiple = TRUE, width='200%')
    
})



#########################################################
output$labelsNames <- renderUI({
    #input$openButton
    #if (input$openButton == 0) {return(NULL) }
    
    input$file1
    input$LabelField
    input$eData.box
    if (is.null(input$file1)) {return(NULL)  }
    
    w <- ""
    
    for(i in 1:length(input$eData.box)) {
    t <- strsplit(input$eData.box[i],".", fixed=T)
    w <- paste(w,  textInput(paste("label",i,sep=""), 
                                NULL, 
                                value=t[[1]][as.numeric(input$LabelField)]))
    }
    HTML(w)
    
})

#########################################################
output$ChooseLabelField <- renderUI({
    #input$openButton
    #if (input$openButton == 0) {return(NULL) }
    input$file1
    input$eData.box
    if (is.null(input$file1)) {return(NULL)  }
    
    t <- strsplit(input$eData.box[1],".", fixed=T)
    selectInput("LabelField", "Choose label field", 
                choices=  as.character(1:length(t[[1]])))
})


GetOneField <- function(col, liste){
    for(i in 1:length(liste)) {
    liste[[i]] <- strsplit(liste[[i]],".", fixed=T)
    }
    
    df <- data.frame(matrix(unlist(liste), nrow=length(liste), byrow=T))
    return(as.vector(df[,col]))
}

########################################################
output$pDataField1 <- renderText({
    input$file1
    input$eData.box
    if (is.null(input$file1)) {return(NULL)  }
    
    t <- GetOneField(1,input$eData.box)
    w <- NULL
    for (i in 1:length(t)){
    w <- paste(w,t[i], "<br>", sep="")
    }
    HTML(w)
    
})

########################################################
output$pDataField2 <- renderText({
    input$file1
    input$eData.box
    if (is.null(input$file1)) {return(NULL)  }
    
    t <- GetOneField(2,input$eData.box)
    w <- NULL
    for (i in 1:length(t)){
    w <- paste(w,t[i], "<br>", sep="")
    }
    HTML(w)
})

########################################################
output$pDataField3 <- renderText({
    input$file1
    input$eData.box
    if (is.null(input$file1)) {return(NULL)  }
    
    t <-GetOneField(3,input$eData.box)
    w <- NULL
    for (i in 1:length(t)){
    w <- paste(w,t[i], "<br>", sep="")
    }
    HTML(w)
    
})

########################################################
output$pDataField4 <- renderText({
    input$file1
    input$eData.box
    if (is.null(input$file1)) {return(NULL)  }
    
    t <- GetOneField(4,input$eData.box)
    w <- NULL
    for (i in 1:length(t)){
    w <- paste(w,t[i], "<br>", sep="")
    }
    HTML(w)
    
})


output$logSession <- DT::renderDataTable({
    rv$text.log
    if (is.null(rv$text.log)) {return (NULL)}
    rv$text.log}, 
                options=list(pageLength=DT_pagelength,
                                orderClasses = TRUE,
                                autoWidth=FALSE)
)

#########################################################
output$References <- renderText({
    HTML("<strong><font size=\"5\">HELP</font></strong>
        <br><hr color:\"blue\"><br>

        <strong><font size=\"4\">User manual:</font></strong>
        <a href=\"https://www.bioconductor.org/packages/release/bioc/vignettes/Prostar/inst/doc/Prostar_UserManual.pdf\"
                                title=\"here\" target=\"_blank\">here</a>
        <br><br>

        <strong><font size=\"4\">Tutorial:</font></strong>
        Coming soon.<br><br>

        <strong><font size=\"4\">Contact:</font></strong><br>
        If you need any help, but also if you wish to make comments 
        or suggestions, please contact Samuel Wieczorek, Florence Combes or 
        Thomas Burger (firstname.lastname@cea.fr)<br><br>


        <strong><font size=\"4\">Additional ressources:</font></strong>
        <ul>
        <li> ProStaR reference manual:  
<a href=\"https://www.bioconductor.org/packages/release/bioc/manuals/Prostar/man/Prostar.pdf\"
            title=\"here\" target=\"_blank\">here</a>
        </li>
        <li> DAPAR reference manual: <a href=\"https://www.bioconductor.org/packages/release/bioc/manuals/DAPAR/man/DAPAR.pdf\"
                                title=\"here\" target=\"_blank\">here</a>


        </li>
        <li> MSnbase package: <a href=\"https://www.bioconductor.org/packages/release/bioc/html/MSnbase.html\" title=\"here\" target=\"_blank\">here</a>
        </li>
        <li> Cp4p tutorial: <a href=\"https://cran.r-project.org/web/packages/cp4p/cp4p.pdf\"
                                title=\"here\" target=\"_blank\">here</a>
        </li>
        </ul>
<br><br>

        <strong><font size=\"4\">References:</font></strong>
        <ul>
        <li> S. Wieczorek, F. Combes, C. Lazar, Q. Giai-Gianetto, L. Gatto, 
        A. Dorffer, A.-M. Hesse, Y. Coute, M. Ferro, C. Bruley, T. Burger. 
        \"DAPAR & ProStaR: software to perform statistical analyses in 
        quantitative discovery proteomics\", under (minor) revision, 
        <i>Bioinformatics</i>, 2016
        </li>
        <li> C. Lazar, L. Gatto, M. Ferro, C. Bruley, T. Burger. Accounting 
        for the multiple natures of missing values in label-free quantitative 
        proteomics datasets to compare imputation strategies. <i>Journal of 
        Proteome Research</i>, accepted for publication, February 2016. 
        </li>
        <li> Q. Giai Gianetto, F. Combes, C. Ramus, C. Bruley, Y. Couté, 
        T. Burger. Calibration Plot for Proteomics (cp4p): A graphical tool 
        to visually check the assumptions underlying FDR control in 
        quantitative experiments. <i>Proteomics</i>, 16(1):29-32, 2016. 

        </li>
        </ul>
        ")
})



output$helpTextDataID <- renderUI({
    input$typeOfData
    
    t <- ""
    if (input$typeOfData == "protein") {t <- "proteins"}
    else if (input$typeOfData == "peptide") {t <- "peptides"}
    txt <- paste ("Please select among the columns ofyour data the one that 
                corresponds to a unique ID of the ", t, ".", sep=" ")
    helpText(txt)
    
})


##' Quick overview of the MSnbase object
##' @author Florence Combes
output$overview <- renderUI({
    rv$current.obj
    rv$typeOfDataset
    if (is.null(rv$current.obj)) {return(NULL)    }
    
    isolate({
    rv$current.obj
    rv$typeOfDataset
    NA.count <- apply(data.frame(exprs(rv$current.obj)), 
                        2, 
                        function(x) length(which(is.na(data.frame(x))==TRUE)) )
    pourcentage <- 100 * round(sum(NA.count)/
                                    (dim(exprs(rv$current.obj))[1]*
                                    dim(exprs(rv$current.obj))[2]), digits=4)
    d <- "lines"
    #print("rv$typeOfDataset")
    #print(rv$typeOfDataset)
    if (rv$typeOfDataset == "peptide") {d <- "peptides"}
    else if (rv$typeOfDataset == "protein") {d <- "proteins"}
    else {d <- "analytes"}
    
    nb.empty.lines <- sum(apply(
        is.na(as.matrix(exprs(rv$current.obj))), 1, all))
    tags$ul(
        # if (rv$typeOfData != "") {tags$li(paste("This is ", rv$typeOfData, 
        #                                            "dataset.", sep=" "))}, 
        tags$li(paste("There are", dim(exprs(rv$current.obj))[2], 
                    "samples in your data.", sep=" ")),
        
        tags$li(paste("There are", dim(exprs(rv$current.obj))[1], d,
                    "in your data.", sep=" ")), 
        tags$li(paste("Percentage of missing values:",
                    pourcentage , "%", sep=" ")),
        tags$li(paste("Number of lines with only NA values =",
                    nb.empty.lines , sep=" "))
    )

    })
})


output$overviewNewData <- renderUI({
    rv$current.obj
    if (is.null(rv$current.obj)) {return(NULL)}
    
    isolate({
    txt1 <- paste("There is", 
                dim(exprs(rv$current.obj))[2],
                "samples in your data.")
    txt2 <- paste("There is",
                dim(exprs(rv$current.obj))[1], 
                "lines in your data.")
    
    NA.count<-apply(data.frame(exprs(rv$current.obj)), 
                    2, 
                    function(x) length(which(is.na(data.frame(x))==TRUE)) )
    pourcentage <- 100 * round(sum(NA.count)/
                                    (dim(exprs(rv$current.obj))[1]*
                                    dim(exprs(rv$current.obj))[2]), digits=4)
    txt3 <- paste("Percentage of missing values:",pourcentage , "%")
    
    nb.empty.lines <- sum(apply(
        is.na(as.matrix(exprs(rv$current.obj))), 1, all))
    txt4 <- NULL
    if (nb.empty.lines > 0){
        
        if( nb.empty.lines > 1){
        verb <- "are"
        plurial <- "s"} else {
            verb <- "is"
            plurial <- ""}
        
        txt4 <- paste("There ", verb, " : ",
                    nb.empty.lines ," line",plurial," with only NA values !!")
    }
    
    tags$ul(
        tags$li(txt1), 
        tags$li(txt2), 
        tags$li(txt3),
        if (!is.null(txt4)){tags$li(txt4)}
    )

    })
})




output$GlobalPieChart <- renderPlot({
    rv$current.obj
    input$idBoxContaminants
    input$idBoxReverse
    input$prefixContaminants
    input$prefixReverse
    
    
    if (is.null(rv$current.obj)){return(NULL)}
    
    proportionConRev(rv$current.obj,
            input$idBoxContaminants, 
            input$prefixContaminants, 
            input$idBoxReverse,
            input$prefixReverse)
})


##' distribution of missing values in current.obj
##' @author Samuel Wieczorek
output$histoMV_DS <- renderPlot({
    rv$current.obj
    if (is.null(rv$current.obj)){return(NULL)}
    
    wrapper.mvHisto(rv$current.obj)
})



##' distribution of missing values in current.obj
##' @author Samuel Wieczorek
output$histo.missvalues.per.lines_DS <- renderPlot({
    rv$current.obj
    if (is.null(rv$current.obj)){return(NULL)}
    wrapper.mvPerLinesHisto(rv$current.obj, 
                            c(2:length(colnames(pData(rv$current.obj)))))
})

##' distribution of missing values in current.obj
##' @author Samuel Wieczorek
output$histo.missvalues.per.lines.per.conditions_DS <- renderPlot({
    rv$current.obj
    if (is.null(rv$current.obj)){return(NULL)}
    wrapper.mvPerLinesHistoPerCondition(rv$current.obj, 
                    c(2:length(colnames(pData(rv$current.obj)))))
})



##' distribution of missing values in current.obj
##' @author Samuel Wieczorek
output$histoMV <- renderPlot({
    rv$current.obj
    if (is.null(rv$current.obj)){return(NULL)}
    
    wrapper.mvHisto(rv$current.obj)
})



##' distribution of missing values in current.obj
##' @author Samuel Wieczorek
output$histo.missvalues.per.lines <- renderPlot({
    rv$current.obj
    if (is.null(rv$current.obj)){return(NULL)}
    wrapper.mvPerLinesHisto(rv$current.obj, 
                    c(2:length(colnames(pData(rv$current.obj)))))
})

##' distribution of missing values in current.obj
##' @author Samuel Wieczorek
output$histo.missvalues.per.lines.per.conditions <- renderPlot({
    rv$current.obj
    if (is.null(rv$current.obj)){return(NULL)}
    wrapper.mvPerLinesHistoPerCondition(rv$current.obj, 
                            c(2:length(colnames(pData(rv$current.obj)))))
})

##' xxxxxxxxxxxxxxxxxxxxxxxx
##' @author Samuel Wieczorek
output$showImageNA <- renderPlot({
    rv$current.obj
    if (is.null(rv$current.obj)){return(plot.new())}
    
    wrapper.mvImage(rv$current.obj)
})


##########################
output$ChooseLegendForNormTabPanel <- renderUI({
    rv$current.obj
    if (is.null(rv$current.obj)){return(NULL)}
    .names <- colnames(pData(rv$current.obj))[-1]
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

output$choose_Normalization_Test <- renderUI({
    rv$current.obj
    if (is.null(rv$current.obj)) { return (NULL)}
    
    #isolate({
    
    # check if the normalisation has already been performed
    m <- NULL
    if( !is.null(rv$current.obj@experimentData@other$normalizationFamily)
        && !is.null(rv$current.obj@experimentData@other$normalizationMethod)) { 
        family <- rv$current.obj@experimentData@other$normalizationFamily
        method <- rv$current.obj@experimentData@other$normalizationMethod
        m <- paste(family, method, sep=" - ")
    }
    if (GetNbNA() == 0){
    choices <- normMethods
    } else {
    choices <- normMethods
    } 
    
    selectInput("normalization.method", 
                "Choose normalization method", 
                names(choices), 
                selected = m)
})


# Check boxes
output$choose_Normalization_2 <- renderUI({
    input$normalization.family
    if(is.null(input$normalization.family) || 
        ( input$normalization.family == "None"))
    return()
    
    outVar <- normalization.methods[[which(names(normalization.methods) == 
                                            input$normalization.family)]]
    selectInput("normalization.method", "Choose normalization method",
                choices  = outVar)
})


##' boxplot and densityplot of intensities in current.obj 
##' in the normalization panel
##' @author Samuel Wieczorek
output$NormData <- renderPlot({
    rv$current.obj
    input$graph.choice.normalization.tab
    input$legendXAxisNormTabPanel
    input$legendXAxis
    if (is.null(rv$current.obj)){return(plot.new())}
    
    typeOfGraphics <- input$graph.choice.normalization.tab
    if (typeOfGraphics == "boxplot"){
    input$legendXAxisNormTabPanel
    input$legendXAxis
    rv$current.obj
    
    #.axis <- match(input$legendXAxisNormTabPanel,
    #colnames(pData(rv$current.obj)))
    .axis <- input$legendXAxis
    wrapper.boxPlotD(rv$current.obj,.axis)
    
    }else if (typeOfGraphics == "densityplot") {
    wrapper.densityPlotD(rv$current.obj, 
                    unique(pData(rv$current.obj)[,"Label"]),
                    NULL)
    }
})

#------------------------------------------------------
output$ChooseLegendForAxis <- renderUI({
    rv$current.obj
    if (is.null(rv$current.obj)){return(NULL)}
    isolate(rv$current.obj)
    .names <- colnames(pData(rv$current.obj))[-1]
    tags$head(tags$link(rel="stylesheet", type="text/css", 
                        href="css/overrides.css"))
    
    checkboxGroupInput("legendXAxis",
                        label = "Choose data to show in legend",
                        choices = .names,
                        selected = .names[1])
})


#------------------------------------------------------
output$ChooseLegendForAxis_DS <- renderUI({
    rv$current.obj
    if (is.null(rv$current.obj)){return(NULL)}
    isolate(rv$current.obj)
    .names <- colnames(pData(rv$current.obj))[-1]
    tags$head(tags$link(rel="stylesheet", type="text/css", 
                        href="css/overrides.css"))
    
    checkboxGroupInput("legendXAxis_DS",
                        label = "Choose data to show in legend",
                        choices = .names,
                        selected = .names[1])
})



##' boxplot of intensities in current.obj
##' @author Samuel Wieczorek
output$viewBoxPlot_DS <- renderPlot({
    input$legendXAxis_DS
    rv$current.obj
    #input$whichGroup2Color
    
    if (is.null(rv$current.obj)){return(NULL)}
    # print(input$legendXAxis_DS)
    #print(str(input$legendXAxis_DS))
    wrapper.boxPlotD(rv$current.obj,  input$legendXAxis_DS)
    
    
})


##' Distribution of intensities in current.obj
##' @author Samuel Wieczorek
output$viewDensityplot_DS <- renderPlot({
    rv$current.obj
    input$lab2Show
    input$whichGroup2Color
    if (is.null(rv$current.obj) || (length(input$lab2Show) == 0))
    {return(plot.new())}
    
    
    if (input$whichGroup2Color == "Condition"){
    labs <- pData(rv$current.obj)[,"Label"]
    }else {
    labs <- paste(pData(rv$current.obj)[,"Label"],
                    pData(rv$current.obj)[,"Bio.Rep"],
                    pData(rv$current.obj)[,"Tech.Rep"],
                    pData(rv$current.obj)[,"Analyt.Rep"],
                    sep= "_")
    }
    
    wrapper.densityPlotD(rv$current.obj, labs, as.numeric(input$lab2Show), 
                        input$whichGroup2Color)
})





##' boxplot of intensities in current.obj
##' @author Samuel Wieczorek
output$viewBoxPlot <- renderPlot({
    input$legendXAxis
    rv$current.obj
    input$whichGroup2Color
    
    if (is.null(rv$current.obj)){return(plot.new())}
    
    if( is.null(input$legendXAxis) || is.null(input$whichGroup2Color))
    { wrapper.boxPlotD(rv$current.obj)
    } else {
    wrapper.boxPlotD(rv$current.obj,input$legendXAxis ,input$whichGroup2Color)
    }
    
})


##' Distribution of intensities in current.obj
##' @author Samuel Wieczorek
output$viewDensityplot<- renderPlot({
    rv$current.obj
    input$lab2Show
    input$whichGroup2Color
    
    if (is.null(rv$current.obj) ){return(plot.new())}
    
    
    if(is.null(input$whichGroup2Color) && is.null(input$lab2Show))
    { 
    labs <- pData(rv$current.obj)[,"Label"]
    wrapper.densityPlotD(rv$current.obj, labs)
    } else {
    if (input$whichGroup2Color == "Condition")
            {
            labs <- pData(rv$current.obj)[,"Label"]
    }else {
            labs <- paste(pData(rv$current.obj)[,"Label"],
                    pData(rv$current.obj)[,"Bio.Rep"],
                    pData(rv$current.obj)[,"Tech.Rep"],
                    pData(rv$current.obj)[,"Analyt.Rep"],
                    sep= "_")
            }
    wrapper.densityPlotD(rv$current.obj, labs, as.numeric(input$lab2Show),
                        input$whichGroup2Color)
    }

})

#######################
output$viewComparisonNorm<- renderPlot({
    rv$current.obj
    input$whichGroup2Color
    input$lab2Show
    
    if (is.null(rv$current.obj) ){return(plot.new())}
    
    if(is.null(input$whichGroup2Color) && is.null(input$lab2Show))
    { 
    labs <- pData(rv$current.obj)[,"Label"]
    compareNormalizationD(exprs(rv$dataset[[input$datasets]]), 
                        exprs(rv$current.obj), 
                        labs)
    } else {
    if (input$whichGroup2Color == "Condition"){
        labs <- pData(rv$current.obj)[,"Label"]
    }else {
        labs <- paste(pData(rv$current.obj)[,"Label"],
                    pData(rv$current.obj)[,"Bio.Rep"],
                    pData(rv$current.obj)[,"Tech.Rep"],
                    pData(rv$current.obj)[,"Analyt.Rep"],
                    sep= "_")
    }
    compareNormalizationD(exprs(rv$dataset[[input$datasets]]), 
                            exprs(rv$current.obj), 
                            labs,
                            as.numeric(input$lab2Show), 
                            input$whichGroup2Color)
    }
    
    
    
})






##' boxplot of intensities in current.obj
##' @author Samuel Wieczorek
output$viewNAbyMean <- renderPlot({
    rv$current.obj
    input$seuilMNAR
    if (is.null(rv$current.obj)){return(plot.new())}
    wrapper.mvTypePlot(rv$current.obj,input$seuilMNAR)
})


##' distribution of the variance in current.obj
##' 
##' @author Samuel Wieczorek
output$viewDistVariance <- renderPlot({
    rv$current.obj
    if (is.null(rv$current.obj))
    {return(plot.new())}
    
    wrapper.varianceDistD(rv$current.obj)
})


##' Draw a correlation matrix of intensities in current.obj
##' 
##' @author Samuel Wieczorek
output$corrMatrix <- renderPlot({
    input$expGradientRate
    rv$current.obj
    if (is.null(rv$current.obj)){return(NULL)}
    if (is.null(input$expGradientRate)){return(NULL)}
    wrapper.corrMatrixD(rv$current.obj, rate = input$expGradientRate)
})


##' Draw a heatmap of current data
##' 
##' @author Samuel Wieczorek
output$heatmap <- renderPlot({
    rv$current.obj
    input$linkage
    input$distance

    
    if (is.null(rv$current.obj) 
        || is.null(input$linkage) 
        || is.null(input$distance)) {return(NULL)
#return(plot.new())
}
    
    # if (getNumberOfEmptyLines(exprs(rv$current.obj)) != 0) {
    # return(NULL)
    # # plot.new()
    # }
    # else {  
    # plot.new()
    wrapper.heatmapD(rv$current.obj,
                    input$distance, 
                    input$linkage,
                    TRUE) 
    # buildHeatmapPlot()
    #}
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

##' Select the labels to be highlighted in densityplots
##' @author Samuel Wieczorek
output$nGroup_DS <- renderUI({
    rv$current.obj
    if (is.null(rv$current.obj) ) {return(NULL) }
    
    radioButtons("whichGroup2Color",
                "Plot to show",
                choices=list("By condition" = "Condition",
                            "By replicate" = "Replicate"))
    
})



output$topNOption <- renderUI({
    input$aggregationMethod
    if(is.null(input$aggregationMethod )) {return(NULL)}
   
    if(input$aggregationMethod == gAgregateMethod[["sum on top n"]])
        numericInput("nTopn", "nTopn",value = NULL, min = 0)

})

##' Select the labels to show in densityplots
##' @author Samuel Wieczorek
output$nShow_DS <- renderUI({
    rv$current.obj
    #input$whichGroup2Color
    if (is.null(rv$current.obj) ) {return(NULL) }
    
    isolate({
    rv$current.obj
    labs <- paste(pData(rv$current.obj)[,"Label"],
                    pData(rv$current.obj)[,"Bio.Rep"],
                    pData(rv$current.obj)[,"Tech.Rep"],
                    pData(rv$current.obj)[,"Analyt.Rep"],
                    sep= "_")
    
    #label.names <- unique(pData(rv$current.obj)[,"Label"])
    label.names <- setNames(as.list(c(1:length(labs))),labs)
    
    
    checkboxGroupInput("lab2Show"
                        , label = "Select data to show"
                        , choices = label.names
                        , selected = unlist(label.names))
    
    })
})




##' Select the labels to show in densityplots
##' @author Samuel Wieczorek
output$nShow <- renderUI({
    rv$current.obj
    #input$whichGroup2Color
    if (is.null(rv$current.obj) ) {return(NULL) }
    
    isolate({
    rv$current.obj
labs <- paste(pData(rv$current.obj)[,"Label"],
pData(rv$current.obj)[,"Bio.Rep"],
                            pData(rv$current.obj)[,"Tech.Rep"],
                            pData(rv$current.obj)[,"Analyt.Rep"],
                            sep= "_")

    #label.names <- unique(pData(rv$current.obj)[,"Label"])
    label.names <- setNames(as.list(c(1:length(labs))),labs)


    checkboxGroupInput("lab2Show"
                        , label = "Select data to show"
                        , choices = label.names
                        , selected = unlist(label.names))
    
    })
})

output$equivPVal <- renderText ({
    input$seuilPVal
    input$diffAnaMethod
    rv$current.obj
    if (is.null(rv$current.obj)){return(NULL)}
    if (is.null(input$condition1) || is.null(input$condition2))
    {return(NULL)}
    if (is.null(input$seuilPVal)){return(NULL)}
    if (is.null(input$diffAnaMethod) || (input$diffAnaMethod == "None"))
    {return(NULL)}
    if ((input$condition1 == input$condition2)) {return(NULL)}
    
    HTML(paste("<h4>(p-value = ",
                signif(10^(- (input$seuilPVal)), digits=3), ") </h4>", sep=""))
})


output$equivLog10 <- renderText ({
    input$test.threshold
    rv$current.obj
    input$diffAnaMethod
    if (is.null(input$diffAnaMethod)){return(NULL)}
    if (is.null(rv$current.obj)){return(NULL)}
    if (is.null(input$condition1) || is.null(input$condition2)){return(NULL)}
    if (is.null(input$test.threshold)){return(NULL)}
    
    HTML(paste("<h4>-log10 (p-value) = ",
                signif(- log10(input$test.threshold/100), digits=1),
                "</h4>", sep=""))
})


##update diffAna Panel
observe({
    rv$current.obj
    if (is.null(rv$current.obj)){return(NULL)}
    
    if ("P.Value"  %in% names(fData(rv$current.obj))){
    
    updateSelectInput(session,"diffAnaMethod",
                        selected =  rv$current.obj@experimentData@other$method)
    
    updateNumericInput(session,
                    "seuilPVal",
                    min = 0,
                    max = max(-log10(fData(rv$current.obj)$P.Value)),
                    value = rv$current.obj@experimentData@other$seuil.p.value, 
                    step=0.1)
    
    updateNumericInput(session,
                    "seuilLogFC", 
                    min = 0, 
                    max = max(abs(fData(rv$current.obj)$logFC)), 
                    value = rv$current.obj@experimentData@other$seuil.logFC, 
                    step=0.1)
    }
    
})

observe({
    if (!is.null(input$seuilPVal)){rv$seuilPVal <- input$seuilPVal}
    # print(rv$seuilPVal)
})

observe({
    if (!is.null(input$seuilLogFC)){rv$seuilLogFC <- input$seuilLogFC}
    # print(rv$seuilLogFC)
})


output$nbSelectedItems <- renderUI({
    rv$seuilPVal
    rv$seuilLogFC
    input$condition1
    input$condition2
    input$diffAnaMethod
    rv$current.obj

    if (is.null( input$diffAnaMethod) || (input$diffAnaMethod == "None")){
        return(NULL)}
    p <- NULL
    if ("P.Value"  %in% names(fData(rv$current.obj))){
        p$P.Value <- fData(rv$current.obj)$P.Value
        p$logFC <- fData(rv$current.obj)$logFC
    }else {
        p <- RunDiffAna()
    }
    if (is.null(p)) {return (NULL)}
    upItemsPVal <- NULL
    upItemsLogFC <- NULL
    
    if (!is.null(rv$seuilPVal)) {
        upItemsPVal <- which(-log10(p$P.Value) >= rv$seuilPVal)}
    
    if (!is.null(rv$seuilPVal)) {
        upItemsLogFC <- which(abs(p$logFC) >= rv$seuilLogFC)}
    
    
    nbTotal <- nrow(exprs(rv$current.obj))
    nbSelected <- NULL
    t <- NULL
    
    if (!is.null(rv$seuilPVal) && !is.null(rv$seuilLogFC) ) {
        t <- intersect(upItemsPVal, upItemsLogFC)}
    else if (!is.null(rv$seuilPVal) && is.null(rv$seuilLogFC) ) {
        t <- upItemsPVal}
    else if (is.null(rv$seuilPVal) && !is.null(rv$seuilLogFC) ) {
        t <- upItemsLogFC}
    
    nbSelected <- length(t)
    
    txt <- paste("Total number of ",rv$typeOfDataset, "(s) = ", 
                nbTotal,"<br>",
                "Number of selected ",rv$typeOfDataset, "(s) = ", 
                nbSelected,"<br>",
                "Number of non selected ",rv$typeOfDataset, "(s) = ", 
                (nbTotal-nbSelected), sep="")
    HTML(txt)
})

output$nbSelectedItemsStep3 <- renderUI({
    rv$seuilPVal
    rv$seuilLogFC
    input$condition1
    input$condition2
    input$diffAnaMethod
    rv$current.obj
    
    if (is.null( input$diffAnaMethod) || (input$diffAnaMethod == "None")){
        return(NULL)}
    
    p <- NULL
    if ("P.Value"  %in% names(fData(rv$current.obj))){
        p$P.Value <- fData(rv$current.obj)$P.Value
        p$logFC <- fData(rv$current.obj)$logFC
    }else {
        p <- RunDiffAna()
    }
    
    if (is.null(p)) {return (NULL)}
    upItemsPVal <- NULL
    upItemsLogFC <- NULL
    
    if (!is.null(rv$seuilPVal)) {
        upItemsPVal <- which(-log10(p$P.Value) >= rv$seuilPVal)}
    
    if (!is.null(rv$seuilPVal)) {
        upItemsLogFC <- which(abs(p$logFC) >= rv$seuilLogFC)}
    
    
    nbTotal <- nrow(exprs(rv$current.obj))
    nbSelected <- NULL
    t <- NULL
    
    if (!is.null(rv$seuilPVal) && !is.null(rv$seuilLogFC) ) {
        t <- intersect(upItemsPVal, upItemsLogFC)}
    else if (!is.null(rv$seuilPVal) && is.null(rv$seuilLogFC) ) {
        t <- upItemsPVal}
    else if (is.null(rv$seuilPVal) && !is.null(rv$seuilLogFC) ) {
        t <- upItemsLogFC}
    
    nbSelected <- length(t)
    
    txt <- paste("Total number of ", rv$typeOfDataset, " = ", 
                nbTotal,"<br>",
                "Number of selected ", rv$typeOfDataset, " = ", 
                nbSelected,"<br>",
                "Number of non selected ", rv$typeOfDataset, " = ", 
                (nbTotal-nbSelected), sep="")
    HTML(txt)
})




observe({
    rv$current.obj
    if (is.null(rv$current.obj)){return(NULL)}
    isolate({
    
    #Si on a deja des pVal, alors, ne pas recalculer 
    if ("logFC" %in% names(fData(rv$current.obj) )){
        updateNumericInput(session, 
            "seuilLogFC",
            value= rv$current.obj@experimentData@other$threshold.logFC)
        updateNumericInput(session, 
            "seuilPVal",
            value= rv$current.obj@experimentData@other$threshold.p.value)
        updateSelectInput(session,
            "diffAnaMethod",
            selected = rv$current.obj@experimentData@other$method)
        updateRadioButtons(session,
            "condition1",
            selected = rv$current.obj@experimentData@other$condition1)
        updateRadioButtons(session,
            "condition2",
            selected = rv$current.obj@experimentData@other$condition2)
        updateRadioButtons(session,
            "calibrationMethod",
            selected = rv$current.obj@experimentData@other$calibrationMethod)
    }
    
})
    
})




#-------------------------------------------------------------------
output$volcanoplot <- renderPlot({
    rv$seuilPVal
    rv$seuilLogFC
    input$condition1
    input$condition2
    input$diffAnaMethod
    
    if (is.null(input$condition1) ||is.null(input$condition2))
    {return(NULL)}
    if (input$condition1 == input$condition2) {return(NULL)}
    if (is.null(rv$seuilLogFC) || is.na(rv$seuilLogFC)) { return (NULL)}
    
    isolate({
    
    #Si on a deja des pVal, alors, ne pas recalculer 
    if ("logFC" %in% names(fData(rv$current.obj) )){
        
        cond <- c(rv$current.obj@experimentData@other$condition1,
                  rv$current.obj@experimentData@other$condition2)
        
        
        diffAnaVolcanoplot(fData(rv$current.obj)$logFC,
                        fData(rv$current.obj)$P.Value, 
                        0,
                        rv$current.obj@experimentData@other$threshold.logFC,
                        cond)
    }else{
        #p <- NULL
        p <- RunDiffAna()
        cond <- c(input$condition1, input$condition2)
        diffAnaVolcanoplot(p$logFC, 
                            p$P.Value, 
                            rv$seuilPVal, 
                            rv$seuilLogFC,
                            cond)
    }
    })
})


output$volcanoplotStep3 <- renderPlot({
    rv$seuilPVal
    rv$seuilLogFC
    input$condition1
    input$condition2
    input$diffAnaMethod
    
    if (is.null(input$condition1) ||is.null(input$condition2))
    {return(NULL)}
    if (input$condition1 == input$condition2) {return(NULL)}
    if (is.null(rv$seuilPVal) || is.na(rv$seuilPVal)) { return (NULL)}
    
    
    isolate({
    
    #Si on a deja des pVal, alors, ne pas recalculer 
    if ("logFC" %in% names(fData(rv$current.obj) )){
        diffAnaVolcanoplot(fData(rv$current.obj)$logFC,
                    fData(rv$current.obj)$P.Value, 
                    rv$current.obj@experimentData@other$threshold.p.value,
                    rv$current.obj@experimentData@other$threshold.logFC,
                    c(rv$current.obj@experimentData@other$condition1,
                    rv$current.obj@experimentData@other$condition2)
        )
    }else{
        #p <- NULL
        p <- RunDiffAna()
        cond <- c(input$condition1, input$condition2)
        
        diffAnaVolcanoplot(p$logFC, 
                            p$P.Value, 
                            rv$seuilPVal, 
                            rv$seuilLogFC,
                            cond)
    }
    })
})


#-------------------------------------------------------------------
output$aboutText <- renderUI({
    busyIndicator("Calculation In progress",wait = 0)
    
    t <- sessionInfo()
    daparVersion <- t$otherPkgs$DAPAR$Version
    ProstarVersion <- installed.packages()["Prostar","Version"]
    
    
    text <- paste("<strong>DAPAR</strong> and <strong>ProStaR</strong> form a 
                software suite for quantitative analysis of mass spectrometry 
                based proteomics, more specifically designed to process 
                relative quantitative data from discovery experiments.<br> <br>
                
                
It is composed of two distinct R packages : <br>", 
"<ul style=\"list-style-type:disc;\">
<li>
<a href=\"http://www.bioconductor.org/packages/release/bioc/html/Prostar.html\"
title=\"here\" target=\"_blank\">Prostar</a> package (version ",
ProstarVersion, "): the web based graphical user interface to DAPAR 
</li>
<li>
<a href=\"http://www.bioconductor.org/packages/release/bioc/html/DAPAR.html\"
title=\"here\" target=\"_blank\">DAPAR</a> (version ",daparVersion,"): a 
collection of tools and graphs dedicated to proteomic analysis
</li>
</ul> 
            
In addition, it is bind numerous other R packages available on 
<a href=\"the https://cran.r-project.org/\" title=\"here\" target=\"_blank\">
CRAN</a> or on the <a href=\"http://www.bioconductor.org\"
title=\"here\" target=\"_blank\">Bioconductor</a>, among which 
<a href=\"http://www.bioconductor.org/packages/release/bioc/html/MSnbase.html\"
title=\"here\" target=\"_blank\">MSnbase</a>, which has introduced Msnsets, 
the data structure on which all the processing are based.
<br>
Here is a brief overview of the available functionalities:
<ul style=\"list-style-type:disc;\">
<li>  
Descriptive statistics are available, for exploration and visualization of the 
quantitative dataset;
</li>
<li>  
Filtering options allows pruning the protein or peptide list according to 
various criteria (missing values, contaminants, reverse sequences);
</li>

<li>
Cross replicate normalization, so as to make the quantitative values 
comparable between the different analyzed samples;
</li>

<li>  
Missing values imputation with different methods, depending on the nature of 
the missing values;
</li>
                
<li>
Differential analysis, which includes null hypothesis significance testing 
as well as multiple testing correction (for false discovery rate estimation).
</li>
</ul>
                
<br>
<br>
For more details, please refer to the \"Help\" tab.", sep="")
    
    HTML(text)

})

########################################################
output$limmaplot <- DT::renderDataTable({
    rv$current.obj
    input$diffAnaMethod
    input$seuilLogFC
    input$seuilPVal
    
    if ( is.null(rv$current.obj) ||
        is.null(input$seuilLogFC)    ||
        is.null(input$seuilPVal)
    ) {return(NULL)}
    
    if (is.null(input$diffAnaMethod) || (input$diffAnaMethod == "None")) 
    {return(NULL)}
    
    # isolate({
    t <- NULL
    # Si on a deja des pVal, alors, ne pas recalculer avec ComputeWithLimma
    if (isContainedIn(c("logFC","P.Value"),names(fData(rv$current.obj)) ) ){
    selectedItems <- (which(fData(rv$current.obj)$Significant == TRUE)) 
    t <- data.frame(id =  
                        rownames(exprs(rv$current.obj))[selectedItems],
                    fData(rv$current.obj)[selectedItems,
                    c("logFC", "P.Value", "Significant")])
        } else{
    data <- RunDiffAna()
    upItems1 <- which(-log10(data$P.Value) >= rv$seuilPVal)
    upItems2 <- which(abs(data$logFC) >= rv$seuilLogFC)
    selectedItems <- intersect(upItems1, upItems2)
    t <- data.frame(id =  rownames(exprs(rv$current.obj))[selectedItems],
                    data[selectedItems,])
    }
    t
    
})

isContainedIn <- function(strA, strB){
    return (all(strA %in% strB))
}

# ---- Download of only significat data --------------
output$linkWelch <- renderUI({
    input$ExportWelchTest
    if (input$ExportWelchTest == 0) {return(NULL) }
    
    saveMSnset(input$filenameWelchData,
                gFileExtension$msnset,
                rv$current.obj[
                which(fData(rv$current.obj)$Significant.Welch == TRUE)])
    filename <- paste(input$filenameWelchData, gFileExtension$msnset, sep="")
    
    completeFilename <- paste(rv$dirnameforlink,filename, sep="/")
    a(filename, href=completeFilename)
    
})

# ---- Download of only significat data --------------
output$linkLimma <- renderUI({
    input$ExportdiffAnaLimma
    if (input$ExportdiffAnaLimma == 0) {return(NULL) }
    
    saveMSnset(input$filenameLimmaData, gFileExtension$msnset, 
                rv$current.obj[
                which(fData(rv$current.obj)$Significant.limma == TRUE)])
    filename <- paste(input$filenameLimmaData, gFileExtension$msnset, sep="")
    completeFilename <- paste(rv$dirnameforlink,filename, sep="/")
    a(filename, href=completeFilename)
    
})

# store the object in binary file
saveMSnset <- function(name, fileExt, obj ){
    #print(rv$dirname)
    saveRDS(obj,file=paste(rv$dirname,"/", name, fileExt,sep=""))
    return(obj)
}

############ Read text file ######################
observe({
    input$file1
    input$XLSsheets
    if (is.null(input$file1) ) {return(NULL)  }
    if (((GetExtension(input$file1$name)== "xls") 
        || (GetExtension(input$file1$name) == "xlsx") ) 
        && is.null(input$XLSsheets)) {return(NULL)  }
    
    # print(input$file1$datapath)
    ClearMemory()
    ext <- GetExtension(input$file1$name)
    if ((ext == "txt") || (ext == "csv") ){
    rv$tab1 <- read.csv(input$file1$datapath, 
                        header=TRUE, 
                        sep="\t", 
                        as.is=T)
    } else if ((ext == "xls") || (ext == "xlsx") ){
    file <- loadWorkbook(input$file1$datapath)
    rv$tab1 <- readWorksheet(file, sheet = input$XLSsheets)
    
    }
})



#########################################################
UpdateFilterWidgets <- function(){
    
    isolate({
    rv$current.obj
    if (length(rv$current.obj@processingData@processing) > 0){
        
        val <- match (gReplaceAllZeros ,
                    rv$current.obj@processingData@processing)
        updateCheckboxInput(session, "replaceAllZeros",value=val)
        
        val <- match (gLogTransform, rv$current.obj@processingData@processing)
        #updateCheckboxInput(session,"log2transform",value=val)
        
        r <- grep(pattern = gFilterTextPrefix, 
                rv$current.obj@processingData@processing, 
                fixed=TRUE, value=FALSE)
        if ( length(r) > 0)
        { 
        listMots <- unlist(strsplit(
            rv$current.obj@processingData@processing[r], split=" "))
        updateSliderInput(session,inputId = "seuilNA", value = listMots[6])
        updateRadioButtons(session,inputId = "ChooseFilters", 
                            selected = listMots[3])
        }
        else
        { 
        updateRadioButtons(session,
                            inputId = "ChooseFilters", 
                            selected = gFilterNone)
        }
    }
    else{
        updateCheckboxInput(session, "replaceAllZeros",value=F)
        updateRadioButtons(session,
                            inputId = "ChooseFilters", 
                            selected = gFilterNone)
    }
    updateSelectInput(session,"typeImputation",selected= c("none")) 
    updateSelectInput(session, "normalization.family",selected = c("None"))
    })
}

#########################################################
##' Function to compute the maximum value for the filter
##' @author Samuel Wieczorek
GetMaxValueThresholdFilter <- function(){
    vMax <- 0
    isolate({
    input$ChooseFilters
    if (input$ChooseFilters == gFilterWholeMat) { 
        vMax <- ncol(exprs(rv$current.obj))}
    else if (input$ChooseFilters == gFilterAllCond 
                || input$ChooseFilters == gFilterOneCond){ 
        ll <- NULL
        for (i in 1:length(unique(pData(rv$current.obj)$Label))){
        ll <- c(ll, length(which(
            pData(rv$current.obj)$Label==
            unique(pData(rv$current.obj)$Label)[i])))
        }
        
        vMax <- min(ll)
    }
    })
    return(vMax)
}


## Perform missing values filtering
observe({
    if (is.null(input$perform.filtering.MV) ){return(NULL)}
    if (input$perform.filtering.MV == 0){return(NULL)}
    
    isolate({
    if (input$ChooseFilters == gFilterNone){
        rv$current.obj <- rv$dataset[[input$datasets]]
    } else {
        
        
        keepThat <- mvFilterGetIndices(rv$dataset[[input$datasets]],
                                        input$ChooseFilters,
                                        input$seuilNA)

        if (!is.null(keepThat))
            {
            rv$deleted.mvLines <- rv$dataset[[input$datasets]][-keepThat]
            rv$current.obj <- mvFilterFromIndices(rv$dataset[[input$datasets]],
                        keepThat,
                        GetFilterText(input$ChooseFilters, input$seuilNA) )

            
            #write command log
            # l <- paste(keepThat,",", collapse="")
            # writeToCommandLogFile(
            #     paste("keepThat <- ",
            #         findSequences(keepThat),
            #     sep="")
            # )
            
            
            writeToCommandLogFile(
                paste("keepThat <- mvFilterGetIndices(rv$dataset[['",
                      input$datasets, 
                      "']], '",
                      input$ChooseFilters, "', '",
                      input$seuilNA, "')", sep="")
            )
            writeToCommandLogFile("deleted.mv <- current.obj[-keepThat]")
            writeToCommandLogFile(paste("txt <- '",
                                        GetFilterText(input$ChooseFilters,
                                                      input$seuilNA),
                                        "'",
                                        sep ="")
            )
            writeToCommandLogFile(paste("current.obj <- mvFilterFromIndices(",
                                    "current.obj, keepThat, '",
                                     GetFilterText(input$ChooseFilters,
                                                   input$seuilNA),
                                     "')",
                                     sep ="")
                )
        }


        updateSelectInput(session, "ChooseFilters", 
                        selected = input$ChooseFilters)
        updateSelectInput(session, "seuilNA", 
                        selected = input$seuilNA)

    }
    })
})


observe({
    if (is.null(input$perform.filtering.Contaminants) ){return(NULL)}
    if (input$perform.filtering.Contaminants == 0){return(NULL)}
    
    isolate({
        temp <- rv$current.obj
        if (!is.null(input$idBoxContaminants)
            || (input$idBoxContaminants != "")) {
            ind <- getIndicesOfLinesToRemove(temp,
                                             input$idBoxContaminants, 
                                             input$prefixContaminants)
            if (!is.null(ind))  {
                rv$deleted.contaminants <- temp[ind]
                
                #temp <- temp[-ind]
                temp <- deleteLinesFromIndices(temp, ind, 
                                               paste("'", length(ind), 
                                                     "contaminants were removed from dataset.'",
                                                     sep=" ")
                )
                
                #write command log
                writeToCommandLogFile(
                    paste(
                        "indContaminants <- getIndicesOfLinesToRemove(",
                        "current.obj",
                        ",'", input$idBoxContaminants,
                        "', '",input$prefixContaminants,"')",
                        sep="")
                )
                
                writeToCommandLogFile(
                    "deleted.contaminants <- current.obj[indContaminants]")
                writeToCommandLogFile(
                    paste("txt <- '",
                          length(ind),
                          "contaminants were removed from dataset.'",
                          sep=" "))
                writeToCommandLogFile(
                    paste("current.obj <- ",
                          "deleteLinesFromIndices(current.obj, indContaminants, txt)",
                          sep="")
                )
                
            }
        }
        
        if (!is.null(input$idBoxReverse) || (input$idBoxReverse != "")){
            ind <- getIndicesOfLinesToRemove(temp,
                                             input$idBoxReverse,
                                             input$prefixReverse)
            
            if (!is.null(ind))  {
                rv$deleted.reverse <- temp[ind]
                temp <- deleteLinesFromIndices(temp, ind, 
                                               paste(length(ind), "reverse were removed from dataset",
                                                     sep=" ")
                )
                
                writeToCommandLogFile(
                    paste(
                        "indReverse <- getIndicesOfLinesToRemove(",
                        "current.obj",
                        ",'", input$idBoxReverse,
                        "', '",input$prefixReverse,"')",
                        sep="")
                )
                
                writeToCommandLogFile(
                    "deleted.reverse <- current.obj[indReverse]")
                writeToCommandLogFile(
                    paste("txt <- '",
                          length(ind),
                          "reverse were removed from dataset.'",
                          sep=" "))
                writeToCommandLogFile(
                    paste("current.obj <- ",
                          "deleteLinesFromIndices(current.obj, indReverse, txt)",
                          sep="")
                )
            }
        }
        
        rv$current.obj <- temp
        
        updateSelectInput(session, "idBoxReverse",
                          selected = input$idBoxReverse)
        updateSelectInput(session, "idBoxContaminants",
                          selected = input$idBoxContaminants)
        updateSelectInput(session, "prefixContaminants", 
                          selected = input$prefixContaminants)
        updateSelectInput(session, "prefixReverse",
                          selected = input$prefixReverse)
        
        updateTabsetPanel(session, "tabFilter", selected = "FilterContaminants")
        
    })
})







#########################################################
##' Validation of the filters and modification on current object
##' @author Samuel Wieczorek
    observe({ 
    input$ValidateFilters
    if(is.null(input$ChooseFilters) || (input$ValidateFilters == 0)) 
    {return(NULL)}
    
    isolate({
        if((input$ChooseFilters != gFilterNone) 
            || !is.null(input$idBoxContaminants) 
            || !is.null(input$idBoxReverse)){
        
        rv$typeOfDataset <- rv$current.obj@experimentData@other$typeOfData
        name <- paste ("Filtered", " - ", rv$typeOfDataset, sep="")
        rv$dataset[[name]] <- rv$current.obj
        
    ###### write to commandLog File
         writeToCommandLogFile(  
             paste("dataset[['",name, "']] <- current.obj", sep=""))
    ###### end write to command log file
        
        
        updateSelectInput(session, "datasets", 
                            choices = names(rv$dataset), selected = name)
        txtFilterMV <- paste("Filtering :",
                                GetFilterText(input$ChooseFilters, 
                                            input$seuilNA), 
                            sep="")
        txt <- paste(txtFilterMV, "Contaminants deleted", 
                    "Reverse deleted", 
                    sep=" ")
        UpdateLog(txt,name)
        }
        
    })

})



output$chooseProteinId <- renderUI({
    rv$current.obj
    if (is.null(rv$current.obj)) {return (NULL)}
    
    selectInput("proteinId", 
                "Choose the protein ID",
                choices = c("None",colnames(fData(rv$current.obj))))
})

observe({
    input$fData.box
    choices = colnames(rv$tab1)[-which(colnames(rv$tab1) %in% input$fData.box)]
    names(choices) = 
    colnames(rv$tab1)[-which(colnames(rv$tab1) %in% input$fData.box)]
    updateSelectInput(session, "eData.box", 
                    label = "",
                    choices = choices,
                    selected = choices)
    
})


#-----------------------------------------------
output$ObserverAggregationDone <- renderUI({
    rv$temp.aggregate
    input$perform.aggregation
    
    isolate({
        if (input$perform.aggregation == 0) 
    {return(NULL)  }
    else if (input$aggregationMethod != "none"){
    h3(paste("Aggregation done with the ", input$aggregationMethod, " method.", sep=""))
    }
        
    })
})


#-----------------------------------------------
output$aggregationPlot <- renderPlot({
    input$proteinId
    rv$current.obj
    if (is.null( input$proteinId) || (input$proteinId == "None"))
    {return(NULL)}
    if (is.null( rv$current.obj)){return(NULL)}
    matAdj <- ComputeAdjacencyMatrix()
    if (input$checkSharedPeptides) {GraphPepProt(matAdj$matWithSharedPeptides)}
    else {GraphPepProt(matAdj$matWithUniquePeptides)}
    
})


output$headerpanel <- renderUI({
    tree <- input$tree
    rv$current.obj
    
    if (!is.null(tree)){
    selected.leaf <- unlist(get_selected(tree), use.names = FALSE)
    if (length(selected.leaf) >0) {
        l <- list(paste("ProStaR", selected.leaf, sep = " - "))
    } else { l <- list("ProStaR")}
        }
    else { l <- list("ProStaR")}
    
    headerPanel(l,windowTitle="ProStaR - DEV"  )
})

output$aggregationStats <- renderUI ({
    input$proteinId
    rv$current.obj
    if (is.null( input$proteinId) || (input$proteinId == "None"))
    {return(NULL)}
    if (is.null( rv$current.obj)){return(NULL)}
    matAdj <- ComputeAdjacencyMatrix()
    
    text <- paste("<ul style=\"list-style-type:disc;\">
                <li>
                Number of peptides: ", 
                nrow(matAdj$matWithSharedPeptides),
                "</li>

                <li>
                Number of unique peptides: ", 
                nrow(matAdj$matWithUniquePeptides),
                "</li>


                <li>
                Number of shared peptides: ",
                nrow(matAdj$matWithSharedPeptides)
                -nrow(matAdj$matWithUniquePeptides),
                "</li>

                <li>
                Number of proteins:  ", ncol(matAdj$matWithSharedPeptides),
                " </li>

                <li>
                Number of proteins only defined by unique peptides: ", 
                ncol(matAdj$matWithUniquePeptides), 
                "</li>

                <li>
                Number of proteins only defined by shared peptides:  ", 
                ncol(matAdj$matWithSharedPeptides)
                - ncol(matAdj$matWithUniquePeptides), 
                "</li>

                </ul>" , sep="")
    
    
    
    
    HTML(text)
    })

output$aggregationPlotShared <- renderPlot({
    input$proteinId
    rv$current.obj
    if (is.null( input$proteinId) || (input$proteinId == "None"))
    {return(NULL)}
    if (is.null( rv$current.obj)){return(NULL)}
    matAdj <- ComputeAdjacencyMatrix()
    GraphPepProt(matAdj$matWithSharedPeptides)
    
})

output$aggregationPlotUnique <- renderPlot({
    input$proteinId
    rv$current.obj
    if (is.null( input$proteinId) || (input$proteinId == "None"))
    {return(NULL)}
    if (is.null( rv$current.obj)){return(NULL)}
    matAdj <- ComputeAdjacencyMatrix()
    GraphPepProt(matAdj$matWithUniquePeptides)
    
})




###------------ Perform aggregation--------------------
observe({
    #input$perform.aggregation
    #input$aggregationMethod
    if (is.null(input$perform.aggregation) 
        || (input$perform.aggregation == 0))
    {return(NULL)}
    
    isolate({
        if (input$aggregationMethod != "none")
            {
            rv$temp.aggregate <- RunAggregation()
            writeToCommandLogFile("temp.aggregate <- data")
        }
    })
})


output$choosePrefixContaminants <- renderUI({
    rv$current.obj
    input$idBoxContaminants
    if (is.null(rv$current.obj)) {return(NULL)  }
    if (is.null(input$idBoxContaminants)) {return(NULL)  }
    
    textInput("prefixContaminants", label = "Choose prefix",value = "")
})


output$choosePrefixReverse <- renderUI({
    rv$current.obj
    input$idBoxReverse
    if (is.null(rv$current.obj)) {return(NULL)  }
    if (is.null(input$idBoxReverse)) {return(NULL)  }
    
    textInput("prefixReverse", label = "Choose prefix", value = "" )
    
})



output$id_Contaminants <- renderUI({
    rv$current.obj
    if (is.null(rv$current.obj)) {return(NULL)  }
    
    .choices <- c("",colnames(fData(rv$current.obj)))
    names(.choices) <- c("",colnames(fData(rv$current.obj)))
    selectInput("idBoxContaminants", 
                label = "Choose column", 
                choices = .choices , 
                selected = NULL)
})


output$id_Reverse <- renderUI({
    rv$current.obj
    if (is.null(rv$current.obj)) {return(NULL)  }
    
    .choices <- c("",colnames(fData(rv$current.obj)))
    names(.choices) <- c("",colnames(fData(rv$current.obj)))
    selectInput("idBoxReverse", 
                label = "Choose column", 
                choices = .choices , 
                selected = NULL)
})




output$conversionDone <- renderUI({
    rv$current.obj
    if (is.null(rv$current.obj)) { return(NULL)}
    
    h4("The conversion is done. Your dataset has been automatically loaded 
        in memory. Now, you can switch to the Descriptive statistics panel to 
        vizualize your data.")
    
})


output$ManageXlsFiles <- renderUI({
    input$file1
    if (is.null(input$file1)){return(NULL)}
    
    .ext <- GetExtension(input$file1$name)
    if ((.ext == "xls") || (.ext == "xlsx")){ 
    file <- loadWorkbook(input$file1$datapath)
    sheets <- getSheets(file)
    selectInput("XLSsheets", "sheets", choices = sheets)
    }
    
})




output$DS_PlotHeatmap <- renderUI({
    rv$current.obj

    if (is.null(rv$current.obj)) {return(plot.new())}
    if (getNumberOfEmptyLines(exprs(rv$current.obj)) != 0) {return (NULL)}
    
    conditionalPanel(
        condition = TRUE,
        busyIndicator("Calculation In progress",wait = 0),
        plotOutput("heatmap", width = "900px", height = "600px")
    )
})


output$helpForNormalizationMethods <- renderUI({
    input$normalization.method
    rv$typeOfDataset
    if (is.null(input$normalization.method) || (input$normalization.method == "None")) {return(NULL)}
    toto <- input$normalization.method
    
    
    switch(input$normalization.method,
            "Global Rescaling - sum by columns" = {
t <- paste("Each abundance value is divided by the total of the abundance 
values in the same replicate. This normalization  <br> 
is interesting to compare the proportions of a given", rv$typeOfDataset, "in 
different replicates that do not necessarily contain  <br> the same amount of 
biological material. Contrarily to the others, this normalization is not
performed on the log2 scale  <br> (for it would not have any interpretation), 
but on the real intensity scale: the data are thus exponentiated first, 
normalize, <br> and finally re-log2-transformed.", sep=" ")},

            "Global Rescaling - quantiles" = {
t <- paste("The log-abundances are normalized by the quantile method (from R 
package preprocessCore). Roughly, the abundance  <br> 
values are replaced by their order statics. This normalization is the 
strongest one available, and it should be use <br> 
carefully, for it leads to a strong loss of information, making all the 
replicates rather similar.", sep=" ")},

            "Median Centering - overall" = {
t <- "The medians of the replicates are aligned. To do so, the median of each 
replicate is computed and subtracted to each <br>
abundance value. Then, the means of all the medians (over all the conditions) 
is added, so as to roughly find back <br> 
the original range of values. As a result, any global shift of the abundance 
range between the conditions is suppressed. <br>
Note that all these computations are performed on the log scale"},
            
            "Median Centering - within conditions" = {
t <- "The medians of the replicates are aligned. To do so, the median of each 
replicate is computed and subtracted to each abundance <br>
value. Then, the means of all the medians (within each condition) is added, so 
as to roughly find back the original range of values. <br>
As a result, global shift of the abundance range between the conditions 
remains un-normalized. Note that all these computations <br>
are performed on the log scale."},
            
            "Mean Centering - overall" = {
t <- "The means of the replicates are aligned. To do so, the mean of each 
replicate is computed and subtracted to each abundance value. <br>
Then, the means of all the means (over all the conditions) is added, so as to 
roughly find back the original range of values. <br>
As a result, any global shift of the abundance range between the conditions 
is suppressed. Note that all these computations <br>
are performed on the log scale."},
            
            "Mean Centering - within conditions" = {
t <- "The means of the replicates are aligned. To do so, the means of each 
replicate is computed and subtracted to each abundance value. <br>
Then, the means of all the means (within each condition) is added, so as to 
roughly find back the original range of values. As a result, <br>
global shift of the abundance range between the conditions remains 
un-normalized. Note that all these computations are performed on the log 
scale."},
            
            
            "Mean Centering Scaling - overall" = {
t <- "Same as \"Mean Centering – overall\", however, in addition, the variance 
of the distribution of each replicate is re-scaled to 1. <br>
This normalization only applies to dataset where log-abundance values are 
normally distributed along each replicate."},
            
            "Mean Centering Scaling - within conditions" = {
t <- "Same as \"Mean Centering – within conditions\", however, in addition, 
the variance of the distribution of each replicate is re-scaled <br>
to 1. This normalization only applies to dataset where log-abundance values 
are normally distributed along each replicate."},

stop("Enter something that switches me!")
)
    
    HTML(t)
})




ProcessStepsTabPanel <- function(){
    
    tabPanel(title = "processStepsTab",
            value = "processStepsTab",
            uiOutput("processSteps")
    )
    
}



#-------------------------------------------------------------
LogTabPanel <- reactive({
    rv$text.log
    tabPanel(title="logTabPanel",
            value = "tabLogSession",
            h3(paste("R session",Sys.getpid(),sep=" ")),
            DT::dataTableOutput("log")
    )
})


ConditionTabPanel <- reactive({
    rv$conditions
    rv$current$obj
    if (is.null(rv$current.obj)){return(NULL)}
    
    tabPanel(title="ConditionsSetup",
            value = "tabConditionsSetup",
            h3("Select conditions to perform the differential analysis"),
            helpText("Please choose the labels for condition to analyse"),
            if (GetNbNA() > 0){
                h3("There are some NA in your data. please impute before.")
            }
            else{
                h3("Conditions setup")
                helpText("Please choose the labels for condition to analyse")
            }
    )
})


# 
#------------------------------------------
##' Missing values imputation - reactivity behavior
##' @author Samuel Wieczorek
observe({
    if (is.null(input$perform.imputation.button) ){return(NULL)}
    if (input$perform.imputation.button == 0){return(NULL)}
    
    isolate({
    
    
    result = tryCatch(
        {
        
        
        input$missing.value.algorithm
    rv$current.obj
    input$datasets
    .temp <- unlist(strsplit(input$missing.value.algorithm, " - "))
    if (.temp[1] == "None"){
        rv$current.obj <- rv$dataset[[input$datasets]]
    } else {
        if ((.temp[1] == "LeftCensored") || (.temp[1] == "RandomOccurence")) 
        {
        
            busyIndicator("Calculation In progress",wait = 0)
            rv$current.obj <- wrapper.mvImputation(
                                rv$dataset[[input$datasets]],
                                .temp[2])
            
            #write log command file
            writeToCommandLogFile(
                paste("current.obj <- wrapper.mvImputation(",
                      "rv$dataset[['",
                      input$datasets, 
                      "']],'",.temp[2],"')",
                      sep="")
            )
            
            updateSelectInput(session, 
                            "missing.value.algorithm", 
                            selected = input$missing.value.algorithm)
            
        }
        else if (input$missing.value.type == "Mix")
        {}
    }
        }
        , warning = function(w) {
            print(w)
        }, error = function(e) {
            shinyjs::info(e)
        }, finally = {
            #cleanup-code
        
                }

    )
    })
})


##' Reactive behavior : Normalization of data
##' @author Samuel Wieczorek
observe({
    # input$perform.normalization
    # input$normalization.method
    if (is.null(input$perform.normalization) ){return(NULL)}
    if (input$perform.normalization == 0){return(NULL)}
    
    isolate({
    .temp <- unlist(strsplit(input$normalization.method, " - "))
    
    if (.temp[1] == "None"){
        rv$current.obj <- rv$dataset[[input$datasets]]
    } else {
        rv$current.obj <- wrapper.normalizeD(rv$dataset[[input$datasets]], 
                                    .temp[1], 
                                    .temp[2])
        updateSelectInput(session, "normalization.method", 
                        selected = input$normalization.method)
        
        
        ## Write command log file
        writeToCommandLogFile(
            paste("current.obj <- wrapper.normalizeD(",
                  "rv$dataset[['",
                input$datasets, 
                "']],'",.temp[1], "','", .temp[2],"')",
                  sep="")
        )
    }
    })
})



})
