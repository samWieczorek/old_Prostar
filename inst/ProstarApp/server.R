options(shiny.maxRequestSize=30*1024^2) 
options(shiny.trace=TRUE)

library(shiny)
library(rhandsontable)
library(data.table)
library(reshape2)
library(quantmod)


# initialize data with colnames
df <- data.frame(matrix(c("0","0"), 1, 2))
colnames(df) <- c("Input1", "Input2")

port <- data.table(Experiment=list(),
                   Label=list(),
                   Bio.Rep=list(),
                   Tech.Rep=list(),
                   Analyt.Rep=list())

shinyServer(function(input, output, session) {
  
  output$hot <- renderRHandsontable({
    #input$eData.box
    
    #if (is.null(input$hot)) {
    if (is.null(input$eData.box)) {
      DT <- rv$hot
    } else {
      DT <- data.table(Experiment = as.character(input$eData.box),
                       Label = rep(" ",length(input$eData.box)),
                       Bio.Rep = rep(" ",length(input$eData.box)),
                       Tech.Rep = rep(" ",length(input$eData.box)),
                       Analyt.Rep = rep(" ",length(input$eData.box)))
      
      rownames(DT) <- input$eData.box
      rv$hot <- DT
      
    }
    
    if (!is.null(DT))
      rhandsontable(DT,  height = 600,stretchH = "all") %>% 
      hot_cols(colWidths = 100) %>%
      hot_rows(rowHeights = 30) %>%
      hot_col(col = "Experiment", readOnly = TRUE)
  })
  
  rv <- reactiveValues(
    # variable to handle the current object that will be showed
    current.obj = NULL,
    current.obj.name = NULL,
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
    errMsgcalibrationPlotALL = NULL)
  
  env <- environment()
  
  
  #Tree in the sidebar panel
  output$tree <- renderTree({
    list(
      "Dataset manager" = list(
        "Open MSnset File" = "Open MSnset File",
        "Convert Data To MSnset" = "Convert Data To MSnset",
        "Export MSnset" = "Export",
        "Session Log" = "Session Log"),
      "Descriptive Statistics" = "Descriptive Statistics",
      "Data processing" = list(
        Filtering = "Filtering",
        Normalization = "Normalization", 
        Imputation = "Imputation",
        Aggregation = "Aggregation",
        "Differential Analysis" = "Differential Analysis"),
      Help = "Help"
    )
  })
  
  # Show panels with leaves selected 
  output$test <- renderUI({
    tree <- input$tree
    rv$current.obj
    
    if (is.null(tree)){return(NULL)
    } else{
      selected.leaf <- unlist(get_selected(tree), use.names = FALSE)
      if (is.null(selected.leaf))
      {return (NULL)}
      if (selected.leaf == "Open MSnset File") { openFile_tabPanel()}
      else if (selected.leaf == "Convert Data To MSnset") {import_tabPanel()}
      else if (selected.leaf == "Filtering") { 
                       # filter_tabPanel()
        filter_Complete_tabPanel()
        }
      else if (selected.leaf == "Export MSnset") {export_tabPanel()}
      
      else if (selected.leaf == "Descriptive Statistics") 
      { ViewGraphicsTabPanel()}
      
      else if (selected.leaf == "Data processing") { ProcessStepsTabPanel()}
      else if (selected.leaf == "Normalization") { NormalizationTabPanel()}
      else if (selected.leaf == "Imputation") {ImputationTabPanel()}
      else if (selected.leaf == "Aggregation") {AggregationTabPanel()}
      
      else if (selected.leaf == "Session Log") { LogTabPanel()}
      else if (selected.leaf == "Help") {help_tabPanel()}
      
      else if (selected.leaf == "Differential Analysis") {diffAnaTabPanelComplete()}
       
    }
    
    
    
    
  })
  
  # shinyFileChoose(input, 'files', root='/', filetypes=c('', '.txt'))
  ComputeMVTags <- reactive({
    tags <- TaggingMissingValues(rv$current.obj, 
                                 input$type.of.missvalues, 
                                 input$seuilMNAR)
    return(tags)
  })
  
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
    return(list(matWithSharedPeptides=matSharedPeptides,
                matWithUniquePeptides=matUniquePeptides))
  })
  
  RunAggregation <- reactive({
    mat <- ComputeAdjacencyMatrix()
    n <- NULL
    if (input$aggregationMethod == gAgregateMethod$topn) { n <- input$nTopn}
    
   
    tryCatch (
      {
        if (input$checkSharedPeptides){
          data <- pepAgregate(rv$current.obj, 
                              input$proteinId,
                              input$aggregationMethod, 
                              mat$matWithSharedPeptides, 
                              n)
          
        }else{
          data <- pepAgregate(rv$current.obj, 
                              input$proteinId,
                              input$aggregationMethod, 
                              mat$matWithUniquePeptides
                              , n)
        }
      },
      err=function(errorCondition) {
        cat("in err handler")
        message(errorCondition)
      })
      
      
    
    return(data)
  })
  
  RunDiffAna <- reactive({
    input$diffAnaMethod
    rv$current.obj
    input$condition1
    input$condition2
    
    data <- NULL
    if (input$diffAnaMethod == "Limma"){
      data <- wrapper.diffAnaLimma(rv$current.obj, input$condition1, input$condition2)
    } else if (input$diffAnaMethod == "Welch"){
      data <- wrapper.diffAnaWelch(rv$current.obj, input$condition1, input$condition2)
    }

    return(data)
  })
  
  
  # Update the global variable og log
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
    
    rv$dataset[["Original"]] <- rv$current.obj
    UpdateFilterWidgets()
    updateSelectInput(session, "datasets", 
                      choices = names(rv$dataset),
                      selected = "Original")
    
    #log update
    UpdateLog(paste("Open : file ",input$file$name, " opened"),"Original")
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
      ClearMemory()
      rv$current.obj <- readRDS(input$file$datapath)
      rv$current.obj.name <- DeleteFileExtension(input$file$name)
      loadObjectInMemoryFromConverter()
    })
  })
  
  ##' -- Validate the normalization ---------------------------------------
  ##' @author Samuel Wieczorek
  observe({ 
    input$valid.normalization
    input$normalization.method
    if (is.null(input$valid.normalization) || 
        (input$valid.normalization == 0)) 
    {return(NULL)}
    
    isolate({
      if (input$normalization.method != "None") {
        rv$dataset[["Normalized"]] <- rv$current.obj
        updateSelectInput(session, "datasets", 
                          choices = names(rv$dataset),
                          selected = "Normalized")
        UpdateLog(paste("Normalization : data normalized with the method",
                        input$normalization.method, sep=" "), "Normalized")
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
      }else{ m <-mat$matWithUniquePeptides}
        
      for(c in input$columnsForProteinDataset.box){
         newCol <- BuildColumnToProteinDataset(fData(rv$current.obj), m, c)
        cnames <- colnames(fData(rv$temp.aggregate))
        fData(rv$temp.aggregate) <- data.frame(fData(rv$temp.aggregate), newCol)
        colnames(fData(rv$temp.aggregate)) <- c(cnames, c)
        
      }
      rv$current.obj <- rv$temp.aggregate
      rv$dataset[["Aggregated"]] <- rv$current.obj
      
      
      updateSelectInput(session, "datasets", 
                        choices = names(rv$dataset),
                        selected = "Aggregated")
      UpdateLog(
        paste("Aggregation : peptides were aggregated into proteins with method =",
              input$aggregationMethod,
              ", include Shared Peptides = ", input$checkSharedPeptides,
              ", protein id = ", input$proteinId, sep=" "),
        "Aggregated")
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
      rv$dataset[["Imputed"]] <- rv$current.obj
      updateSelectInput(session, "datasets", 
                        choices = names(rv$dataset),
                        selected = "Imputed")
      UpdateLog(paste("Imputation with" ,
                      input$missing.value.algorithm,sep=" "),
                "Imputed")
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
        m <- NULL
        if (input$calibrationMethod == "Benjamini-Hochberg") { m <- 1}
        else if (input$calibrationMethod == "numeric value") { m <- input$numericValCalibration} 
        else {m <- input$calibrationMethod }
        
        fdr <- diffAnaComputeFDR(data, rv$seuilPVal, rv$seuilLogFC, m)
         HTML(paste("<h4>FDR = ", signif(100*fdr, digits=1)," % </h4>", sep=""))
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
    val <- tryCatch(withCallingHandlers(expr, warning = wHandler), error = eHandler)
    list(value = val, warnings = myWarnings, error=myError)
  } 
  
  
  
  
  output$calibrationPlot <- renderPlot({
    input$calibrationMethod
    input$numericValCalibration
    
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
      t <- t[which(abs(fData(rv$current.obj)$logFC) >= rv$current.obj@experimentData@other$threshold.logFC)]
      method <- NULL
    } else{
      data <- RunDiffAna()
      t <- data$P.Value
      t <- t[which(abs(data$logFC) >= rv$seuilLogFC)]
      method <- NULL
    }
    
    
   ll <- NULL
  
        if ((input$calibrationMethod == "numeric value") && !is.null(input$numericValCalibration)) {
          print("methode numeric value")
          ll <-catchToList(wrapperCalibrationPlot(t, input$numericValCalibration))
          rv$errMsgCalibrationPlot <- ll$warnings[grep( "Warning:", ll$warnings)]
        }
        else if (input$calibrationMethod == "Benjamini-Hochberg") {
          print("methode BH")
          ll <-catchToList(wrapperCalibrationPlot(t, 1))
          rv$errMsgCalibrationPlot <- ll$warnings[grep( "Warning:", ll$warnings)]
        }else { 
        print("methode xxx")
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
      t <- t[which(abs(fData(rv$current.obj)$logFC) >= rv$current.obj@experimentData@other$threshold.logFC)]
      
    } else{
      data <- RunDiffAna()
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
      rv$current.obj <- rv$dataset[[input$datasets]]
      UpdateLog(
        paste("Current dataset has changed. Now, it is ",
              input$datasets, 
              sep=" "),
        input$datasets)
    })
    
  })
  
  
  ##' show intensity values of the MSnset object in a table
  ##' @author Samuel Wieczorek
  output$viewExprs <- renderDataTable({
    rv$current.obj
    input$nDigits
    if (is.null(rv$current.obj)) {return(NULL)}
    if (input$nDigits == T){nDigits = 1e100}else {nDigits = 3}
    as.data.frame(cbind(ID = 
                          rownames(fData(rv$current.obj)),
                        round(exprs(rv$current.obj), 
                              digits=nDigits)))
  },
  option=list(pageLength=25,
              orderClasses = TRUE,
              autoWidth=FALSE,
              dom = 'R<"clear">lfrtip',
              columnDefs = list(
                list(
                  columns.width=c("60px"),
                  columnDefs.targets=c(list(0),list(1),list(2))))
  )
  )
  
  
  
  observe({ 
    input$ValidDiffAna
    # input$diffAnaMethod
    # input$condition1
    # input$condition2
    
    if ((input$ValidDiffAna == 0) ||  is.null(input$ValidDiffAna) ) {return(NULL)}
     if (input$condition1 == input$condition2) {return(NULL)}
    
      isolate({
    
    data <- RunDiffAna()
    
    
    m <- NULL
    if (input$calibrationMethod == "Benjamini-Hochberg") { m <- 1}
    else {m <- input$calibrationMethod }
    
    
    
    fdr <- diffAnaComputeFDR(data, rv$seuilPVal, rv$seuilLogFC, m)
    
    rv$current.obj <- diffAnaSave(rv$current.obj,
                                  data,
                                  input$diffAnaMethod,
                                  input$condition1,
                                  input$condition2,
                                  rv$seuilPVal, rv$seuilLogFC, fdr,
                                  input$calibrationMethod)
    txt <- paste("DiffAnalysis.", input$diffAnaMethod, sep="")
    
    rv$dataset[[txt]] <- rv$current.obj
    updateSelectInput(session, "datasets", 
                      choices = names(rv$dataset),
                      selected = txt)
    
    
    cMethod <- NULL
    if (input$calibrationMethod == "numeric value"){cMethod <- paste("The proportion of true null
hypotheses was set to", input$numericValCalibration, sep= " ")}
    else {cMethod <-input$calibrationMethod }
    
    text <- paste("Differential analysis with", input$diffAnaMethod, 
                  "Selection with the following threshold values :logFC =",
                  rv$seuilLogFC,
                   "The calibration was made with the method", cMethod,
                  ", -log10(p-value) = ",
                  rv$seuilPVal,
                  "corresponding to a FDR = ", round(100*fdr, digits=2),
                  sep=" ")
    UpdateLog(text,txt)
   
    updateTabsetPanel(session, "abc", selected = "ValidateAndSaveAnaDiff")
      }) 
    
  })
  

  output$DiffAnalysisSaved <- renderUI({
    input$datasets
    rv$current.obj
    if (is.null(input$datasets) || (length(grep("DiffAnalysis.",input$datasets)) !=1) ) {return(NULL)  }
    else if (grep("DiffAnalysis.",input$datasets) ==1 ) {
      h4("The differential analysis has been saved.")
    }
  })
  
  
  output$viewProcessingData <- renderDataTable({
    rv$current.obj
    if (is.null(rv$current.obj)) {return(NULL)}
    
    data.frame(History=(rv$current.obj)@processingData@processing
               [-grep("Subset", (rv$current.obj)@processingData@processing)])
    
    
  },
  option=list(pageLength=25,
              orderClasses = TRUE,
              autoWidth=FALSE,
              dom = 'R<"clear">lfrtip',
              columnDefs = list(list(columns.width=c("60px"),
                                     columnDefs.targets= c(list(0),list(1),list(2)))))
  )
  
  
  ##' show pData of the MSnset object
  ##' @author Samuel Wieczorek
  output$viewpData <- renderDataTable({
    rv$current.obj
    if (is.null(rv$current.obj)) {return(NULL)}
    as.data.frame(pData(rv$current.obj))
  },
  option=list(pageLength=25,
              orderClasses = TRUE,
              autoWidth=FALSE,
              columnDefs = list(list(columns.width=c("60px"),
                                     columnDefs.targets= c(list(0),list(1),list(2)))))
  )
  
  ##' show fData of the MSnset object in a table
  ##' @author Samuel Wieczorek
  output$viewfData <- renderDataTable({
    rv$current.obj
    if (is.null(rv$current.obj)) {return(NULL)}
    as.data.frame(fData(rv$current.obj))
  },
  option=list(pageLength=25,
              orderClasses = TRUE,
              autoWidth=FALSE,
              columns.searchable=F,
              columnDefs = list(list(columns.width=c("60px"),
                                     columnDefs.targets=c(list(0),list(1),list(2)))))
  )
  
  
  
  ##' Visualisation of missing values table
  ##' @author Samuel Wieczorek
  output$viewExprsMissValues <- renderDataTable({
    rv$current.obj
    if (is.null(rv$current.obj)) {return(NULL)}
    as.data.frame(cbind(ID = rownames(fData(rv$current.obj)),
                        exprs(rv$current.obj)))
  },
  
  option=list(orderClasses = TRUE,
              autoWidth=FALSE,
              columns.searchable=F,
              pageLength = 25,
              columnDefs = list(list(columns.width=c("60px"),
                                     columnDefs.targets=c(list(0),list(1),list(2)))))
  )
  
  
  output$RenderLimmaCond1 <- renderUI({
    rv$current.obj
    if (is.null(rv$current.obj) ) {return(NULL)  }
    
    #isolate({
    labels <- unique(pData(rv$current.obj)[,"Label"])
    labels <- setNames(as.list(labels),labels)
    #else{rv$conditions$cond1 <- input$condition1.sam}
    radioButtons("condition1", label = h3("Condition 1"), 
                 choices = labels, 
                 selected = labels[[1]], 
                 inline=F)
    #})
  })
  
  
  
  output$RenderLimmaCond2 <- renderUI({
    rv$current.obj
    if (is.null(rv$current.obj) ) {return(NULL)  }
    
    isolate({
      labels <- unique(pData(rv$current.obj)[,"Label"])
      labels <- setNames(as.list(labels),labels)
      radioButtons("condition2", label = h3("Condition 2"), 
                   choices = labels , 
                   selected = labels[[2]],
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
  
  
  
  output$toto <- renderDataTable({
    # mtcars
  },
  option=list(pageLength=6,
              orderClasses = TRUE,
              autoWidth=FALSE,
              lengthChange=F,
              searching=0,
              info=0,
              # bCaseInsensitive = TRUE,
              columnDefs = 
                list(list(columns.width=c("60px"),
                          columnDefs.targets=c(list(0),list(1),list(2)))))
  
  )
  
  
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
    if (is.null(rv$current.obj) || is.null(rv$current.obj.name)) {
      w <- paste("Analysis pipeline") }
    else {
      w <- paste("Analysis pipeline for ", rv$current.obj.name, sep = "")
    }
    
    h4(w)
    
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
    
    selectInput(inputId = "seuilNA", 
                label = "Keep lines with at least x intensity values", 
                choices = choix, 
                selected = choix[[1]])
    
  })
  
  #########################################################
  output$MSnsetView <- renderPrint({
    rv$current.obj
    if (is.null(rv$current.obj)) {return(NULL)  }
    rv$current.obj
  })
  
  
  
#   isUniqueID <- reactive({
#     rv$tab1
#     if (is.null(rv$tab1)) {return(NULL)  }
#     
#     
#     t <- TRUE
#     if (is.null(input$idBox)){return(NULL)}
#    t <- (length(rv$tab1[, input$idBox]) == length(unique(rv$tab1[, input$idBox])))
#          
#          return(t)
# 
#   })
#   
#   
  
  output$warningNonUniqueID <- renderUI({
    input$idBox
    rv$tab1
  if (is.null(rv$tab1)) {return(NULL)  }
    if (is.null(input$idBox) || (input$idBox =="")) {return(NULL)  }
    
    t <- (length(rv$tab1[, input$idBox]) == length(unique(rv$tab1[, input$idBox])))
    
    if (!t){
      text <- "<font color=\"red\">
              Warning ! Your ID contains duplicate data. Please choose another one."
  
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
      if (is.na(indexForIDBox) || length(indexForIDBox) == 0) {indexForIDBox <- NULL}
      
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
                   multiple = TRUE, width='200%')
    
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
  
  
  output$log <- renderDataTable({
    rv$text.log
    if (is.null(rv$text.log)) {return (NULL)}
    print(str(rv$text.log))
    as.data.frame(rv$text.log)
  },
  option=list(pageLength=6,
              orderClasses = TRUE,
              autoWidth=FALSE,
              lengthChange=F,
              searching=0,
              info=0,
              # bCaseInsensitive = TRUE,
              columnDefs = 
                list(list(columns.width=c("60px"),
                          columnDefs.targets=c(list(0),list(1),list(2)))))
  )
  
  #########################################################
  output$References <- renderText({
    HTML("Laurent Gatto and Kathryn S. Lilley. MSnbase - an R/Bioconductor
         package for isobaric tagged mass spectrometry data visualization,
         processing and quantitation. <i>Bioinformatics </i>28, 288-289 (2012).
         <br><br>
         Poster : S. Wieczorek, F. Combes, A.-M. Hesse, C. Lazar, 
         C. Ramus, Y. Coute, C. Bruley, T. Burger,
         <i>A package R and a web application for the analysis of 
         quantitative proteomics data</i>, SMAP 2014, Lyon
         ")
  })
  
  ##' Quick overview of the MSnbase object
  ##' @author Florence Combes
  output$overview <- renderUI({
    rv$current.obj
    if (is.null(rv$current.obj)) {return(NULL)    }
    
    isolate({
      rv$current.obj
      NA.count <- apply(data.frame(exprs(rv$current.obj)), 
                        2, 
                        function(x) length(which(is.na(data.frame(x))==TRUE)) )
      pourcentage <- 100 * round(sum(NA.count)/
                                   (dim(exprs(rv$current.obj))[1]*
                                      dim(exprs(rv$current.obj))[2]), digits=4)
      
      nb.empty.lines <- sum(apply(
        is.na(as.matrix(exprs(rv$current.obj))), 1, all))
      #tags$code(
      tags$ul(
        tags$li(paste("There is", dim(exprs(rv$current.obj))[2], 
                      "samples in your data.", sep=" ")), 
        tags$li(paste("There is", dim(exprs(rv$current.obj))[1], 
                      "lines in your data.", sep=" ")), 
        tags$li(paste("Percentage of missing values:",
                      pourcentage , "%", sep=" ")),
        tags$li(paste("Number of lines with only NA values =",
                      nb.empty.lines , sep=" "))
      )
      #)
      
      
    })
  })
  
  
  output$overviewNewData <- renderPrint({
    rv$current.obj
    if (is.null(rv$current.obj)) {return(NULL)}
    
    isolate({
      print(paste("There is", dim(exprs(rv$current.obj))[2], 
                  "samples in your data."))
      print(paste("There is", dim(exprs(rv$current.obj))[1], 
                  "lines in your data."))
      NA.count<-apply(data.frame(exprs(rv$current.obj)), 
                      2, 
                      function(x) length(which(is.na(data.frame(x))==TRUE)) )
      pourcentage <- 100 * round(sum(NA.count)/
                                   (dim(exprs(rv$current.obj))[1]*
                                      dim(exprs(rv$current.obj))[2]), digits=4)
      print(paste("Percentage of missing values:",pourcentage , "%"))
      
      nb.empty.lines <- sum(apply(
        is.na(as.matrix(exprs(rv$current.obj))), 1, all))
      if (nb.empty.lines > 0){
        
        if( nb.empty.lines > 1){
          verb <- "are"
          plurial <- "s"} else {
            verb <- "is"
            plurial <- ""}
        
        print(paste("There ", verb, " : ",nb.empty.lines , 
                    " line",plurial," with only NA values !!"))
      }
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
  output$histoMV <- renderPlot({
    rv$current.obj
    if (is.null(rv$current.obj)){return(plot.new())}
    
    wrapper.mvHisto(rv$current.obj)
  })
  
  ##' distribution of missing values in current.obj
  ##' @author Samuel Wieczorek
  output$histo.missvalues.per.lines <- renderPlot({
    rv$current.obj
    if (is.null(rv$current.obj)){return(plot.new())}
    wrapper.mvPerLinesHisto(rv$current.obj, 
                    c(2:length(colnames(pData(rv$current.obj)))))
  })
  
  ##' distribution of missing values in current.obj
  ##' @author Samuel Wieczorek
  output$histo.missvalues.per.lines.per.conditions <- renderPlot({
    rv$current.obj
    if (is.null(rv$current.obj)){return(plot.new())}
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
  
  ##' Distribution of intensities in current.obj
  ##' @author Samuel Wieczorek
  output$viewDensityplot<- renderPlot({
    rv$current.obj
    input$labToHighlight
    input$lab2Show
    if (is.null(rv$current.obj) || (length(input$lab2Show) == 0))
    {return(plot.new())}
    wrapper.densityPlotD(rv$current.obj, input$lab2Show, input$labToHighlight)
  })
  
  
  output$viewComparisonNorm<- renderPlot({
    rv$current.obj
    input$perform.normalization
    
    compareNormalizationD(exprs(rv$dataset[[input$datasets]]), 
                          exprs(rv$current.obj), 
                          labels = pData(rv$current.obj)[,"Label"])
   
  })
  
  
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
    if (GetNbNA() == 0){
      choices <- normMethods
    } else {
      choices <- normMethods
    } 
    
    selectInput("normalization.method", 
                "Choose normalization method", 
                names(choices))
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
  
  ##' boxplot of intensities in current.obj
  ##' @author Samuel Wieczorek
  output$viewBoxPlot <- renderPlot({
    input$legendXAxis
    rv$current.obj
    if (is.null(rv$current.obj)){return(plot.new())}
    
    isolate({
      input$legendXAxis
      rv$current.obj
      .axis <- input$legendXAxis
      wrapper.boxPlotD(rv$current.obj,  .axis)
    })
    
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
    if (is.null(rv$current.obj)){return(plot.new())}
    if (is.null(input$expGradientRate)){return(NULL)}
    print(input$expGradientRate)
    wrapper.corrMatrixD(rv$current.obj, rate = input$expGradientRate)
  })
  
  
#   output$ChooseLegendForHeatmap <- renderUI({
#     rv$current.obj
#     if (is.null(rv$current.obj)){return(NULL)}
#     isolate(rv$current.obj)
#     .names <- colnames(pData(rv$current.obj))[-1]
#     tags$head(tags$link(rel="stylesheet", type="text/css",
#                         href="css/overrides.css"))
#     
#     checkboxGroupInput("heatmap.legend",
#                        label = "Choose data to show in legend",
#                        choices = .names,
#                        selected = .names[1])
#   })
#   
  
#   buildHeatmapPlot <- reactive({
#     wrapper.heatmapD(rv$current.obj,input$distance, input$linkage)
#   })
#   
  ##' Draw a heatmap of current data
  ##' 
  ##' @author Samuel Wieczorek
  output$heatmap <- renderPlot({
    rv$current.obj
    input$linkage
    input$distance
    
    if (is.null(rv$current.obj) || is.null(input$linkage) || is.null(input$distance)) {return(NULL)
  #return(plot.new())
  }
    
    if (getNumberOfEmptyLines(exprs(rv$current.obj)) != 0) {
      return(NULL)
     # plot.new()
      }
    else {  
      plot.new()
      wrapper.heatmapD(rv$current.obj,input$distance, input$linkage) 
      #buildHeatmapPlot()
    }
  })  
  
  ##' Select the labels to be highlighted in densityplots
  ##' @author Samuel Wieczorek
  output$nGroup <- renderUI({
    rv$current.obj
    input$lab2Show
    if (is.null(rv$current.obj) ) {return(NULL) }
    
    #label.names <- unique(pData(rv$current.obj)[,"Label"])
    #setNames(as.list(label.names),label.names)
    # if (length(input$lab2Show) != 0){ 
    l <- setNames(as.list(c("none",input$lab2Show)),c("none",input$lab2Show))
    radioButtons("labToHighlight", "Condition to highlight", choices=l)
    # }
  })
  
  
  ##' Select the labels to show in densityplots
  ##' @author Samuel Wieczorek
  output$nShow <- renderUI({
    rv$current.obj
    if (is.null(rv$current.obj) ) {return(NULL) }
    
    isolate(
      rv$current.obj)
    label.names <- unique(pData(rv$current.obj)[,"Label"])
    label.names <- setNames(as.list(label.names),label.names)
    checkboxGroupInput("lab2Show"
                       , label = "Condition to show"
                       , choices = label.names
                       , selected = unlist(label.names)
    )
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
     print(rv$seuilPVal)
  })
  
  observe({
    if (!is.null(input$seuilLogFC)){rv$seuilLogFC <- input$seuilLogFC}
    print(rv$seuilLogFC)
  })
  
  
  output$nbSelectedItems <- renderUI({
    rv$seuilPVal
    rv$seuilLogFC
    input$condition1
    input$condition2
    input$diffAnaMethod
    rv$current.obj

    if (is.null( input$diffAnaMethod) || (input$diffAnaMethod == "None")){return(NULL)}
    
    p <- RunDiffAna()
     
    upItemsPVal <- NULL
    upItemsLogFC <- NULL
    
    if (!is.null(rv$seuilPVal)) {upItemsPVal <- which(-log10(p$P.Value) >= rv$seuilPVal)}
    
    if (!is.null(rv$seuilPVal)) {upItemsLogFC <- which(abs(p$logFC) >= rv$seuilLogFC)}
    
    
    nbTotal <- nrow(exprs(rv$current.obj))
    nbSelected <- NULL
    t <- NULL
    
    if (!is.null(rv$seuilPVal) && !is.null(rv$seuilLogFC) ) {t <- intersect(upItemsPVal, upItemsLogFC)}
    else if (!is.null(rv$seuilPVal) && is.null(rv$seuilLogFC) ) {t <- upItemsPVal}
    else if (is.null(rv$seuilPVal) && !is.null(rv$seuilLogFC) ) {t <- upItemsLogFC}
    
    nbSelected <- length(t)
    
    txt <- paste("Total number of peptides/proteins = ", 
                 nbTotal,"<br>",
                 "Number of selected peptides/proteins = ", 
                 nbSelected,"<br>",
                 "Number of non selected peptides/proteins = ", 
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
    
    if (is.null( input$diffAnaMethod) || (input$diffAnaMethod == "None")){return(NULL)}
    
    p <- RunDiffAna()
    
    upItemsPVal <- NULL
    upItemsLogFC <- NULL
    
    if (!is.null(rv$seuilPVal)) {upItemsPVal <- which(-log10(p$P.Value) >= rv$seuilPVal)}
    
    if (!is.null(rv$seuilPVal)) {upItemsLogFC <- which(abs(p$logFC) >= rv$seuilLogFC)}
    
    
    nbTotal <- nrow(exprs(rv$current.obj))
    nbSelected <- NULL
    t <- NULL
    
    if (!is.null(rv$seuilPVal) && !is.null(rv$seuilLogFC) ) {t <- intersect(upItemsPVal, upItemsLogFC)}
    else if (!is.null(rv$seuilPVal) && is.null(rv$seuilLogFC) ) {t <- upItemsPVal}
    else if (is.null(rv$seuilPVal) && !is.null(rv$seuilLogFC) ) {t <- upItemsLogFC}
    
    nbSelected <- length(t)
    
    txt <- paste("Total number of peptides/proteins = ", 
                 nbTotal,"<br>",
                 "Number of selected peptides/proteins = ", 
                 nbSelected,"<br>",
                 "Number of non selected peptides/proteins = ", 
                 (nbTotal-nbSelected), sep="")
    HTML(txt)
  })
  
  
  
  
  observe({
    rv$current.obj
    if (is.null(rv$current.obj)){return(NULL)}
    isolate({
      
      #Si on a deja des pVal, alors, ne pas recalculer 
      if ("logFC" %in% names(fData(rv$current.obj) )){
        updateNumericInput(session, "seuilLogFC", value= rv$current.obj@experimentData@other$threshold.logFC)
        updateNumericInput(session, "seuilPVal", value= rv$current.obj@experimentData@other$threshold.p.value)
        updateSelectInput(session, "diffAnaMethod", selected = rv$current.obj@experimentData@other$method)
        updateRadioButtons(session, "condition1", selected = rv$current.obj@experimentData@other$condition1)
        updateRadioButtons(session, "condition2", selected = rv$current.obj@experimentData@other$condition2)
        updateRadioButtons(session, "calibrationMethod", selected = rv$current.obj@experimentData@other$calibrationMethod)
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
        
        print(rv$seuilPVal)
        print(rv$seuilLogFC)
        
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
    
    t <- sessionInfo()
    daparVersion <- t$otherPkgs$DAPAR$Version
    ProstarVersion <- installed.packages()["Prostar","Version"]
    
    
    text <- paste("This application is an interface to several statistical 
                  tools based on the MSnset format.<br>
                  It is designed to be helpful in the domain of proteomic 
                  analysis by mass spectrometry.<br> <br>
                  
                  
                  It is composed of two parts which are R packages : <br>", 
                  "<ul style=\"list-style-type:disc;\">
                  <li>
                  <a href=\"http://www.bioconductor.org/packages/release/bioc/html/Prostar.html\"
                  title=\"here\" target=\"_blank\">Prostar</a> 
                  package (version ",ProstarVersion, ") that contains the GUI itself (direct link to the 
                  <a href=\"http://bioconductor.org/packages/release/bioc/vignettes/Prostar/inst/doc/Prostar_UserManual.pdf\"
                  title=\"here\" target=\"_blank\">User manual</a>)
                  </li>
                  <li>
                  <a href=\"http://www.bioconductor.org/packages/release/bioc/html/Prostar.html\"
                  title=\"here\" target=\"_blank\">DAPAR</a>
                  (version ",daparVersion, 
                  ") that is a collection of tools and graphs dedicated to 
                  proteomic analysis</li>
                  </ul>" , sep="")
    
    HTML(text)
    
    
    
    #tags$code("test")
    
    
    #                 <div class="highlight"><pre><code class="r">tags<span class="o">$</span>div<span class="p">()</span>
    #                         <span class="c1">## &lt;div&gt;&lt;/div&gt; </span>
    #                         </code></pre></div>
    
    
  })
  
  ########################################################
  output$limmaplot <- renderDataTable({
    rv$current.obj
    
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
      print(t)
    } else{
      data <- RunDiffAna()
      upItems1 <- which(-log10(data$P.Value) >= rv$seuilPVal)
      upItems2 <- which(abs(data$logFC) >= rv$seuilLogFC)
      selectedItems <- intersect(upItems1, upItems2)
      t <- data.frame(id =  rownames(exprs(rv$current.obj))[selectedItems],
                      data[selectedItems,])
      print(t)
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
    print(rv$dirname)
    saveRDS(obj,file=paste(rv$dirname,"/", name, fileExt,sep=""))
    return(obj)
  }
  
  ############ Read text file ######################
  observe({
    input$file1
     input$XLSsheets
    if (is.null(input$file1) ) {return(NULL)  }
     if (((GetExtension(input$file1$name)== "xls") || (GetExtension(input$file1$name) == "xlsx") ) && is.null(input$XLSsheets)) {return(NULL)  }
    
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
  
  
  observe({
    if (is.null(input$perform.filtering.MV) ){return(NULL)}
    if (input$perform.filtering.MV == 0){return(NULL)}
    #if (is.null(rv$current.obj)){return(NULL)}
    
    isolate({
     # rv$current.obj
      #print(input$ChooseFilters)
      if (input$ChooseFilters == gFilterNone){
        rv$current.obj <- rv$dataset[[input$datasets]]
      } else {
       
        rv$current.obj <- mvFilter(rv$dataset[[input$datasets]],
                                   input$ChooseFilters,
                                   input$seuilNA,
                                   GetFilterText(input$ChooseFilters, input$seuilNA) )
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
    #if (is.null(rv$current.obj)){return(NULL)}
    
    isolate({
      
      if (input$idBoxContaminants == "" && input$idBoxReverse ==""  ){
        rv$current.obj <- rv$dataset[[input$datasets]]
      } else {
        temp <-rv$dataset[[input$datasets]]
        
        if (!is.null(input$idBoxContaminants)) {
          temp <- removeLines(temp,input$idBoxContaminants, input$prefixContaminants)
           
        }
        
        if (!is.null(input$idBoxReverse)){
         temp <- removeLines(temp,input$idBoxReverse, input$prefixReverse)
          
        }
        
        rv$current.obj <- temp

      }
      updateSelectInput(session, "idBoxReverse", selected = input$idBoxReverse)
      updateSelectInput(session, "idBoxContaminants",selected = input$idBoxContaminants)
      updateSelectInput(session, "prefixContaminants", selected = input$prefixContaminants)
      updateSelectInput(session, "prefixReverse",selected = input$prefixReverse)
      
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
        rv$dataset[["Filtered"]] <- rv$current.obj
        updateSelectInput(session, "datasets", 
                          choices = names(rv$dataset), selected = "Filtered")
        txtFilterMV <- paste("Filtering :",
                     GetFilterText(input$ChooseFilters, input$seuilNA), sep="")
        txt <- paste(txtFilterMV, "Contaminants deleted", "Reverse deleted", sep=" ")
        UpdateLog(txt,"Filtered")
      }
      
    })
  })
  
  output$chooseProteinId <- renderUI({
    rv$current.obj
    if (is.null(rv$current.obj)) {return (NULL)}
    
    selectInput("proteinId", 
                "Prot ID",
                choices = c("None",colnames(fData(rv$current.obj))))
  })
  
  observe({
    #input$eData.box
    input$fData.box
    choices = colnames(rv$tab1)[-which(colnames(rv$tab1) %in% input$fData.box)]
    names(choices) = 
      colnames(rv$tab1)[-which(colnames(rv$tab1) %in% input$fData.box)]
    updateSelectInput(session, "eData.box", 
                      label = "",
                      choices = choices,
                      selected = choices)
    
  })
  
  #---------------------------------------
  help_tabPanel <- function(){
    tabsetPanel(id = "tabAbout",
                AboutTabPanel(),
                tabPanel(title="The MSnset format",
                         value = "tabHelpMSnset",
                         HTML("You can access the package 
                              and read the document 
                              <a href=\"http://www.bioconductor.org/packages/release/bioc/html/MSnbase.html\"
                              title=\"here\" target=\"_blank\">here</a>. <br>")
                ),
                tabPanel(title="Refs", htmlOutput("References")
                )
    )
  }
  
  openFile_tabPanel <- function(){
    tabPanel(
      #title="Open a MSnset file",
             #icon = icon("file"),
             value = "open",
             h2("Open a MSnset file"),
             fileInput("file", "", 
                       multiple=FALSE, 
                       accept=c("MSnset", "MSnSet")),
             br(),
             hr(),
             h3("Quick overview of the dataset"),
             uiOutput("overview")
    )
    
  }
  
  #         observe({
  #                 if (is.null(rv$current.obj)) {return (NULL)}
  #                 if (is.null(input$distance)) {return (NULL)}
  #                 if (is.null(input$linkage)) {return (NULL)}
  #                         t <- buildHeatmapPlot()
  # 
  #         })
  
  AggregationTabPanel <- function(){
    
    tabsetPanel(
      title = "agreagationTabsetPanel",
      id = "agreagationTabsetPanel",
      tabPanel(title = "1 - Agregate peptides",
               value = "aggregation",
               helpText("Please select first the id of protein in your dataset. Then, the stats
                        will be showed and it will be possible to perform the aggregation"),
               conditionalPanel(
                 condition = 'input.datasets != "Aggregated"',
                 fluidRow(
                   column(width=3, uiOutput("chooseProteinId")),
                   column(width=7, uiOutput("aggregationStats"))
                 ),
                 
                 fluidRow(
                   column(width=6, h4("Only unique peptides")),
                   column(width=6, h4("All (unique & shared) peptides"))
                 ),
                 
                 fluidRow(
                   column(width=6, plotOutput("aggregationPlotUnique")),
                   column(width=6, plotOutput("aggregationPlotShared"))
                 ),
                 
                 fluidRow(
                   column(width=3,
                          checkboxInput("checkSharedPeptides", 
                                        "Include shared peptides", 
                                        value = FALSE)),
                   column(width=3, 
                          selectInput("aggregationMethod", 
                                      "Aggregation methods",
                                      choices =  gAgregateMethod)),
                   column(width=4, 
                          conditionalPanel(
                            condition='input.aggregationMethod == "sum on top n"',
                            numericInput("nTopn", "nTopn", 
                                         value = NULL,
                                         min = 0)))),
                 
                 fluidRow(
                   column(width=3,
                          actionButton("perform.aggregation", "Aggregate")),
                   column(width=3,uiOutput("ObserverAggregationDone"))
               )
               
      )),
      tabPanel(title = "2 - Configure protein dataset",
               value = "configureProteinDataset",
               helpText("Select the columns of the meta-data (related to proteins) that have to be recorded in the new protein dataset."),
               div(class="row"),
               div(class="span5", "",
                   uiOutput("columnsForProteinDataset"),
                   
                   fluidRow(
                 column(width=3,
                        actionButton("valid.aggregation","Save aggregation", styleclass = "primary"))
               )
               )
      )
)

  }
  
  output$ObserverAggregationDone <- renderUI({
    rv$temp.aggregate
    if (is.null(rv$temp.aggregate) ) 
    {return(NULL)  }
    else {
      h3("Aggregation done !")
    }
  })
  
  
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
      l <- list(paste("ProStaR", selected.leaf, sep = "     -    ")) }
    else { l <- list("ProStaR")}
    
      headerPanel(l,windowTitle="ProStaR"  )
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
                  Number of peptides: ", nrow(matAdj$matWithSharedPeptides),
                  "</li>

                  <li>
                  Number of unique peptides: ", nrow(matAdj$matWithUniquePeptides),
                  "</li>


                  <li>
                  Number of shared peptides: ", nrow(matAdj$matWithSharedPeptides)-nrow(matAdj$matWithUniquePeptides),
                  "</li>

                  <li>
                  Number of proteins:  ", ncol(matAdj$matWithSharedPeptides),
                  " </li>

                  <li>
                  Number of proteins only defined by unique peptides: ", ncol(matAdj$matWithUniquePeptides), 
                  "</li>

                  <li>
                  Number of proteins only defined by shared peptides:  ", ncol(matAdj$matWithSharedPeptides)-ncol(matAdj$matWithUniquePeptides), 
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
  
  
  observe({
    input$perform.aggregation
    if (is.null(input$perform.aggregation) || (input$perform.aggregation == 0))
    {return(NULL)}
    
    isolate({
      rv$temp.aggregate <- RunAggregation()
    })
  })
  
  
  output$choosePrefixContaminants <- renderUI({
    rv$current.obj
    input$idBoxContaminants
    if (is.null(rv$current.obj)) {return(NULL)  }
    if (is.null(input$idBoxContaminants)) {return(NULL)  }
    
    textInput("prefixContaminants", label = "Choose prefix for contaminant",value = "")
  })
  
  
  output$choosePrefixReverse <- renderUI({
    rv$current.obj
    input$idBoxReverse
    if (is.null(rv$current.obj)) {return(NULL)  }
    if (is.null(input$idBoxReverse)) {return(NULL)  }
    
    textInput("prefixReverse", label = "Choose prefix for reverse", value = "" )
    
  })
  
  
  
  output$id_Contaminants <- renderUI({
    rv$current.obj
    if (is.null(rv$current.obj)) {return(NULL)  }
    
    
    .choices <- c("",colnames(fData(rv$current.obj)))
    names(.choices) <- c("",colnames(fData(rv$current.obj)))
    selectInput("idBoxContaminants", label = "", choices = .choices , selected = NULL)
    
    
  })
  
  
  output$id_Reverse <- renderUI({
    rv$current.obj
    if (is.null(rv$current.obj)) {return(NULL)  }
    
    
    .choices <- c("",colnames(fData(rv$current.obj)))
    names(.choices) <- c("",colnames(fData(rv$current.obj)))
    selectInput("idBoxReverse", label = "", choices = .choices , selected = NULL)
    
  })
  
  
  filter_Complete_tabPanel <- reactive({
    tabPanel(
      title="Convert data",
     # icon = icon("download"),
      value = "filtering",
      tabsetPanel(id = "tabFilter",
                  tabPanel( "1 - Missing values",
                            value = "FilterMissingValues",
                            h4("Filtering options"),
                            
                            helpText("The filter below allows keeping the lines that contain 
                      a certain amount of quantitative data rather than NA values. 
                                     The threshold to define correponds to the number of quantitative values in a 
                                     line and means that the lines which contain at least this threshold value 
                                     are kept. This filtering threshold may be applied on the whole  dataset, on 
                                     each condition or on at leat one condition."),
                            
                            fluidRow(
                              column(width=4, 
                                     radioButtons("ChooseFilters","", choices = gFiltersList)),
                              column(width=4, 
                                     conditionalPanel(
                                       condition='input.ChooseFilters != "None"',
                                       uiOutput("seuilNADelete")))),
          
                            
                            fluidRow(
                              column(width = 4, plotOutput("histoMV")),
                              column(width = 4,plotOutput("histo.missvalues.per.lines")),
                              column(width = 4,plotOutput("histo.missvalues.per.lines.per.conditions"))
                            )
                            ,
                            column(width=4, 
                                   actionButton("perform.filtering.MV", 
                                                "Perform filtering MV"))
                            
                  
                            ),
                  tabPanel( "2 - Contaminants",
                            value = "FilterContaminants",
                            
                            helpText("Please select among the columns of
                                     your data the one that corresponds 
                                     to the contaminants"),
                            
                            
                            fluidRow(
                              column(width=4,uiOutput("id_Contaminants")),
                              column(width=4,uiOutput("choosePrefixContaminants"))
                            ),
                            
                           
                            helpText("Please select among the columns of
                                     your data the one that corresponds 
                                     to the reverse peptides/proteins"),
                            
                            fluidRow(
                              column(width=4,uiOutput("id_Reverse")),
                              column(width=4,uiOutput("choosePrefixReverse"))
                               ),
                             
                            plotOutput("GlobalPieChart"),
                            
                            column(width=4, 
                                   actionButton("perform.filtering.Contaminants", 
                                                "Perform filtering contaminants"))
                            
                  ),
                  tabPanel( "3 - Visualize and Validate",
                            value = "FilterValidate",
                            helpText("After checking the data, validate the filters"),
                            fluidRow(
                             
                              column(width=4,
                                     actionButton("ValidateFilters","Save filtered dataset",  
                                                  styleclass = "primary")
                              )
                            )
                            
                            
                            
                            )
                  
                  )
    )
    
  })
  
  
  
  
  
#   
#   filter_tabPanel <- function(){
#     tabPanel(
#       #title="Filtering",
#              value = "filtering",
#              #h3("Filtering tools"),
#              br(),
#              
#              helpText("The filter below allows keeping the lines that contain 
#                       a certain amount of quantitative data rather than NA values. 
#                       The threshold to define correponds to the number of quantitative values in a 
#                       line and means that the lines which contain at least this threshold value 
#                       are kept. This filtering threshold may be applied on the whole  dataset, on 
#                       each condition or on at leat one condition."),
#              
#              h4("Filtering options"),
#              fluidRow(
#                column(width=4, 
#                       radioButtons("ChooseFilters",
#                                    "", 
#                                    choices = gFiltersList)),
#                column(width=4, 
#                       conditionalPanel(
#                         condition='input.ChooseFilters != "None"',
#                         uiOutput("seuilNADelete")))),
#              
#              
#              fluidRow(
#                column(width=4, 
#                       actionButton("perform.filtering", 
#                                    "Perform filtering")),
#                column(width=4,
#                       actionButton("ValidateFilters","Save filtered dataset",  
#                                    styleclass = "primary")
#                )
#              )
#              
# #              fluidRow(
# #                column(width = 4, plotOutput("histoMV")),
# #                column(width = 4,plotOutput("histo.missvalues.per.lines")),
# #                column(width = 4,plotOutput("histo.missvalues.per.lines.per.conditions"))
# #              )
# #              
#              
#              )
#     
#   }
#   
  AboutTabPanel <- function(){
    tabPanel(
      title="About",
             value = "tabHelpMSnset",
             
             #htmlOutput("aboutText")
             uiOutput("aboutText")
    )
    
  }
  
  import_tabPanel <- function(){
    tabPanel(
      #title="Convert data",
             icon = icon("download"),
             value = "import",
             helpText("These steps allow to create a MSnSet file from a tabulated-text file."),
             tabsetPanel(id = "tabImport",
                         tabPanel( "1 - Select file",
                                   value = "SelectFile2Import",
                                  
                                   fileInput("file1", "Data file", 
                                             multiple=FALSE, 
                                             accept=c(".txt", ".csv",".xls", ".xlsx")),
                                   uiOutput("ManageXlsFiles"),
                                   
                                   helpText("Hint : before importing quantification 
                                            file data, check the syntax 
                                            of your text file."),
                                   br(),
                                   wellPanel(
                                     radioButtons("typeOfData", "Is it a peptide or protein dataset ?", 
                                                  choices=c("peptide dataset" = "peptide", 
                                                            "protein dataset" = "protein")
                                                  ),

                                     radioButtons("checkDataLogged", "Check whether the data you want to analyze are already logged or not.
                                              If not, they will be automatically logged", 
                                                  choices=c("yes", "no"), 
                                                  selected="no"),
                                     br(),
                                     checkboxInput("replaceAllZeros", 
                                                   "Replace all 0 and NaN by NA", 
                                                   value= TRUE)
                                   )
                                   ),
                         tabPanel( "2 - Data Id",
                                   value = "ID",
                                   helpText("Please select among the columns of
                                            your data the one that corresponds 
                                            to a unique ID of the peptides/proteins.",
                                            style = "color:black"),
                                   
                                    radioButtons("autoID", 
                                                 "If you choose the automatic ID, Prostar will build an index.", 
                                                  choices=c("Auto ID" = "Auto ID", 
                                                            "user ID" = "user ID")),
                                    
                                   conditionalPanel(
                                     condition = 'input.autoID == "user ID"',
                                     uiOutput("id"),
                                   uiOutput("warningNonUniqueID"))
                         ),
                         tabPanel( "3 - Exp. and feat. data",
                                   value = "Import1",
                                   helpText("Select the columns that are quantitation values 
                                            by clicking in the fiels below."),
                                   div(class="row"),
                                   div(class="span5", "Quantitative  Data",
                                       uiOutput("eData"))
                                   ),
                         tabPanel( "4 - Samples metadata",
                                   value = "Import2",
                                   #helpText("TODO"),
                                   helpText("Attention : it is mandatory that the column 
                                            \"Label\" is filled."),
                                   br(),
                                   rHandsontableOutput("hot")
                         ),
                         tabPanel( "5 - Convert",
                                   value = "Convert",
                                   htmlOutput("msgAlertCreateMSnset"),
                                   fluidRow(
                                     column(width = 6, 
                                            textInput("filenameToCreate",
                                                      "Enter the name of the study")),
                                     column(width = 6, 
                                            actionButton("createMSnsetButton",
                                                         "Convert data"))
                                     
                                   ),
                                   uiOutput("conversionDone")
                         )
                         )
    )
    
  }
  
  
  
  output$conversionDone <- renderUI({
    rv$current.obj
    if (is.null(rv$current.obj)) { return(NULL)}
    
    h4("The conversion is done. Your dataset has been automatically loaded in memory. Now, you can switch to the 
       Descriptive statistics panel to vizualize your data.")
    
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
  
  #-------------------------------------------------------------
  export_tabPanel <- function(){
    tabPanel(
      #title="Export",
             value = "export",
             icon = icon("upload"),
             helpText("Choose the export format of the dataset and choose a name."),
             #helpText("TODO"),
             selectInput("fileformatExport", "File format", 
                         choices=  c( "MSnset","Excel")),
             
             conditionalPanel(
               condition = "input.fileformatExport == 'Excel'",
               uiOutput("selectIDforExcelExport")
             ),
             
             textInput("nameExport", "Enter the name of the files to be created"),
             downloadButton('downloadMSnSet', 'Download')
    )
  }
  
  
  ViewHeatmapTabPanel <- reactive({
    tabPanel(title="Heatmap",value="tabheatmap",
             #helpText("TODO"),
             helpText("For this view, it is necessary that your dataset does 
                      not contains any NA lines
                      Please check your data and use Filtering options or 
                      missing values imputation.",
                      style = "color:black"),
             
             uiOutput("heatmapOptions"),
             
             
             plotOutput("heatmap")
    )
  })
  
  
  
  output$heatmapOptions <- renderUI({
    rv$current.obj

    if (is.null(rv$current.obj)) {return(plot.new())}
    if (getNumberOfEmptyLines(exprs(rv$current.obj)) != 0) {return (NULL)}
      
    h3("Clustering Options")
    
    fluidRow(
      column(width = 3,
             radioButtons("distance","Distance",
                          choices = list(euclidean ="euclidean",
                                         manhattan="manhattan"))
      ),
      column(width = 3,
             radioButtons("linkage","Linkage for clustering",
                          choices=list(average="average",
                                       ward.D="ward.D"))
      )
    )
  })
  #------------------------------------------------------------------------
  ViewMSnsetTabPanel <- function(){
    tabPanel(title="Data explorer",
             value = "tabular view",
             h2("Viewer of the content of a MSnset file"),
             tabsetPanel(id = "tabViewData",
                         tabPanel("Quantitative data",value = "tabExprs", 
                                  checkboxInput("nDigits", "Show full length number", value = FALSE),
                                  dataTableOutput("viewExprs")),
                         tabPanel("Analyte metadata",value = "tabfData", 
                                  dataTableOutput("viewfData")),
                         tabPanel("Replicate metadata",value = "tabpData",
                                  dataTableOutput("viewpData")),
                         tabPanel("Dataset history",value = "processingData",
                                  helpText("Previous operations made on the original dataset :"),
                                  dataTableOutput("viewProcessingData"))
             )
    )
  }
  
  #----------------------------------------------------------
  ViewGraphicsTabPanel <- function(){
    tabPanel("Descriptive statistics",
             value="tabView",
             icon = icon("bar-chart-o"),
             tabsetPanel(id="View1",
                         GeneralTabPanel(),
                         OverviewTabPanel(),
                         ViewMSnsetTabPanel(),
                         ViewHeatmapTabPanel(),
                         tabPanel(title="Corr. matrix",
                                  value="tabCorrMatrix",
                                  #helpText("TODO"),
                                  sliderInput("expGradientRate", 
                                              "Modify the rate to modify the gradient of color", 
                                              min = 0, 
                                              max = 10, 
                                              value = 5, 
                                              step=0.05),
                                  #uiOutput("ChooseLegendForAxis2"),
                                  plotOutput("corrMatrix",
                                             height="500px", 
                                             width="800px")),
                         
                         tabPanel(title = "Boxplot", 
                                  value="tabboxplot",
                                  # helpText("TODO"),
                                  uiOutput("ChooseLegendForAxis"),
                                  plotOutput("viewBoxPlot", 
                                             height= "500px", 
                                             width="600px")
                         ),
                         
                         tabPanel(title="Variance distr.", value="tabDistVar",
                                  p("This graphics shows, for each condition, 
                                    the distribution of the variance of the 
                                    log-intensities."),
                                  plotOutput("viewDistVariance",
                                             height="500px", 
                                             width="800px")
                                  ),
                         
                         DensityPlotTabPanel()
             )
  )   
  }
  
  #-----------------------------------------------------------------------
  NormalizationTabPanel <- reactive({
    rv$current.obj
    tabPanel(title="1 - Normalization",
             value = "Normalization",
             helpText("Select a normalization method before performing normalization on the dataset"),
             
             fluidRow(
               column(width=4, uiOutput("choose_Normalization_Test")),
               column(width=3, 
                      actionButton("perform.normalization", 
                                   "Perform normalization")),
               column(width=3, 
                      actionButton("valid.normalization",
                                   "Save normalization", 
                                   styleclass = "primary"))),
             br(),
             uiOutput("helpForNormalizationMethods"),
             hr(),
              fluidRow(
                column(width=4, uiOutput("ChooseLegendForAxis")),
                column(width = 4, uiOutput("nShow") ),
                column(width = 4, uiOutput("nGroup"))),
             # 
             fluidRow(
               column(width=4, plotOutput("viewBoxPlot")),
               column(width=4, plotOutput("viewDensityplot")),
               column(width=4, plotOutput("viewComparisonNorm"))
             )
    )
  })
  
  
  
  output$helpForNormalizationMethods <- renderUI({
    input$normalization.method
    if (is.null(input$normalization.method) || (input$normalization.method == "None")) {return(NULL)}
    toto <- input$normalization.method
    
   
    switch(input$normalization.method,
           "Global Rescaling - sum by columns" = {t <- "The abundance of each protein is divided by the total abundance of all the proteins
in the same replicates. This normalization is interesting to compare the proportions of a given
           protein in different samples that do not necessarily contain the same amount of biological material.
           Contrarily to the others, this normalization is not performed on the log2 scale, for it would not
           have any interpretation (the data are thus exponentiated and re-log2-transformed as pre-and
           post-processing)."},
           "Global Rescaling - quantiles" = {t <- "The protein abundance are roughly replaced by the order statics on their abundance (from
package preprocessCore). This is the strongest normalization method available, and it should be
           use carefully, for it erazes most of the difference between the samples."},
           
           "Median Centering - overall" = {t <- "The medians of the samples are aligned. To do so, one computes
first the median for each
           replicates. Then, to each abundance value, one subtracts the corresponding median.
           Finally, one adds to this abundance value, an offset in order to find roughly back the original range
           of values. This offset is the mean of all the medians,
           whatever the conditions (then, any global difference between the conditions will disappear). Note that all these computations are performed on values
           that were originaly log2-transformed."},
           
           "Median Centering - within conditions" = {t <- "The medians of the samples are aligned. To do so, one computes
first the median for each
           replicates. Then, to each abundance value, one subtracts the corresponding median.
           Finally, one adds to this abundance value, an offset in order to find roughly back the original range
           of values. This offset is the mean of all the medians within each conditions (then, any global difference
           between the conditions is preserved). Note that all these computations are performed on values
           that were originaly log2-transformed."},
           
           "Mean Centering - overall" = {t <- "The means of the samples are aligned. To do so, one computes
first the mean for each
           replicates. Then, to each abundance value, one subtracts the corresponding mean.
           Finally, one adds to this abundance value, an offset in order to find roughly back the original range
           of values. This offset is the mean of all the means,
           whatever the conditions (then, any global difference between the conditions will disappear). Note that all these computations are performed on values
           that were originaly log2-transformed."},
           
           "Mean Centering - within conditions" = {t <- "The means of the samples are aligned. To do so, one computes
           first the mean for each
           replicates. Then, to each abundance value, one subtracts the corresponding mean.
           Finally, one adds to this abundance value, an offset in order to find roughly back the original range
           of values. This offset is the mean of all the means within each conditions (then, any global difference
           between the conditions is preserved). Note that all these computations are performed on values
           that were originaly log2-transformed."},
           
           
           "Mean Centering Scaling - overall" = {t <- "The spirit of this normalization is the same as the Mean Centering, yet,
it is stronger, and it only applies to log2-tranformed abundance values that distributes roughly
           normaly for each sample. Basically, a mean centering as described above is applied. Then, the
           variance of the distribution is re-scaled to 1. Let us note that median centering is not really
           adapted to a rescaling the variance; this is why such combination of parameters is not available.
           The centering operate over the entire dataset."},
           
           "Mean Centering Scaling - within conditions" = {t <- "The spirit of this normalization is the same as the Mean Centering, yet,
it is stronger, and it only applies to log2-tranformed abundance values that distributes roughly
           normaly for each sample. Basically, a mean centering as described above is applied. Then, the
           variance of the distribution is re-scaled to 1. Let us note that median centering is not really
           adapted to a rescaling the variance; this is why such combination of parameters is not available.
           The centering operate over each condition."},
           
           
           
           stop("Enter something that switches me!")
    )
    
    helpText(t)
  })
  
  #-----------------------------------------------------------
  DensityPlotTabPanel <- function(){
    tabPanel(title = "Densityplot",value="tabdensityplot",
             fluidRow(
               column(width = 2, uiOutput("nGroup")),
               column(width = 2, uiOutput("nShow")
               )),
             plotOutput("viewDensityplot", height="500px", width="600px")
    )
  }
  
  #-----------------------------------------------------------
  ImputationTabPanel <- function(){
    tabPanel(title="2 - Miss. values imputation",
             value = "imputation",
             helpText("Select an imputation method before performing the imputation of missing values."),
             fluidRow(
               column(width = 5, 
                      selectInput("missing.value.algorithm", 
                                  "Choose algorithm", 
                                  choices = names(imputationAlgorithms))),
               column(width = 3, 
                      actionButton("perform.imputation.button",
                                   "Perform imputation")),
               column(width = 3, 
                      actionButton("ValidImputation",
                                   "Save imputation", 
                                   styleclass = "primary"))
             ),
             
             fluidRow(
               column(width = 4, plotOutput("viewNAbyMean")),
               column(width = 8, plotOutput("showImageNA"))
             )
             
    )
  }
  
  #--------------------------------------------------------------------
  GeneralTabPanel <- function(){
    tabPanel("Overview",
             value = "tabGeneral",
             verbatimTextOutput("overviewNewData")
    )
  }
  
  #--------------------------------------------------------------------
  OverviewTabPanel <- function(){
    tabPanel("Miss. values",value = "taboverviewMV",
             helpText("Those bargraph plots display some information to view the distribution 
                      of missing values."),
             fluidRow(
               column(width = 4, plotOutput("histoMV")),
               column(width = 4,plotOutput("histo.missvalues.per.lines")),
             column(width = 4,plotOutput("histo.missvalues.per.lines.per.conditions")))
    )
  }
  
  
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
             dataTableOutput("log")
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
  
  
  # ---------------- diffAna TAB PANEL -----------------
  diffAnaTabPanelComplete <- reactive({
    rv$current.obj
    if (is.null(rv$current.obj)){
      print("No dataset is loaded. Please open a MSnSet file or create one before.")
      return(NULL)}
    
    
    
    tabPanel(title="Limma",
             value = "tabLimma",
             if (sum(is.na(exprs(rv$current.obj))==TRUE) > 0){
               h3("There are some NA in your data. please impute before.")
               return()
             },
             
     tabsetPanel(
       title = "abc",
       id = "abc",
       tabPanel(title = "1 - Choose groups",
                value = "Choosegroups",
                fluidRow(
                  column(width = 3, uiOutput("RenderLimmaCond1")),
                  column(width = 3, uiOutput("RenderLimmaCond2")),
                  column(width = 3, 
                         selectInput("diffAnaMethod", 
                                     "Choose test", 
                                     choices = c("None","Limma", "Welch")))
                ),
                br(),
                
                fluidRow(
                  column(width=2, numericInput("seuilLogFC", "log(FC)", 
                                               min = 0, 
                                               value = 0, 
                                               step=0.1)),
                  
                  column(width=7, uiOutput("nbSelectedItems") )
                                               
                ),
                plotOutput("volcanoplot", height="500px", width="600px")
 ),
       tabPanel(title = "2 - Calibrate Ana Diff",
                value = "Calibrate Ana Diff",
                
                htmlOutput("errMsgCalibrationPlotAll"),
                plotOutput("calibrationPlotAll"),
                fluidRow(
                  column(width = 3, selectInput("calibrationMethod", "Choose the calibration method", 
                            choices = c("st.boot", "st.spline", "langaas", 
                                        "jiang", "histo", "pounds", "abh","slim", "Benjamini-Hochberg", "numeric value"),
                            selected = "pounds")),
                  column(width = 3, 
                         uiOutput("numericalValForCalibrationPlot")
                )),
         
                uiOutput("errMsgCalibrationPlot"),
                plotOutput("calibrationPlot")
                ),
       
        tabPanel(title = "3 - Visualize FDR",
                 value = "viewFDR",
               # uiOutput("calibrationResults"),
                uiOutput("nbSelectedItemsStep3"),
                br(), br(), hr(),
               fluidRow(
                      column(width= 4, numericInput("seuilPVal", "-log10(p.value)", 
                             min = 0, 
                             value = 0, 
                             step=0.1)),
                      column(width= 4, htmlOutput("equivPVal")),
                      column(width= 4, htmlOutput("showFDR"))
                      ),
              plotOutput("volcanoplotStep3", height="500px", width="600px")
              ),
        
       tabPanel(title = "4 - Validate and Save",
                value = "ValidateAndSaveAnaDiff",
                
                dataTableOutput("limmaplot"),
                br(),
                fluidRow(
                  column(width=2, actionButton("ValidDiffAna","Save diff analysis"))
                ),
                fluidRow(
                  column(width=12, uiOutput("DiffAnalysisSaved"))
                )
     )
      )
    )
  })
  
  #------------------------------------------
  ##' Missing values imputation - reactivity behavior
  ##' @author Samuel Wieczorek
  observe({
    # input$perform.imputation.button
    if (is.null(input$perform.imputation.button) ){return(NULL)}
    if (input$perform.imputation.button == 0){return(NULL)}
    
    isolate({
      input$missing.value.algorithm
      rv$current.obj
      input$datasets
      .temp <- unlist(strsplit(input$missing.value.algorithm, " - "))
      if (.temp[1] == "None"){
        rv$current.obj <- rv$dataset[[input$datasets]]
      } else {
        if ((.temp[1] == "LeftCensored") || 
            (.temp[1] == "RandomOccurence")) 
        {
          
          rv$current.obj <- wrapper.mvImputation(rv$current.obj, .temp[2])
          
          updateSelectInput(session, 
                            "missing.value.algorithm", 
                            selected = input$missing.value.algorithm)
          
        }
        else if (input$missing.value.type == "Mix")
        {}
        
      }
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
      }
    })
  })
  
  
  
  output$processSteps <- renderUI({

  text <- "<font color=\"red\">

          The four processing steps are the following: <br> <br>
                <ul style=\"list-style-type:disc;\">
                <li>
                Descriptive  statistics: Exploration  and  visualization  of  your  dataset  with  a detailed  overview,  which  includes  the  following  functionalities:
                          <ul style=\"list-style-type:disc;\">
                          <li>Missing  Valuesexploration, </li>
                          <li>heatmap and correlation matrices, </li>
                          <li>boxplots,</li> 
                          <li>expectation and variancedistribution.</li>
                          </ul>
                </li>
<br> 
                <li>
                Filtering: alter proteins according to their number of missing values in each condition
                </li>
<br> 
               <li>
                Cross replicate normalisation, with the following methods:
                          <ul style=\"list-style-type:disc;\">
                          <li>(i) global rescaling (quantiles method, proportion method),</li> 
                          <li>(ii) median or mean centering (overall orwithin conditions), </li>
                          <li>(iii) mean centering and scaling (overall or within conditions).</li>
                          </ul>

                </li>

<br> 
                <li>
                Missing  values imputation:
                          <ul style=\"list-style-type:disc;\">
                          <li>(i)  for  random  occurences: k-nearest-neighbors, Bayesian Principal Component Analysis and Maximum Likelihood Estimation;</li>
                          <li>for left censored missing values: Quantile Regression for Imputation of Left Censored data.</li>
                          </ul>
                </li>
 <br>                
                <li>
                Differential analysis, according to a Welcht-test or a Limma moderated t-test.
                </li>
                
           </ul>
  
 </font>"
  
  HTML(text)
  })
  
  })