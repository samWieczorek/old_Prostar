

# 
# callModule(modulePopover,"modulePopover_toto", data = reactive(paste0(data(), 
#                                                                       a("tidy data paper", 
#                                                                         href = "http://vita.had.co.nz/papers/tidy-data.pdf",
#                                                                         target="_blank"))))

callModule(modulePopover,"modulePopover_pushPVal", data = reactive(list(title="P-value push",
content= "This functionality is useful in case of multiple pairwise omparisons (more than 2 conditions): At the filtering step, a given analyte X (either peptide or protein) may have been kept because it contains very few missing values in a given condition (say Cond. A), even though it contains (too) many of them in all other conditions (say Cond B and C only contains “MEC” type missing values). Thanks to the imputation step, these missing values are no longer an issue for the differential analysis, at least from the computational viewpoint. However, statistically speaking, when performing B vs C, the test will rely on too many imputed missing values to derive a meaningful p-value: It may be wiser to consider analyte X as non-differentially abundant, regardless the test result (and thus, to push its p-value to 1). This is just the role of the “P-value push” parameter. It makes it possible to introduce a new filtering step that only applies to each pairwise comparison, and which assigns a p-value of 1 to analytes that, for the considered comparison are assumed meaningless due to too many missing values (before imputation).")))

callModule(modulePopover,"modulePopover_convertDataQuanti", 
           data = reactive(list(title = "Quantitative data", 
                                content="Select the columns that are quantitation values by clicking in the field below.")))


callModule(modulePopover,"modulePopover_exportMetaData", 
           data = reactive(list(title = "Meta data", 
                                content="Select the columns you want to keep as metadata. By default, if any column is specified, all metadata in your dataset will be exported.")))


callModule(modulePopover,"modulePopover_exportFilename", 
           data = reactive(list(title = "Filename", 
                                content="Enter the name of the files to be created")))


callModule(modulePopover,"modulePopover_exportFileFormat", 
           data = reactive(list(title = "File format", 
                                content="File format")))



callModule(modulePopover,"modulePopover_convertChooseDatafile", 
           data = reactive(list(title = "Data file", 
                                content="Select one (.txt, .csv, .tsv, .xls, .xlsx) file.")))

callModule(modulePopover,"modulePopover_convertIdType", 
           data = reactive(list(title = "Type de ID", 
                                content="If you choose the automatic ID, Prostar will build an index.")))


##' Get back to a previous object ---------------------------------------
##' @author Samuel Wieczorek
observeEvent( input$datasets,ignoreInit = TRUE,{ 
    
  
    isolate({
        if (!is.null(input$datasets)) {
            rv$current.obj <- rv$dataset[[input$datasets]]
            
        if (!is.null( rv$current.obj))
            rv$typeOfDataset <- rv$current.obj@experimentData@other$typeOfData
        }
        
    })
})




###-------------------------------------------------------------------

session$onSessionEnded(function() {
    #setwd(tempdir())
    graphics.off()
    unlink(sessionID, recursive = TRUE)
    unlink(paste(tempdir(), sessionID, commandLogFile, sep="/"),recursive = TRUE)
    unlink(paste(tempdir(), sessionID, sep="/"),recursive = TRUE)
    unlink(paste(tempdir(), "*html", sep="/"))
    unlink("www/*pdf")
})


###-------------------------------------------------------------------
ClearUI <- reactive({
        
        #rv$commandLog <- ""
        updateSelectInput(session, 
                          "datasets",  
                          "Dataset versions", 
                          choices = G_noneStr)
        updateRadioButtons(session,"typeOfData",selected = typePeptide )
        updateRadioButtons(session, "checkDataLogged", selected="no")
        updateRadioButtons(session, "autoID", selected = "Auto ID")
        
        updateSelectInput(session, "idBox", selected = NULL)
        
        updateSelectizeInput(session,"eData.box",choices = NULL, selected=NULL)
        updateTextInput(session,"filenameToCreate",value= "")
        updateTextInput(session,"nameExport",value= "")
        
        #UpdateLog("Memory has been cleared","None")
        updateCheckboxInput(session, "replaceAllZeros",value = TRUE)
        updateRadioButtons(session,
                           inputId = "ChooseFilters", 
                           selected = gFilterNone)
        #updateSelectInput(session, "normalization.method",selected = "None")
        # updateSelectInput(session,"type.of.missvalues", selected= "Majoritary" )
        #updateSelectInput(session,"typeImputationMNAR",selected= "QRILC" )
        
    })






######################################
loadObjectInMemoryFromConverter <- reactive({
    req(rv$current.obj)
  rv$typeOfDataset <- rv$current.obj@experimentData@other$typeOfData
    if (is.null(rv$typeOfDataset)) {rv$typeOfDataset <- ""}
    
    #If there are already pVal values, then do no compute them 
    if (G_logFC_Column %in% names(Biobase::fData(rv$current.obj) )){
        rv$resAnaDiff <- list(logFC = Biobase::fData(rv$current.obj)$FC,
                              P_Value = Biobase::fData(rv$current.obj)$P_Value)
        rv$seuilLogFC <- rv$current.obj@experimentData@other$threshold_logFC
        rv$seuilPVal  <- rv$current.obj@experimentData@other$threshold_p_value
        
    }
    
    
    name <- paste ("Original", " - ", rv$typeOfDataset, sep="")
    rv$dataset[[name]] <- rv$current.obj
    

        txt <- paste("dataset <- list()","\n", "dataset[['", name,"']] <- current.obj","\n","typeOfDataset <- \"",  
                 rv$typeOfDataset, "\"", "\n",
                 "colnames(fData(current.obj)) <- gsub(\".\", \"_\", colnames(fData(current.obj)), fixed=TRUE)",
                 sep="")
        writeToCommandLogFile(txt)
    
    
    #if (!is.null(rv$current.obj@experimentData@other$OriginOfValues)){
    #    writeToCommandLogFile("current.obj@experimentData@other$OriginOfValues <- Matrix(as.numeric(!is.na(current.obj)),nrow = nrow(current.obj), sparse=TRUE)")
    #} 
   
    updateSelectInput(session, "datasets", 
                      label = paste("Dataset versions of",
                                    rv$current.obj.name, sep=" "),
                      choices = names(rv$dataset),
                      selected = name)

})

#




###-------------------------------------------------------------------
writeToCommandLogFile <- function(txt, verbose = FALSE){
    rv$commandLog <- c(rv$commandLog, txt)
    # cat(rv$commandLog,
    #     file = paste(tempdir(), sessionID, commandLogFile, sep="/"),
    #     txt,
    #     sep = "\n",
    #     append = TRUE)
    # if (verbose) print(txt)
}

###-------------------------------------------------------------------
dirSessionPath <- paste(tempdir(), sessionID, sep="/")
if (!dir.exists(dirSessionPath)){
    dir.create(dirSessionPath)
}





###-------------------------------------------------------------------
ClearMemory <- function(){
    
    rv$current.obj = NULL
    rv$current.obj.name = NULL
    rv$deleted.mvLines = NULL
    rv$deleted.stringBased.exprsData = NULL
    rv$deleted.stringBased = NULL
    rv$deleted.stringBased.fData = NULL
    rv$deleted.stringBased = NULL
    rv$DT_filterSummary = data.frame(Filtre=NULL, 
                                     Prefix=NULL,
                                     nbDeleted=NULL, 
                                     Total=NULL, 
                                     stringsAsFactors=F)
    # variable to keep memory of previous datasets before 
    # transformation of the data
    rv$dataset = list()
    # Variable that contains the log for the current R session
    rv$text.log = data.frame(Date="", 
                             Dataset="", 
                             History="", 
                             stringsAsFactors=F)
    rv$volcanoTooltip = NULL
    rv$listLogFC = list()
    rv$seuilLogFC = 0
    rv$seuilPVal = 1e-60
    rv$tab1 = NULL
    rv$dirname = ""
    rv$dirnameforlink = ""
    rv$conditions = list(cond1 = NULL, cond2 = NULL)
    rv$temp.aggregate = NULL
    rv$hot = port 
    rv$calibrationRes = NULL
    rv$errMsgcalibrationPlot = NULL
    rv$errMsgcalibrationPlotALL = NULL
    rv$typeOfDataset = ""
    rv$widthSidebar = 3
    rv$commandLog =  "" 
    rv$normalizationFamily = NULL
    rv$normalizationMethod = NULL 
    rv$matAdj = NULL
    test = NULL
    rv$resAnaDiff = list(FC=NULL, P_Value=NULL, condition1 = NULL, condition2 = NULL)
    rv$res_AllPairwiseComparisons = data.frame()
    indexNA = NULL
    rv$pourcentageNA = 0
    rv$nb.empty.lines = 0
    rv$nbDeleted = 0
    rv$nbDeletedInfos = NULL
    rv$fdr = NULL
    rv$ValidFilteringClicked = FALSE
    rv$ValidImputationClicked = FALSE
    rv$nbTotalAnaDiff = NULL
    rv$nbSelectedAnaDiff = NULL
    rv$nbSelectedTotal_Step3 = NULL
    rv$nbSelected_Step3 = NULL
    rv$groupGO_data = NULL
    rv$enrichGO_data = NULL
    rv$universeData = NULL
    rv$uniprotID = NULL
    rv$ratio = NULL
    rv$impute_Step = 0
    rv$hot = port
    rv$MECIndex = NULL
    rv$tempDatasetImputation = NULL
    rv$text.log <- data.frame(Date="", 
                              Dataset="", 
                              History="", 
                              stringsAsFactors=F)
    rv$ProtIDList = NULL
    rv$GOWarningMessage = NULL
    rv$proteinsNotMapped = NULL
    rv$gene = NULL
    rv$stringBasedFiltering_Done = FALSE
    rv$iDat = NULL
    rv$imputePlotsSteps = list(step0 = NULL,
                            step1 = NULL,
                            step2 = NULL)
    rv$tempplot = list(Density = NULL,
                       corrMatrix = NULL,
                       varDist = NULL,
                       mvHisto_HC = NULL,
                       mvHisto_perLines_HC = NULL,
                       histo_missvalues_per_lines_per_conditions = NULL)
    rv$PlotParams = list(legDS = NULL,
                         corrMatrixGradient = defaultGradientRate,
                         legDS_Violinplot = NULL,
                        HeatmapLinkage = NULL,
                      HeatmapDistance = NULL
                      )
    
    rv$AggregProtStats = data.frame(name = c("Number of peptides",
                                              "Number of specific peptides",
                                              "Number of shared peptides", 
                                              "Number of proteins",
                                              "Number of proteins only defined by specific peptides",
                                              "Number of proteins only defined by shared peptides",
                                              "Number of proteins defined both by shared and specific peptides"),
                                     nb = rep(0,7))
    
    
    
    unlink(paste(tempdir(), sessionID, commandLogFile, sep="/"))
    unlink("www/*pdf")
    
}







#-------------------------------------------------------------
rv <- reactiveValues(
    # variable to handle the current object that will be showed
    current.obj = NULL,
    current.obj.name = NULL,
    deleted.mvLines = NULL,
    deleted.stringBased.exprsData = NULL,
    deleted.stringBased.fData = NULL,
    deleted.stringBased = NULL,
    DT_filterSummary = data.frame(Filtre=NULL, 
                                  Prefix=NULL,
                                  nbDeleted=NULL, 
                                  Total=NULL, 
                                  stringsAsFactors=F),
    # variable to keep memory of previous datasets before 
    # transformation of the data
    dataset = list(),
    # Variable that contains the log for the current R session
    text.log = data.frame(Date="", 
                          Dataset="", 
                          History="", 
                          stringsAsFactors=F),
    volcanoTooltip = NULL,
    listLogFC = list(),
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
    ValidFilteringClicked = FALSE,
    ValidImputationClicked = FALSE,
    commandLog = "", 
    normalizationFamily = NULL,
    normalizationMethod = NULL, 
    matAdj = NULL,
    test = NULL, 
    resAnaDiff = list(FC=NULL, P_Value=NULL, condition1 = NULL, condition2 = NULL),
    res_AllPairwiseComparisons = data.frame(),
    wb = NULL,
    progressImputation = 0,
    indexNA = NULL,
    IP_Client= "",
    pourcentageNA = 0,
    nb.empty.lines = 0,
    nbDeleted = 0,
    nbDeletedInfos = NULL,
    fdr = NULL,
    nbSelectedAnaDiff = NULL,
    nbTotalAnaDiff = NULL,
    nbSelectedTotal_Step3 = NULL,
    nbSelected_Step3 = NULL,
    enrichGO_data = NULL,
    groupGO_data = NULL,
    universeData = NULL,
    uniprotID = NULL,
    ProtIDList = NULL,
    GOWarningMessage = NULL,
    proteinsNotMapped = NULL,
    gene = NULL,
    impute_Step = 0,
    ratio=NULL,
    iDat = NULL,
    tempDatasetImputation = NULL,
    MECIndex = NULL,
    stringBasedFiltering_Done = FALSE,
    imputePlotsSteps = list(step0 = NULL,
                            step1 = NULL,
                            step2 = NULL),
    tempplot = list(Density = NULL,
                    corrMatrix = NULL,
                    mvHisto_HC = NULL,
                    mvHisto_perLines_HC = NULL,
                    histo_missvalues_per_lines_per_conditions = NULL),
    PlotParams = list(legDS = NULL,
                      corrMatrixGradient = defaultGradientRate,
                      legDS_Violinplot = NULL,
                      HeatmapLinkage = NULL,
                      HeatmapDistance = NULL
                      
                      ),
    
    AggregProtStats = data.frame(name = c("Number of peptides",
                                              "Number of specific peptides",
                                              "Number of shared peptides", 
                                              "Number of proteins",
                                              "Number of proteins only defined by specific peptides",
                                              "Number of proteins only defined by shared peptides",
                                              "Number of proteins defined both by shared and specific peptides"),
                                     nb = rep(0,7))
    
    )







###-------------------------------------------------------------------
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


###-------------------------------------------------------------------
output$disableAggregationTool <- renderUI({
    rv$current.obj

    if (!is.null(rv$current.obj))
    {
        if (rv$current.obj@experimentData@other$typeOfData == "protein")
        {
    disable(selector = "#navPage li a[data-value=Aggregation]")
    tags$style(
type="text/css","#navPage li a[data-value=Aggregation] { color:lightgrey;}")


        } else {
            enable(selector = "#navPage li a[data-value=Aggregation]")

        }
    }

})


###-------------------------------------------------------------------
output$CurrentDataset <- renderUI({
    txt <- paste("Current dataset :",input$datasets, sep=" ")
    txt
})



###-------------------------------------------------------------------
GetNbNA <- reactive({
    nb <- sum(is.na(Biobase::exprs(rv$current.obj))==TRUE)
    return(nb)
})



###-------------------------------------------------------------------
output$currentObjLoaded <- reactive({
    rv$current.obj
    return(!is.null(rv$current.obj))})
