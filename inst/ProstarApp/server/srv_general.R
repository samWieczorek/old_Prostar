
# activatePopover <- function(){
#     txt_histo_M <- paste0("<p>Test",
#                           "test</p><p>Explanation .</p>")
#     
#     txt_histo_MV_per_lines <- paste0("<p>Test",
#                                      "test</p><p>Explanation .</p>")
#     
#     
#     txt_histo_MV_per_lines_per_conditions <- paste0("<p>Test",
#                                                     "test</p><p>Explanation .</p>")
#     
#     
#     addPopover(session, "MVPlots_DS-histo_MV", "Info", 
#                content = txt_histo_M, trigger = 'click')
#     
#     addPopover(session, "MVPlots_DS-histo_MV_per_lines", "Info", 
#                content = txt_histo_MV_per_lines, trigger = 'click')
#     
#     addPopover(session, "MVPlots_DS-histo_MV_per_lines_per_conditions", "Info", 
#                content = txt_histo_MV_per_lines_per_conditions, trigger = 'click')
#     
#     
#     addPopover(session, "MVPlots_filtering-histo_MV", "Info", 
#                content = txt_histo_M, trigger = 'click')
#     
#     addPopover(session, "MVPlots_filtering-histo_MV_per_lines", "Info", 
#                content = txt_histo_MV_per_lines, trigger = 'click')
#     
#     addPopover(session, "MVPlots_filtering-histo_MV_per_lines_per_conditions", "Info", 
#                content = txt_histo_MV_per_lines_per_conditions, trigger = 'click')
#     
#     
# }


# 
# callModule(modulePopover,"modulePopover_toto", data = reactive(paste0(data(), 
#                                                                       a("tidy data paper", 
#                                                                         href = "http://vita.had.co.nz/papers/tidy-data.pdf",
#                                                                         target="_blank"))))










getDataForExprs <- reactive({
  req(input$settings_nDigits)
  rv$current$obj
  
  
  test.table <- as.data.frame(round(Biobase::exprs(rv$current.obj),digits=input$settings_nDigits))
  test.table <- cbind(test.table, Biobase::fData(rv$current.obj)[,rv$current.obj@experimentData@other$OriginOfValues])
  test.table
  
})


getData <- reactive({
  req(input$settings_nDigits)
  rv$current$obj
  
  test.table <- round(Biobase::exprs(rv$current.obj),digits=input$settings_nDigits)
  test.table
})



data <- eventReactive(rv$current$obj, {
  input$settings_nDigits
  rv$current$obj
  
  test.table <- round(Biobase::exprs(rv$current.obj),digits=input$settings_nDigits)
  test.table
}, ignoreNULL = FALSE)










callModule(modulePopover,"modulePopover_dataset", 
           data = reactive(list(title = p(if(is.null(rv$current.obj.name)) "No dataset" else paste0(rv$current.obj.name)),

                                content="Before each processing step, a backup of the current dataset is stored. It is possible to reload one of them at any time.")))


observeEvent(input$navbar,{
    if (input$navbar=="stop")
        stopApp()
})

getDatasetName <- reactive({
    req(rv$current.obj.name)
    rv$current.obj.name
})


##' Get back to a previous object ---------------------------------------
##' @author Samuel Wieczorek
observeEvent( input$datasets,ignoreInit = TRUE,{ 

  print("observeEvent( input$datasets,ignoreInit = TRUE,")
    isolate({
        if (!is.null(input$datasets)) {
            rv$current.obj <- rv$dataset[[input$datasets]]

        if (!is.null( rv$current.obj))
            rv$typeOfDataset <- rv$current.obj@experimentData@other$typeOfData
        }

    })
    print(rv$current.obj)
})





output$datasetAbsPanel <- renderUI({
    req(rv$current.obj.name)
     div(
         div(
             style="display:inline-block; vertical-align: middle;"
             ,modulePopoverUI("modulePopover_dataset")
             ),
         div(
             style="display:inline-block; vertical-align: middle;",
             selectInput("datasets", "", choices = list("None"="None"),width = '200px')
         )
     )
})

###-------------------------------------------------------------------
onStop(function() cat("Session stopped\n"))


session$onSessionEnded(function() {
    #setwd(tempdir())
    graphics.off()
    unlink(sessionID, recursive = TRUE)
    unlink(paste(tempdir(), sessionID, commandLogFile, sep="/"),recursive = TRUE)
    unlink(paste(tempdir(), sessionID, sep="/"),recursive = TRUE)
    unlink(paste(tempdir(), "*html", sep="/"))
    unlink(paste(tempdir(), "*log", sep="/"))
    unlink("www/*pdf")
 
    
    #rm(list= list(myListOfThings))
    stopApp()
})


###-------------------------------------------------------------------
ClearUI <- reactive({
        
        updateSelectInput(session, 
                          "datasets",  
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
        
    })




loadObjectInMemoryFromConverter_2 <- function(obj){
  rv$typeOfDataset <- obj@experimentData@other$typeOfData
  if (is.null(rv$typeOfDataset)) {rv$typeOfDataset <- ""}
  
  #If there are already pVal values, then do no compute them 
  if (G_logFC_Column %in% names(Biobase::fData(obj) )){
    rv$resAnaDiff <- list(logFC = Biobase::fData(obj)$FC,
                          P_Value = Biobase::fData(obj)$P_Value)
    rv$seuilLogFC <- obj@experimentData@other$threshold_logFC
    rv$seuilPVal  <- obj@experimentData@other$threshold_p_value
    
  }
  
  
  name <- paste ("Original", " - ", rv$typeOfDataset, sep="")
  rv$dataset[[name]] <- obj
  ClearNavbarPage()
  BuildNavbarPage()
  
  
  # txt <- paste("dataset <- list()","\n", "dataset[['", name,"']] <- current.obj","\n","typeOfDataset <- \"",  
  #              rv$typeOfDataset, "\"", "\n",
  #              "colnames(fData(current.obj)) <- gsub(\".\", \"_\", colnames(fData(current.obj)), fixed=TRUE)",
  #              sep="")
  # writeToCommandLogFile(txt)
  # 
  updateSelectInput(session, "datasets", 
                    #label = paste("Dataset versions of", rv$current.obj.name, sep=" "),
                    choices = names(rv$dataset),
                    selected = name)
  
}

#

######################################
loadObjectInMemoryFromConverter <- reactive({
    req(rv$current.obj)
  rv$typeOfDataset <- rv$current.obj@experimentData@other$typeOfData
    if (is.null(rv$typeOfDataset)) {rv$typeOfDataset <- ""}
    
    #If there are already pVal values, then do no compute them 
    if (G_logFC_Column %in% names(Biobase::fData(rv$current.obj) )){
        rv$resAnaDiff <- list(logFC = Biobase::fData(rv$current.obj)$logFC,
                              P_Value = Biobase::fData(rv$current.obj)$P_Value)
        rv$seuilLogFC <- rv$current.obj@experimentData@other$threshold_logFC
        rv$seuilPVal  <- rv$current.obj@experimentData@other$threshold_p_value
        
    }
    
    
    name <- paste ("Original", " - ", rv$typeOfDataset, sep="")
    rv$dataset[[name]] <- rv$current.obj
    
    ClearNavbarPage()
    BuildNavbarPage()
    
    updateSelectInput(session, "datasets", 
                      #label = paste("Dataset versions of", rv$current.obj.name, sep=" "),
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








###-------------------------------------------------------------------
ClearMemory <- function(){
  #rv$UI_TabsList = NULL
  #rv$UI_fileSourced = NULL
  #rv$SRV_fileSourced = NULL
  
  rv$current.comp = NULL
  
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
    rv$listLogFC = list()
    rv$seuilLogFC = 0
    rv$seuilPVal = 1e-60
    rv$tab1 = NULL
    rv$dirname = ""
    rv$dirnameforlink = ""
    rv$conditions = list(cond1 = NULL, cond2 = NULL)
    rv$temp.aggregate = NULL
   rv$calibrationRes = NULL
    rv$errMsgcalibrationPlot = NULL
    rv$errMsgcalibrationPlotALL = NULL
    rv$typeOfDataset = ""
    rv$commandLog =  "" 
    rv$normalizationFamily = NULL
    rv$normalizationMethod = NULL 
    rv$matAdj = NULL
    rv$resAnaDiff = list(logFC=NULL, P_Value=NULL, condition1 = NULL, condition2 = NULL)
    rv$res_AllPairwiseComparisons = data.frame()
    rv$indexNA = NULL
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
    rv$GO = list(ProtIDList=NULL,
              gene=NULL,
              proteinsNotMapped=NULL,
              ratio=NULL,
              uniprotID=NULL,
              universeData=NULL,
              enrichGO_data=NULL,
              groupGO_data=NULL)

    rv$impute_Step = 0
    
    
    
    rv$hot = NULL
    rv$newOrder = NULL
    rv$designChecked = NULL
    rv$designSaved = FALSE
    rv$conditionsChecked = NULL
    
    
    rv$updateDesign_designSaved=FALSE
    rv$updateDesign_designChecked=NULL
    rv$updateDesign_hot=NULL
    rv$updateDesign_newOrder=NULL
    rv$updateDesign_conditionsChecked=NULL
    
    rv$designIsValid = FALSE
    rv$MECIndex = NULL
    rv$tempDatasetImputation = NULL
    rv$text.log <- data.frame(Date="", 
                              Dataset="", 
                              History="", 
                              stringsAsFactors=F)
    rv$GOWarningMessage = NULL
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
    rv$indProgressDemomode = 0
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
  UI_TabsList = NULL,
  UI_fileSourced = NULL,
  SRV_fileSourced = NULL,
  # variable to handle the current object that will be showed
    current.comp = NULL,
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
    listLogFC = list(),
    seuilLogFC = 0,
    seuilPVal = 1e-60,
    tab1 = NULL,
    dirname = "",
    dirnameforlink = "",
    conditions = list(cond1 = NULL, cond2 = NULL),
    temp.aggregate = NULL,
    
    
    
    # design = list(designChecked=NULL,
    #                  hot=NULL,
    #                  newOrder=NULL,
    #                  conditionsChecked=NULL,
    #                  designSaved=FALSE),
    
    hot = NULL,
    newOrder = NULL,
    designChecked = NULL,
    designSaved = FALSE,
    conditionsChecked = NULL,
    
    
    
    # updateDesign = list(designChecked=NULL,
    #                        hot=NULL,
    #                        newOrder=NULL,
    #                        conditionsChecked=NULL,
    #                     designSaved=FALSE),
    
    updateDesign_designChecked=NULL,
    updateDesign_hot=NULL,
    updateDesign_newOrder=NULL,
    updateDesign_conditionsChecked=NULL,
    updateDesign_designSaved=FALSE,
    
    
    calibrationRes = NULL,
    errMsgcalibrationPlot = NULL,
    errMsgcalibrationPlotALL = NULL,
    typeOfDataset = "",
    ValidFilteringClicked = FALSE,
    ValidImputationClicked = FALSE,
    commandLog = "", 
    normalizationFamily = NULL,
    normalizationMethod = NULL, 
    matAdj = NULL,
    resAnaDiff = list(logFC=NULL, P_Value=NULL, condition1 = NULL, condition2 = NULL),
    res_AllPairwiseComparisons = data.frame(),
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
    GO = list(ProtIDList=NULL,
              gene=NULL,
              proteinsNotMapped=NULL,
              ratio=NULL,
              uniprotID=NULL,
              universeData=NULL,
              enrichGO_data=NULL,
              groupGO_data=NULL),

    GOWarningMessage = NULL,

    impute_Step = 0,

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
    indProgressDemomode = 0,
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
# output$currentObjLoaded <- reactive({
#     #rv$current.obj
#     return(!is.null(isolate({rv$current.obj})))})


retroCompatibility <- reactive({
  req(rv$current.obj)
  if ("FC" %in% colnames(Biobase::fData(rv$current.obj))){
    idx <- which(colnames(Biobase::fData(rv$current.obj)) == "FC")
    names(Biobase::fData(rv$current.obj))[idx] <-"logFC"
  }
  
  if ("Experiment" %in% colnames(Biobase::pData(rv$current.obj))){
    idx <- which(colnames(Biobase::pData(rv$current.obj)) == "Experiment")
    names(Biobase::pData(rv$current.obj))[idx] <-"Sample.name"
  }
  
  if ("Label" %in% colnames(Biobase::pData(rv$current.obj))){
    idx <- which(colnames(Biobase::pData(rv$current.obj)) == "Label")
    names(Biobase::pData(rv$current.obj))[idx] <-"Condition"
  }
})











########################################################
# Update the global variable log
UpdateLog <- function(name, l.params){
  
  hist <- buildLogText(name, l.params, level=rv$typeOfDataset)
  rv$text.log <- rbind(rv$text.log,
                       c(Date=date(), Dataset=name, History=ifelse(is.null(hist), "",hist)))
  
}



NeedsUpdate <- reactive({
  req(rv$current.obj)
  
  PROSTAR.version <- rv$current.obj@experimentData@other$Prostar_Version
  
  if (!is.null(PROSTAR.version) && (compareVersion(PROSTAR.version,"1.12.9") != -1)
      && (DAPAR::check.design(Biobase::pData(rv$current.obj))$valid))
  {return (FALSE)}
  
  else {
    return(TRUE)
  }
})
















#-------------------------------------------------------------------
output$aboutText <- renderUI({
  busyIndicator(WaitMsgCalc,wait = 0)
  
  t <- sessionInfo()
  daparVersion <- installed.packages(lib.loc=DAPAR.loc)["DAPAR","Version"]
  ProstarVersion <- installed.packages(lib.loc=Prostar.loc)["Prostar","Version"]
  
  
  text <- paste("<strong>Maintaining ProStaR as free software is a heavy and time-consuming
                duty. If you use it, please cite the following reference</strong><br> 
                S. Wieczorek, F. Combes, C. Lazar, Q. Giai-Gianetto, 
                L. Gatto, A. Dorffer, A.-M. Hesse, Y. Coute, M. Ferro, 
                C. Bruley and T. Burger. <br>
                <u>\"DAPAR & ProStaR: software to perform statistical 
                analyses in quantitative discovery 
                proteomics\"</u><br>
                <i>Bioinformatics 33(1), 135-136</i>, <strong>2017</strong><br>
                <a href=\"http://doi.org/10.1093/bioinformatics/btw580\"
                title=\"here\" target=\"_blank\">http://doi.org/10.1093/bioinformatics/btw580</a>
                
                <br><hr>
                <strong>DAPAR</strong> and <strong>ProStaR</strong> form a 
                software suite for quantitative analysis of mass spectrometry 
                based proteomics.<br>
                More specifically it is designed to process 
                relative quantitative data from discovery experiments.<br>
                It is composed of two distinct R packages : <br>", 
                "<ul style=\"list-style-type:disc;\">
                <li>
                <a href=\"http://www.bioconductor.org/packages/release/bioc/html/Prostar.html\"
                title=\"here\" target=\"_blank\">Prostar</a> (version ",
                ProstarVersion, "): the web based graphical user interface to DAPAR 
                </li>
                <li>
                <a href=\"http://www.bioconductor.org/packages/release/bioc/html/DAPAR.html\"
                title=\"here\" target=\"_blank\">DAPAR</a> (version ",daparVersion,"): a 
                collection of tools and graphs dedicated to proteomic analysis
                </li>
                </ul> 
                DAPAR includes wrappers to numerous other R packages, either available on 
                <a href=\"the https://cran.r-project.org/\" title=\"here\" target=\"_blank\">
                CRAN</a> or on the <a href=\"http://www.bioconductor.org\"
                title=\"here\" target=\"_blank\">Bioconductor</a>.
                <br>
                Here is a brief overview of the available functionalities:
                <ul style=\"list-style-type:disc;\">
                <li>  
                <strong>Descriptive statistics</strong> are available, for exploration and visualization of the 
                quantitative dataset;
                </li>
                <li>  
                <strong>Filtering</strong> options allows pruning the protein or peptide list according to 
                various criteria (missing values, contaminants, reverse sequences);
                </li>
                
                <li>
                <strong>Cross replicate normalization</strong>, so as to make the quantitative 
                values comparable between the different analyzed samples;
                </li>
                
                <li>  
                <strong>Missing values imputation</strong> with different methods, depending 
                on the nature of  the missing values;
                </li>
                <li>  
                <strong>Aggregation</strong> from peptide to protein intensity values;
                </li>
                
                <li>
                <strong>Differential analysis</strong>, which includes null hypothesis 
                significance testing as well as multiple testing correction 
                (for false discovery rate estimation).
                </li>
                <li>
                <strong>Gene Ontology (GO) analysis</strong> allows is to map protein list onto GO terms and to test category enrichment.
                </li>
                </ul>
                
                <br>
                For more details, please refer to the \"Help\" tab.", sep="")
  
  HTML(text)
  
})



# 
# observeEvent(rv$file2Source,{
#   filename <- rv$file2Source
#   type <- unlist(strsplit(filename, "_"))[1]
#   switch(type,
#          ui = {
#            if (!(filename %in% rv$UI_fileSourced)){
#              source(file.path("ui", filename), local = TRUE)$value
#              rv$UI_fileSourced <- c(rv$UI_fileSourced, filename)
#            }
#          }, 
#          srv = {
#            if (!(filename %in% rv$SRV_fileSourced)){
#              source(file.path("server", filename), local = TRUE)$value
#              rv$SRV_fileSourced <- c(rv$SRV_fileSourced, filename)
#            }
#          }
#   )
#   
# })
# 

