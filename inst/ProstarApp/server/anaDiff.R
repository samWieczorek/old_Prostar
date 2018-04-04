output$warningNA <- renderUI({
    rv$current.obj
    if (is.null(rv$current.obj)) {return ()}
    
    NA.count <- length(which(is.na(Biobase::exprs((rv$current.obj)))))
    
    if(NA.count    >    0){
        
        text <- "<br> <br> <font color=\"red\">
        Warning ! Your dataset contains empty lines so that the 
        imputation cannot be proceed.
        <br> <br> Please filter your data first."
        HTML(text)
    }
})






output$diffAnalysis_GlobalOptions_SB <- renderUI({
    rv$current.obj
    if (is.null(rv$current.obj)) { return()}
    
    tagList(
        selectInput("anaDiff_Design", "Design", choices=c("None"="None", "One vs One"="OnevsOne", "One vs All"="OnevsAll")),
        selectInput("diffAnaMethod","Choose the statistical test",choices = anaDiffMethod_Choices),
        numericInput("seuilLogFC", "Define log(FC) threshold", min=0, step=0.1, value=0)
        
        #actionButton("anaDiff_Next1_Button", "Next")
    ) })





AnaDiff_GetMaxValueThresholdFilter <- function(){
    input$AnaDiff_ChooseFilters
    vMax <- 0
    
    
    result = tryCatch(
        {
            isolate({
                if (input$AnaDiff_ChooseFilters == gFilterWholeMat) { 
                    vMax <- ncol(Biobase::exprs(rv$current.obj))}
                else if (input$AnaDiff_ChooseFilters == gFilterAllCond 
                         || input$AnaDiff_ChooseFilters == gFilterOneCond){ 
                    ll <- NULL
                    for (i in 1:length(unique(Biobase::pData(rv$current.obj)$Label))){
                        ll <- c(ll, length(which(
                            Biobase::pData(rv$current.obj)$Label==
                                unique(Biobase::pData(rv$current.obj)$Label)[i])))
                    }
                    
                    vMax <- min(ll)
                }
                
                return(vMax)
            })
        }
        , warning = function(w) {
            shinyjs::info(conditionMessage(w))
        }, error = function(e) {
            shinyjs::info(paste(match.call()[[1]],":",
                                conditionMessage(e), 
                                sep=" "))
        }, finally = {
            #cleanup-code 
        })
    
    
    
    
}


output$AnaDiff_seuilNADelete <- renderUI({ 
    input$ChooseFilters
    
    if (is.null(rv$current.obj)) {return(NULL)   }
    if (input$AnaDiff_ChooseFilters==gFilterNone) {return(NULL)   }
    
    choix <- list()
    vMax <- AnaDiff_GetMaxValueThresholdFilter()
    choix[[1]] <- 0
    for (i in 2:(vMax+1)){
        choix[[i]] <- i-1
    }
    #ch <- NULL
    #tag <- rv$current.obj@experimentData@other$mvFilter.threshold
    
    #if (!is.null(tag)) { ch <- tag}
    #else {ch <- choix[[1]]}
    selectInput("AnaDiff_seuilNA", 
                "Keep lines with at least x intensity values", 
                choices = choix)
    
})



observeEvent(input$swapVolcano,{
    rv$resAnaDiff
    if (is.null(input$swapVolcano)){return()}
    if (is.null(rv$resAnaDiff$logFC)){return()}
      
    if(input$swapVolcano) rv$resAnaDiff$logFC <- - (rv$resAnaDiff$logFC)
    else rv$resAnaDiff$logFC <- - (rv$resAnaDiff$logFC)
})


GetCurrentResAnaDiff <- reactive({
    rv$res_AllPairwiseComparisons
    if (is.null(input$selectComparison) || (input$selectComparison== "None")){return()}
    if (is.null(rv$res_AllPairwiseComparisons) ){return()}
    
    index <- grep(paste(input$selectComparison, "_logFC", sep=""), colnames(rv$res_AllPairwiseComparisons$FC))
    
    rv$resAnaDiff <- list(logFC = (rv$res_AllPairwiseComparisons$FC)[,index],
                          P_Value = (rv$res_AllPairwiseComparisons$P_Value)[,index],
                          condition1 = strsplit(input$selectComparison, "_")[[1]][1],
                          condition2 = strsplit(input$selectComparison, "_")[[1]][3]
    )
    
    rv$resAnaDiff
})

observeEvent(input$selectComparison,{
  rv$res_AllPairwiseComparisons
  if (is.null(input$selectComparison)){return()}
  if (is.null(rv$res_AllPairwiseComparisons) ){return()}
  
if (input$selectComparison== "None"){
    rv$resAnaDiff <- NULL
} else {
    GetCurrentResAnaDiff()
}
})



output$diffAnalysis_PairwiseComp_SB <- renderUI({
    rv$current.obj
    rv$res_AllPairwiseComparisons
    if (is.null(rv$current.obj)) { return()}
    if (is.null(rv$res_AllPairwiseComparisons)) { return()}
  
    .choices <- colnames(rv$res_AllPairwiseComparisons$FC)
    .choices <- unlist(strsplit(colnames(rv$res_AllPairwiseComparisons$FC), "_logFC"))
    tagList(
        
        selectInput("selectComparison","Select comparison",
                    choices = c("None",.choices)),
        checkboxInput("swapVolcano", "Swap volcanoplot", value = FALSE),
        h4("Filtering options"),
        radioButtons("AnaDiff_ChooseFilters","", choices = gFiltersList),
        uiOutput("AnaDiff_seuilNADelete"),
        actionButton("AnaDiff_perform.filtering.MV", "Perform previous MV filtering")
    ) })






########################################################
## Perform missing values filtering
########################################################
observeEvent(input$AnaDiff_perform.filtering.MV,{
    
    if (is.null(input$AnaDiff_perform.filtering.MV) ){return()}
    #if (input$AnaDiff_perform.filtering.MV == 0){return()}
    

if (input$AnaDiff_ChooseFilters == gFilterNone){
    GetCurrentResAnaDiff()
} else {
        keepThat <- pvalue_FilterGetIndices(rv$dataset[[input$datasets]],
                                                   input$AnaDiff_ChooseFilters,
                                                   as.integer(input$AnaDiff_seuilNA))
        if (!is.null(keepThat))
            {
            #rv$deleted.mvLines <- rv$dataset[[input$datasets]][-keepThat]
            rv$resAnaDiff$P_Value[-keepThat] <- 1
            rv$resAnaDiff
            #write command log
            # l <- paste(keepThat,",", collapse="")
            # writeToCommandLogFile(
            #     paste("keepThat <- ",
            #         findSequences(keepThat),
            #     sep="")
            # )
            
            
            #if (input$showCommandLog){
            # txt <- paste("keepThat <- mvFilterGetIndices(dataset[['",
            #              input$datasets, 
            #              "']], '",
            #              input$ChooseFilters, "', '",
            #              input$seuilNA, "')","\n",
            #              "deleted.mv <- current.obj[-keepThat]","\n",
            #              "txt <- '",GetFilterText(input$ChooseFilters,input$seuilNA),
            #              "'","\n",
            #              "current.obj <- mvFilterFromIndices(",
            #              "current.obj, keepThat, '",
            #              GetFilterText(input$ChooseFilters,
            #                            input$seuilNA),
            #              "')",              
            #              sep="")
            # 
            # 
            # writeToCommandLogFile(txt)
            # }
            updateSelectInput(session, "AnaDiff_ChooseFilters", selected = input$AnaDiff_ChooseFilters)
            updateSelectInput(session, "AnaDiff_seuilNA", selected = input$AnaDiff_seuilNA)
                    
        }
    }
})








output$diffAnalysis_Calibration_SB <- renderUI({
    rv$current.obj
    if (is.null(rv$current.obj)){ return()}
    
    calibMethod <- "pounds"
    if ("logFC" %in% names(Biobase::fData(rv$current.obj) )){
        calibMethod <- rv$current.obj@experimentData@other$calibrationMethod
        if (is.null(calibMethod)) calibMethod <- "pounds"
    }
    
    tagList(
        selectInput("calibrationMethod", 
                    "Choose the calibration method",
                    choices = calibMethod_Choices,
                    selected = calibMethod),
        uiOutput("numericalValForCalibrationPlot"))
})





output$diffAnalysis_FDR_SB <- renderUI({
    rv$current.obj
    if (is.null(rv$current.obj)) {return ()}
    threshold.PVal <- 0
    if ("logFC" %in% names(Biobase::fData(rv$current.obj) )){
        threshold.PVal <- rv$current.obj@experimentData@other$threshold_p_value
    }
    
    
    tagList(
        numericInput("seuilPVal", 
                     "Define the -log10(p_value) threshold",
                     min = 0,value = threshold.PVal,step=0.1)
    ) })



########################################################

### calcul des comparaisons              ####
#######################################################
observe({
    input$diffAnaMethod
    rv$current.obj
    input$anaDiff_Design
    input$seuilLogFC
    if (is.null(rv$current.obj)){ return()}

    if (is.null(input$diffAnaMethod) || (input$diffAnaMethod=="None")) {return ()}
    if (is.null(rv$current.obj)) {return ()}
    if (is.null(input$anaDiff_Design) || (input$anaDiff_Design=="None")) {return ()}
    if (is.null(input$seuilLogFC)) {return ()}
    if (length(which(is.na(Biobase::exprs(rv$current.obj)))) > 0) {
        return()}

    switch(input$diffAnaMethod,
           Limma={
             rv$res_AllPairwiseComparisons <-wrapper.limmaCompleteTest(rv$current.obj, input$anaDiff_Design)
             },
           Welch={
             #rv$res_AllPairwiseComparisons <- wrapper.diffAnaWelch(rv$current.obj)
           })
    
    rv$listNomsComparaison <- colnames(rv$res_AllPairwiseComparisons)[[1]]
    
})



output$FoldChangePlot <- renderHighchart({
  rv$res_AllPairwiseComparisons
    input$seuilLogFC
   if (is.null(input$seuilLogFC)){ return(NULL)}
    if (is.null(rv$res_AllPairwiseComparisons)){ return(NULL)}
    
    #indice_col_FC <- grep("FC" %in% colnames(rv$res_AllPairwiseComparison))
    data <- rv$res_AllPairwiseComparisons
    hc_FC_DensityPlot(data$FC,input$seuilLogFC)
    
})


#-------------------------------------------------------------
output$showFDR <- renderText({
    rv$current.obj
    input$diffAnaMethod
    rv$seuilPVal
    rv$seuilLogFC
    input$numericValCalibration
    input$calibrationMethod
    rv$resAnaDiff
    
    
    if (is.null(input$diffAnaMethod) || (input$diffAnaMethod == "None")) 
    {return()}
    if (is.null(rv$current.obj)) {return()}
    if (is.null(rv$resAnaDiff$logFC)) {return()}
    if (is.null(rv$seuilLogFC) ||is.na(rv$seuilLogFC)  ) 
    {return()}
    if (is.null(rv$seuilPVal) || is.na(rv$seuilPVal)) { return ()}
    if (length(which(is.na(Biobase::exprs(rv$current.obj)))) > 0) {return()}

    isolate({
        result = tryCatch(
            {
                m <- NULL
                if (input$calibrationMethod == "Benjamini-Hochberg") { m <- 1}
                else if (input$calibrationMethod == "numeric value") {
                    m <- as.numeric(input$numericValCalibration)} 
                else {m <- input$calibrationMethod }
                
                rv$fdr <- diffAnaComputeFDR(rv$resAnaDiff, 
                                            rv$seuilPVal, 
                                            rv$seuilLogFC, 
                                            m)
                if (!is.infinite(rv$fdr)){
                    HTML(paste("<h4>FDR = ", 
                               round(100*rv$fdr, digits=2)," % </h4>", sep=""))
                }
            }
            , warning = function(w) {
                shinyjs::info("Warning ! There is no data selected ! 
                              Please modify the p-value threshold.")
            }, error = function(e) {
                shinyjs::info(
                    paste("Show FDR",":",conditionMessage(e), sep=" ")
                )
            }, finally = {
                #cleanup-code 
            })
        
        
        
    })
})



histPValue <- reactive({
    rv$current.obj
    if (is.null(rv$current.obj)){ return()}
    
    if (is.null(rv$seuilPVal) ||
        is.null(rv$seuilLogFC) ||
        is.null(input$diffAnaMethod)
    ) {return()}
    
    t <- NULL
    # Si on a deja des pVal, alors, ne pas recalculer avec ComputeWithLimma
    if (isContainedIn(c("logFC","P_Value"),names(Biobase::fData(rv$current.obj)) ) ){
        t <- Biobase::fData(rv$current.obj)[,"P_Value"]
    } else{
        data <- RunDiffAna()
        if (is.null(data)) {return ()}
        t <- data$P_Value
    }
    
    
    hist(sort(1-t), breaks=80, col="grey")
    
    
})

output$histPValue <- renderPlot({
    histPValue()
})



output$numericalValForCalibrationPlot <- renderUI({
    input$calibrationMethod
    if (is.null(input$calibrationMethod)) {return()}
    
    if (input$calibrationMethod == "numeric value"){
        numericInput( "numericValCalibration","Proportion of TRUE null hypohtesis", 
                      value = 0, min=0, max=1, step=0.05)
    }
})


output$calibrationResults <- renderUI({
    rv$calibrationRes
    rv$seuilLogFC
    input$diffAnaMethod
    rv$current.obj
    
    if (is.null( rv$calibrationRes)){return()}
    
    txt <- paste("Non-DA protein proportion = ", 
                 round(100*rv$calibrationRes$pi0, digits = 2),"%<br>",
                 "DA protein concentration = ", 
                 round(100*rv$calibrationRes$h1.concentration, digits = 2),
                 "%<br>",
                 "Uniformity underestimation = ", 
                 rv$calibrationRes$unif.under,"<br><br><hr>", sep="")
    HTML(txt)
    
})




calibrationPlot <- reactive({
    rv$seuilPVal
    rv$seuilLogFC
    input$diffAnaMethod
    rv$resAnaDiff
    rv$current.obj
    if (is.null(rv$current.obj) ) {return()}
    
    if (is.null(rv$seuilLogFC) || is.na(rv$seuilLogFC) ||
        (length(rv$resAnaDiff$logFC) == 0)) { return()}
    if (length(which(is.na(Biobase::exprs(rv$current.obj)))) > 0) {
        return()}
    cond <- c(rv$resAnaDiff$condition1, rv$resAnaDiff$condition2)
    # ________
    
    if (is.null(input$calibrationMethod)  ) {return()}
    
    t <- NULL
    method <- NULL
    t <- rv$resAnaDiff$P_Value
    t <- t[which(abs(rv$resAnaDiff$logFC) >= rv$seuilLogFC)]
    
    l <- NULL
    
    
    ll <- NULL
    result = tryCatch(
        {
            
            if ((input$calibrationMethod == "numeric value") 
                && !is.null(input$numericValCalibration)) {
                
                ll <-catchToList(
                    wrapperCalibrationPlot(t, 
                                           as.numeric(input$numericValCalibration)))
                rv$errMsgCalibrationPlot <- ll$warnings[grep( "Warning:", ll$warnings)]
            }
            else if (input$calibrationMethod == "Benjamini-Hochberg") {
                
                ll <-catchToList(wrapperCalibrationPlot(t, 1))
                rv$errMsgCalibrationPlot <- ll$warnings[grep( "Warning:", ll$warnings)]
            }else { 
                ll <-catchToList(wrapperCalibrationPlot(t, input$calibrationMethod))
                rv$errMsgCalibrationPlot <- ll$warnings[grep( "Warning:", ll$warnings)]
            }
            
        }
        , warning = function(w) {
            shinyjs::info(conditionMessage(w))
        }, error = function(e) {
            shinyjs::info(paste("Calibration plot",":",
                                conditionMessage(e), sep=" "))
        }, finally = {
            #cleanup-code 
        })
    
    
})

output$calibrationPlot <- renderPlot({
    
    calibrationPlot()
    
})



output$errMsgCalibrationPlot <- renderUI({
    rv$errMsgCalibrationPlot
    rv$seuilLogFC
    rv$current.obj
    if (is.null(rv$current.obj) ) {return()}
    if (is.null(rv$errMsgCalibrationPlot) ) {return()}
    
    txt <- NULL
    
    for (i in 1:length(rv$errMsgCalibrationPlot)) {
        txt <- paste(txt, "toto",rv$errMsgCalibrationPlot[i], "<br>", sep="")
    }
    
    div(HTML(txt), style="color:red")
    
})


output$errMsgCalibrationPlotAll <- renderUI({
    rv$errMsgCalibrationPlotAll
    rv$seuilLogFC
    rv$current.obj
    if (is.null(rv$current.obj) ) {return()}
    if (is.null(rv$errMsgCalibrationPlotAll) ) {return()}
    
    txt <- NULL
    for (i in 1:length(rv$errMsgCalibrationPlotAll)) {
        txt <- paste(txt, rv$errMsgCalibrationPlotAll[i], "<br>", sep="")
    }
    
    div(HTML(txt), style="color:red")
})



calibrationPlotAll <- reactive({
    rv$seuilPVal
    rv$seuilLogFC
    input$diffAnaMethod
    rv$resAnaDiff
    rv$current.obj
    if (is.null(rv$current.obj) ) {return()}
    
    if ( is.null(rv$seuilLogFC) || is.na(rv$seuilLogFC) ||
        (length(rv$resAnaDiff$logFC) == 0)) { return()}
    if (length(which(is.na(Biobase::exprs(rv$current.obj)))) > 0) {
        return()}
    cond <- c(rv$resAnaDiff$condition1, rv$resAnaDiff$condition2)
    # ________
    
    if (is.null(input$calibrationMethod)  ) {return()}
    
    t <- NULL
    method <- NULL
    t <- rv$resAnaDiff$P_Value
    t <- t[which(abs(rv$resAnaDiff$logFC) >= rv$seuilLogFC)]
    
    l <- NULL
    result = tryCatch(
        {
            l <-catchToList(wrapperCalibrationPlot(t, "ALL")  )
            rv$errMsgCalibrationPlotAll <- l$warnings[grep( "Warning:", 
                                                            l$warnings)]
        }
        , warning = function(w) {
            shinyjs::info(conditionMessage(w))
        }, error = function(e) {
            shinyjs::info(paste("Calibration Plot All methods",":",
                                conditionMessage(e), sep=" "))
        }, finally = {
            #cleanup-code 
        })
    
})



#--------------------------------------------------
output$calibrationPlotAll <- renderPlot({
    calibrationPlotAll()
})


#----------------------------------------------
observeEvent(input$ValidDiffAna,{ 
    rv$current.obj
    rv$resAnaDiff
    rv$res_AllPairwiseComparisons
    
    if (is.null(rv$current.obj)){ return()}
    if (is.null(rv$res_AllPairwiseComparisons) ){return()}
    
    
    
    if ((input$ValidDiffAna == 0) ||  is.null(input$ValidDiffAna) ) {
        return()}
    if (length(which(is.na(Biobase::exprs(rv$current.obj)))) > 0) {
        return()}

     temp <-   rv$current.obj         
    if (!rv$current.obj@experimentData@other$RawPValues ){
            temp <- diffAnaSaveRAW_Data(temp,rv$res_AllPairwiseComparisons)
                }
                
    if (!is.null(rv$resAnaDiff)  ){  
                m <- NULL
                if (input$calibrationMethod == "Benjamini-Hochberg") 
                { m <- 1}
                else if (input$calibrationMethod == "numeric value") 
                {m <- as.numeric(input$numericValCalibration)}
                else {m <- input$calibrationMethod }
                
                rv$fdr <- diffAnaComputeFDR(rv$resAnaDiff, rv$seuilPVal, rv$seuilLogFC, m)
                
                
                temp <- DAPAR::diffAnaSave(temp,
                                           rv$resAnaDiff,
                                           input$diffAnaMethod,
                                           rv$seuilPVal, 
                                           rv$seuilLogFC, 
                                           rv$fdr,
                                           input$calibrationMethod)
                
                
                name <- paste("DiffAnalysis.", input$diffAnaMethod, " - ", 
                              rv$typeOfDataset, 
                              sep="")
                
                rv$dataset[[name]] <- temp
                rv$current.obj <- temp
                
                
                updateSelectInput(session, "datasets", 
                                  paste("Dataset versions of",
                                        rv$current.obj.name, sep=" "),
                                  choices = names(rv$dataset),
                                  selected = name)
                
                
                
                ####write command Log file
                #if (input$showCommandLog){
                writeToCommandLogFile(paste("cond1 <- '", rv$resAnaDiff$condition1, "'", sep=""))
                writeToCommandLogFile(paste("cond2 <- '", rv$resAnaDiff$condition2, "'", sep=""))
                writeToCommandLogFile(paste("method <- '", input$diffAnaMethod, "'", sep=""))
                
                switch(input$diffAnaMethod,
                       Limma = writeToCommandLogFile("data <- wrapper.diffAnaLimma(current.obj, cond1, cond2)"),
                       Welch =  writeToCommandLogFile( "data <- wrapper.diffAnaWelch(current.obj, cond1, cond2)")
                )
                
                
                writeToCommandLogFile(paste("threshold_pValue <- ", input$seuilPVal, sep=""))
                writeToCommandLogFile(paste("threshold_logFC <- ", input$seuilLogFC,sep=""))
                
                writeToCommandLogFile(paste("calibMethod <- \"", input$calibrationMethod, "\"", sep=""))
                if (input$calibrationMethod == "Benjamini-Hochberg") { 
                    writeToCommandLogFile("m <- 1") }
                else if (input$calibrationMethod == "numeric value") 
                { writeToCommandLogFile(paste(" m <- ",as.numeric(input$numericValCalibration), sep=""))}
                else {writeToCommandLogFile("m <- calibMethod")}
                
                writeToCommandLogFile("fdr <- diffAnaComputeFDR(data, threshold_pValue, threshold_logFC, m)")
                
                
                writeToCommandLogFile(paste(" temp <- diffAnaSave(dataset[['",
                                            input$datasets,"']],  data, method, cond1, cond2, threshold_pValue, threshold_logFC, fdr, calibMethod)", sep=""))
                writeToCommandLogFile(paste(" name <- \"DiffAnalysis.", 
                                            input$diffAnaMethod, " - ", rv$typeOfDataset,"\"", sep="" ))
                writeToCommandLogFile("dataset[[name]] <- temp")
                writeToCommandLogFile("current.obj <- temp")
                # }
                
                
                cMethod <- NULL
                if (input$calibrationMethod == "numeric value"){
                    cMethod <- paste("The proportion of true null
                                     hypotheses was set to", 
                                     input$numericValCalibration, sep= " ")}
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
                              "corresponding to a FDR = ", round(100*rv$fdr, digits=2),
                              sep=" ")
                UpdateLog(text,name)
                
                updateTabsetPanel(session, "abc", selected = "ValidateAndSaveAnaDiff")
                
                
                
                
                ## Add the necessary text to the Rmd file
                # txt2Rmd <- readLines("Rmd_sources/anaDiff_Rmd.Rmd")
                # filename <- paste(tempdir(), sessionID, 'report.Rmd',sep="/")
                # write(txt2Rmd, file = filename,append = TRUE, sep = "\n")
                #createPNG_DifferentialAnalysis()
  
            }
})


output$DiffAnalysisSaved <- renderUI({
    input$datasets
    rv$current.obj
    if (is.null(input$datasets) || (length(grep("DiffAnalysis.",input$datasets)) !=1) ) {
        return()  }
    else if (grep("DiffAnalysis.",input$datasets) == 1 ) {
        h4("The differential analysis has been saved.")
    }
})






output$equivPVal <- renderText ({
    input$seuilPVal
    input$diffAnaMethod
    rv$current.obj
    if (is.null(rv$current.obj)){return()}
    if (is.null(input$seuilPVal)){return()}
    if (is.null(input$diffAnaMethod) || (input$diffAnaMethod == G_noneStr))
    {return(NULL)}
     if (length(which(is.na(Biobase::exprs(rv$current.obj)))) > 0) {
        return()}
    
    HTML(paste("<h4>(p-value = ",
               signif(10^(- (input$seuilPVal)), digits=3), ") </h4>", sep=""))
})


output$equivLog10 <- renderText ({
    input$test.threshold
    rv$current.obj
    input$diffAnaMethod
    if (is.null(input$diffAnaMethod)){return()}
    if (is.null(rv$current.obj)){return()}
    if (is.null(input$test.threshold)){return()}
    if (length(which(is.na(Biobase::exprs(rv$current.obj)))) > 0) {
        return()}
    
    HTML(paste("<h4>-log10 (p-value) = ",
               signif(- log10(input$test.threshold/100), digits=1),
               "</h4>", sep=""))
})


##update diffAna Panel
observeEvent(rv$current.obj,{
    
    if (is.null(rv$current.obj)){return()}
    if (length(which(is.na(Biobase::exprs(rv$current.obj)))) > 0) {
        return()}
    
    if ("P_Value"  %in% names(Biobase::fData(rv$current.obj))){
        
        updateSelectInput(session,"diffAnaMethod",
                          selected =  rv$current.obj@experimentData@other$method)
        
        updateNumericInput(session,
                           "seuilPVal",
                           min = 0,
                           max = max(-log10(Biobase::fData(rv$current.obj)$P_Value)),
                           value = rv$current.obj@experimentData@other$threshold_p_value, 
                           step=0.1)
        
        updateNumericInput(session,
                           "seuilLogFC", 
                           min = 0, 
                           max = max(abs(Biobase::fData(rv$current.obj)$logFC)), 
                           value = rv$current.obj@experimentData@other$threshold_logFC, 
                           step=0.1)
    }
    
})

observeEvent(input$seuilPVal,{
    if (!is.null(input$seuilPVal)){rv$seuilPVal <- as.numeric(input$seuilPVal)}
    
})

observeEvent(input$seuilLogFC,{
    if (!is.null(input$seuilLogFC)){rv$seuilLogFC <- as.numeric(input$seuilLogFC)}
    
})


output$nbSelectedItems <- renderUI({
    rv$seuilLogFC
    input$diffAnaMethod
    rv$current.obj
    rv$resAnaDiff
    
    
    if (is.null(rv$resAnaDiff$logFC) || is.null(rv$current.obj)){
        return()}
    
    if (is.null( input$diffAnaMethod) || (input$diffAnaMethod == G_noneStr)){
        return()}
    if (length(which(is.na(Biobase::exprs(rv$current.obj)))) > 0) {
        return()}
    
    
    result = tryCatch(
        {
            p <- NULL
            p <- rv$resAnaDiff
            upItemsPVal <- NULL
            upItemsLogFC <- NULL
            
            
            upItemsLogFC <- which(abs(p$logFC) >= rv$seuilLogFC)
            rv$nbTotalAnaDiff_Step1 <- nrow(Biobase::exprs(rv$current.obj))
            rv$nbSelectedAnaDiff_Step1 <- NULL
            t <- NULL
            
            t <- upItemsLogFC
            rv$nbSelectedAnaDiff_Step1 <- length(t)
            
            txt <- paste("Total number of ",rv$typeOfDataset, "(s) = ", 
                         rv$nbTotalAnaDiff_Step1,"<br>",
                         "Number of selected ",rv$typeOfDataset, "(s) = ", 
                         rv$nbSelectedAnaDiff_Step1,"<br>",
                         "Number of non selected ",rv$typeOfDataset, "(s) = ", 
                         (rv$nbTotalAnaDiff_Step1 -rv$nbSelectedAnaDiff_Step1), sep="")
            HTML(txt)
        }
        , warning = function(w) {
            shinyjs::info(conditionMessage(w))
        }, error = function(e) {
            shinyjs::info(paste(match.call()[[1]],":",
                                conditionMessage(e), 
                                sep=" "))
        }, finally = {
            #cleanup-code 
        })
    
    
    
    
})

output$nbSelectedItemsStep3 <- renderUI({
    rv$seuilPVal
    rv$seuilLogFC
    input$diffAnaMethod
    rv$current.obj
    
    if (is.null( input$diffAnaMethod) || (input$diffAnaMethod == "None")
        || is.null(rv$current.obj)){
        return()}
    if (length(which(is.na(Biobase::exprs(rv$current.obj)))) > 0) {
        return()}
    
    
    
    result = tryCatch(
        {
            
            p <- NULL
            if ("P_Value"  %in% names(fData(rv$current.obj))){
                p$P_Value <- fData(rv$current.obj)$P_Value
                p$logFC <- fData(rv$current.obj)$logFC
            }else {
                
                p <- rv$resAnaDiff
            }
            
            if (is.null(p)) {return ()}
            upItemsPVal <- NULL
            upItemsLogFC <- NULL
            
            
            upItemsPVal <- which(-log10(p$P_Value) >= rv$seuilPVal)
            upItemsLogFC <- which(abs(p$logFC) >= rv$seuilLogFC)
            
            
            rv$nbSelectedTotal_Step3 <- nrow(Biobase::exprs(rv$current.obj))
            rv$nbSelected_Step3 <- NULL
            t <- NULL
            
            if (!is.null(rv$seuilPVal) && !is.null(rv$seuilLogFC) ) {
                t <- intersect(upItemsPVal, upItemsLogFC)}
            else if (!is.null(rv$seuilPVal) && is.null(rv$seuilLogFC) ) {
                t <- upItemsPVal}
            else if (is.null(rv$seuilPVal) && !is.null(rv$seuilLogFC) ) {
                t <- upItemsLogFC}
            
            rv$nbSelected_Step3 <- length(t)
            
            txt <- paste("Total number of ", rv$typeOfDataset, " = ", 
                         rv$nbSelectedTotal_Step3,"<br>",
                         "Number of selected ", rv$typeOfDataset, " = ", 
                         rv$nbSelected_Step3,"<br>",
                         "Number of non selected ", rv$typeOfDataset, " = ", 
                         (rv$nbSelectedTotal_Step3-rv$nbSelected_Step3), sep="")
            HTML(txt)
        }
        , warning = function(w) {
            shinyjs::info(conditionMessage(w))
        }, error = function(e) {
            shinyjs::info(paste(match.call()[[1]],":",
                                conditionMessage(e), 
                                sep=" "))
        }, finally = {
            #cleanup-code 
        })
    
    
})





observeEvent(rv$current.obj,{
    
    if (is.null(rv$current.obj)){return()}
    if (length(which(is.na(Biobase::exprs(rv$current.obj)))) > 0) {
        return()}
    
    isolate({
        
        result = tryCatch(
            {
                #Si on a deja des pVal, alors, ne pas recalculer 
                if ("logFC" %in% names(Biobase::fData(rv$current.obj) )){
                    updateNumericInput(session, 
                                       "seuilLogFC",
                                       value= rv$current.obj@experimentData@other$threshold_logFC)
                    updateNumericInput(session, 
                                       "seuilPVal",
                                       value= rv$current.obj@experimentData@other$threshold_p_value)
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
            }
            , warning = function(w) {
                shinyjs::info(conditionMessage(w))
            }, error = function(e) {
                shinyjs::info(conditionMessage(e))
            }, finally = {
                #cleanup-code 
            })
        
        
        
        
    })
    
})







output$selectTooltipInfo <- renderUI({
    rv$current.obj
    if (is.null(rv$current.obj)){return()}
    
    #selectInput("tooltipInfo", "Select the info you want to see", choices = colnames(fData(rv$current.obj)))
    selectizeInput("tooltipInfo",
                   label = "Select the info you want to see",
                   choices = colnames(fData(rv$current.obj)),
                   multiple = TRUE, width='500px')
})


######################
getDataInfosVolcano <- reactive({
    input$eventPointClicked
    rv$current.obj
    if (is.null(rv$current.obj)){ return()}
    
    test.table <- data.frame(lapply(
        Biobase::exprs(rv$current.obj)[(input$eventPointClicked+1),], 
        function(x) t(data.frame(x))))
    rownames(test.table) <- rownames(rv$current.obj)[input$eventPointClicked +1]
    test.table <- round(test.table, digits=3)
    test.table
})


getDataInfosVolcano_Step3 <- reactive({
    rv$current$obj
    input$eventPointClicked
    #if (is.null(input$eventPointClicked)){ return()}
    if (is.null(rv$current.obj)){ return()}
    
    test.table <- data.frame(
        lapply(
            Biobase::exprs(rv$current.obj)[input$eventPointClicked+1,],
            function(x) t(data.frame(x))))
    #rownames(test.table) <- input$eventPointClicked
    rownames(test.table) <- rownames(rv$current.obj)[input$eventPointClicked +1]
    test.table <- round(test.table, digits=3)
    test.table
    
    
})


#################


output$infosVolcanoTable <- DT::renderDataTable({
    rv$current.obj
    input$eventPointClicked
    
    if (is.null(input$eventPointClicked)){return()}
    if (is.null(rv$current.obj)){return()}
    
    data <- fData(rv$current.obj)[, rv$current.obj@experimentData@other$OriginOfValues]
    if (!is.null(data)){
        data <- as.matrix(data)[input$eventPointClicked+1,]
    }
    
    id <-  which(data=="NA")
    #print(input$eventPointClicked)
    #print(data)
    if (length(id) == 0){
        dat <- DT::datatable(getDataInfosVolcano(), 
                             options=list(dom='t',ordering=F))
    } else {
        dat <- DT::datatable(getDataInfosVolcano(), 
                             options=list(dom='t',
                                          ordering=F
                                          ,drawCallback=JS(
                                              paste("function(row, data) {",
                                                    paste(sapply(1:ncol(getDataInfosVolcano()),function(i)
                                                        paste( "$(this.api().cell(",
                                                               id %% nrow(getDataInfosVolcano()),",",
                                                               id / nrow(getDataInfosVolcano()),
                                                               ").node()).css({'background-color': 'lightblue'});")
                                                    ),collapse = "\n"),"}" ))
                                          ,server = TRUE))
    }
    dat
    
})



volcanoplot_rCharts <- reactive({
    rv$seuilPVal
    rv$seuilLogFC
    input$diffAnaMethod
    rv$resAnaDiff
    rv$current.obj
    input$tooltipInfo
    
    if (is.null(rv$seuilLogFC) || is.na(rv$seuilLogFC) ){return()}
    if ((length(rv$resAnaDiff$logFC) == 0)  ){return()}
    if (is.null(rv$current.obj) ){return()}
    #if (is.null(input$tooltipInfo)) {        return()}
    
    if (length(which(is.na(Biobase::exprs(rv$current.obj)))) > 0) {
        return()}
    
    result = tryCatch(
        {
            
            if ("logFC" %in% names(fData(rv$current.obj) )){
                
                df <- data_frame(x=fData(rv$current.obj)$logFC, 
                                 y = -log10(fData(rv$current.obj)$P_Value),
                                 index = as.character(rownames(rv$current.obj)))
                if (!is.null(input$tooltipInfo)){
                    df <- cbind(df,fData(rv$current.obj)[input$tooltipInfo])
                }
                #rownames(df) <- rownames(rv$current.obj)
                colnames(df) <- gsub(".", "_", colnames(df), fixed=TRUE)
                names(rv$current.obj@experimentData@other) <- gsub(".", "_", names(rv$current.obj@experimentData@other), fixed=TRUE)
                
                if (ncol(df) > 3){
                    colnames(df)[4:ncol(df)] <- 
                        paste("tooltip_", colnames(df)[4:ncol(df)], sep="")
                }
                hc_clickFunction <- 
                    JS("function(event) {Shiny.onInputChange('eventPointClicked', [this.index]);}")
                #             print("avant 5")
                cond <- c(rv$current.obj@experimentData@other$condition1,
                          rv$current.obj@experimentData@other$condition2)
                diffAnaVolcanoplot_rCharts2(df,
                                            threshold_logFC = rv$current.obj@experimentData@other$threshold_logFC,
                                            conditions = cond,
                                            clickFunction=hc_clickFunction) 
            } else {
                df <- data_frame(x=rv$resAnaDiff$logFC, 
                                 y = -log10(rv$resAnaDiff$P_Value),
                                 index = 1:nrow(fData(rv$current.obj)))
                if (!is.null(input$tooltipInfo)){
                    df <- cbind(df,fData(rv$current.obj)[input$tooltipInfo])
                }
                #rownames(df) <- rownames(rv$current.obj)
                colnames(df) <- gsub(".", "_", colnames(df), fixed=TRUE)
                if (ncol(df) > 3){
                    colnames(df)[4:ncol(df)] <- 
                        paste("tooltip_", colnames(df)[4:ncol(df)], sep="")
                }
                hc_clickFunction <- 
                    JS("function(event) {Shiny.onInputChange('eventPointClicked', [this.index]);}")
                #             print("avant 5")
                cond <- c(rv$resAnaDiff$condition1, rv$resAnaDiff$condition2)
                diffAnaVolcanoplot_rCharts2(df,
                                            threshold_logFC = rv$seuilLogFC,
                                            conditions = cond,
                                            clickFunction=hc_clickFunction)
                
                
            }
        }
        , warning = function(w) {
            shinyjs::info(conditionMessage(w))
        }, error = function(e) {
            shinyjs::info(paste("titi",match.call()[[1]],":",
                                conditionMessage(e),
                                sep=" "))
        }, finally = {
            #cleanup-code
        })
    
})

output$volcanoplot_rCharts <- renderHighchart({
    volcanoplot_rCharts()
    
})   


volcanoplot <- reactive({
    rv$seuilPVal
    rv$seuilLogFC
    input$diffAnaMethod
    rv$resAnaDiff
    rv$current.obj
    
    if (is.null(rv$seuilLogFC) || is.na(rv$seuilLogFC) ||
        (length(rv$resAnaDiff$logFC) == 0) || is.null(rv$current.obj)) { 
        return()}
    
    if (length(which(is.na(Biobase::exprs(rv$current.obj)))) > 0) {
        return()}
    
    cond <- c(rv$resAnaDiff$condition1, rv$resAnaDiff$condition2)
    result = tryCatch(
        {
            diffAnaVolcanoplot(logFC = rv$resAnaDiff$logFC, 
                               pVal = rv$resAnaDiff$P_Value, 
                               threshold_logFC = rv$seuilLogFC,
                               conditions = cond)
        }
        , warning = function(w) {
            shinyjs::info(conditionMessage(w))
        }, error = function(e) {
            shinyjs::info(paste(match.call()[[1]],":",
                                conditionMessage(e), 
                                sep=" "))
        }, finally = {
            #cleanup-code 
        })
    
    
})

output$volcanoplot <- renderPlot({
    volcanoplot()
})   


volcanoplot_rCharts_Step3 <- reactive({
    rv$current.obj
    rv$seuilPVal
    rv$seuilLogFC
    input$diffAnaMethod
    rv$resAnaDiff
    input$seuilPVal
    
    if ( is.null(input$seuilPVal) ||
        is.null(rv$seuilLogFC) || is.na(rv$seuilLogFC) ||
        is.null(rv$seuilPVal) || is.na(rv$seuilPVal) ||
        (length(rv$resAnaDiff$logFC) == 0) ||  is.null(rv$current.obj)) { 
        return()}
    if (length(which(is.na(Biobase::exprs(rv$current.obj)))) > 0) {
        return()} 
    cond <- c(rv$resAnaDiff$condition1, rv$resAnaDiff$condition2)
    result = tryCatch(
        {
            
            if ("logFC" %in% names(fData(rv$current.obj) )){
                df <- data_frame(x=fData(rv$current.obj)$logFC, 
                                 y = -log10(fData(rv$current.obj)$P_Value),
                                 index = as.character(rownames(rv$current.obj)))
                
                if (!is.null(input$tooltipInfo)){
                    df <- cbind(df,fData(rv$current.obj)[input$tooltipInfo])
                }
                colnames(df) <- gsub(".", "_", colnames(df), fixed=TRUE)
                if (ncol(df) > 3){
                    colnames(df)[4:ncol(df)] <- paste("tooltip_", 
                                                      colnames(df)[4:ncol(df)], 
                                                      sep="")
                }
                hc_clickFunction <- JS(
                    "function(event) {Shiny.onInputChange('eventPointClicked', [this.index]);}"
                )
                
                diffAnaVolcanoplot_rCharts2(df,
                                            threshold_logFC = rv$current.obj@experimentData@other$threshold_logFC,
                                            threshold_pVal = rv$current.obj@experimentData@other$threshold_p_value,
                                            conditions = c(rv$current.obj@experimentData@other$condition1,
                                                           rv$current.obj@experimentData@other$condition2),
                                            clickFunction=hc_clickFunction)
                
            }else{
                cond <- c(rv$resAnaDiff$condition1, rv$resAnaDiff$condition2)
                
                df <- data_frame(x=rv$resAnaDiff$logFC, 
                                 y = -log10(rv$resAnaDiff$P_Value),
                                 index = as.character(rownames(rv$current.obj)))
                if (!is.null(input$tooltipInfo)){
                    df <- cbind(df,fData(rv$current.obj)[input$tooltipInfo])
                }
                colnames(df) <- gsub(".", "_", colnames(df), fixed=TRUE)
                if (ncol(df) > 3){
                    colnames(df)[4:ncol(df)] <- paste("tooltip_", 
                                                      colnames(df)[4:ncol(df)], 
                                                      sep="")
                }
                hc_clickFunction <- JS(
                    "function(event) {Shiny.onInputChange('eventPointClicked', [this.index]);}"
                )
                
                diffAnaVolcanoplot_rCharts2(df,
                                            threshold_logFC = rv$seuilLogFC,
                                            threshold_pVal = rv$seuilPVal,
                                            conditions = cond,
                                            clickFunction=hc_clickFunction)
            }
        }
        , warning = function(w) {
            shinyjs::info(conditionMessage(w))
        }, error = function(e) {
            shinyjs::info(paste(match.call()[[1]],":",
                                conditionMessage(e), 
                                sep=" "))
        }, finally = {
            #cleanup-code 
        })
    
})


output$volcanoplot_rCharts_Step3 <- renderHighchart({
    volcanoplot_rCharts_Step3()
})   






output$infosVolcanoTableStep3 <- renderDataTable({
    rv$current.obj
    input$eventPointClicked
    
    if (is.null(input$eventPointClicked)){data.frame()}
    if (is.null(rv$current.obj)){ data.frame()}
    
    data <- 
        as.matrix(fData(rv$current.obj)[,rv$current.obj@experimentData@other$OriginOfValues])[input$eventPointClicked+1,]
    id <-  which(data=="NA")
    if (length(id) == 0){
        dat <- DT::datatable(getDataInfosVolcano_Step3(), 
                             options=list(dom='t',ordering=F))
    } else {
        dat <- DT::datatable(getDataInfosVolcano_Step3(), 
                             options=list(dom='t',
                                          ordering=F
                                          ,drawCallback=JS(
                                              paste("function(row, data) {",
                                                    paste(sapply(1:ncol(getDataInfosVolcano_Step3()),
                                                                 function(i)
                                                                     paste( "$(this.api().cell(",
                                                                            id %% nrow(getDataInfosVolcano_Step3()),",",
                                                                            id / nrow(getDataInfosVolcano_Step3()),
                                                                            ").node()).css({'background-color': 'lightblue'});")
                                                    ),collapse = "\n"),"}" ))
                                          ,server = TRUE))
    }
    dat
    
} )



volcanoplotStep3 <- reactive({
    rv$current.obj
    rv$seuilPVal
    rv$seuilLogFC
    input$diffAnaMethod
    rv$resAnaDiff
    
    if (is.null(rv$seuilLogFC) || is.na(rv$seuilLogFC) ||
        is.null(rv$seuilPVal) || is.na(rv$seuilPVal) ||
        (length(rv$resAnaDiff$logFC) == 0) ||  is.null(rv$current.obj)) { 
        return()}
    if (length(which(is.na(Biobase::exprs(rv$current.obj)))) > 0) {
        return()} 
    cond <- c(rv$resAnaDiff$condition1, rv$resAnaDiff$condition2)
    result = tryCatch(
        {
            if ("logFC" %in% names(fData(rv$current.obj) )){
                diffAnaVolcanoplot(fData(rv$current.obj)$logFC,
                                   fData(rv$current.obj)$P_Value, 
                                   rv$current.obj@experimentData@other$threshold_p_value,
                                   rv$current.obj@experimentData@other$threshold_logFC,
                                   c(rv$current.obj@experimentData@other$condition1,
                                     rv$current.obj@experimentData@other$condition2)
                )
            }else{
                cond <- c(rv$resAnaDiff$condition1, rv$resAnaDiff$condition2)
                
                diffAnaVolcanoplot(rv$resAnaDiff$logFC, 
                                   rv$resAnaDiff$P_Value, 
                                   rv$seuilPVal, 
                                   rv$seuilLogFC,
                                   cond)
            }
        }
        , warning = function(w) {
            shinyjs::info(conditionMessage(w))
        }, error = function(e) {
            shinyjs::info(paste(match.call()[[1]],":",
                                conditionMessage(e), 
                                sep=" "))
        }, finally = {
            #cleanup-code 
        })
    
})

output$volcanoplotStep3 <- renderPlot({
    volcanoplotStep3()
})





########################################################
output$showSelectedItems <- DT::renderDataTable({
    rv$current.obj
    input$diffAnaMethod
    input$seuilLogFC
    input$seuilPVal
    print("tutu")
    
    
    if ( is.null(rv$current.obj) ||
         is.null(input$seuilLogFC)    ||
         is.null(input$seuilPVal)
    ) {return()}
    
    if (is.null(input$diffAnaMethod) || (input$diffAnaMethod == "None")) 
    {return()}
    
    if (length(which(is.na(Biobase::exprs(rv$current.obj)))) > 0) {
        return()}
  #  result = tryCatch(
  #      {
            
            # isolate({
            t <- NULL
            # Si on a deja des pVal, alors, ne pas recalculer avec ComputeWithLimma
            if (isContainedIn(c("logFC","P_Value"),
                              names(Biobase::fData(rv$current.obj)) ) ){
                selectedItems <- 
                    (which(Biobase::fData(rv$current.obj)$Significant == TRUE)) 
                t <- data.frame(id =  
                                    rownames(Biobase::exprs(rv$current.obj))[selectedItems],
                                Biobase::fData(rv$current.obj)[selectedItems,
                                                               c("logFC", "P_Value", "Significant")])
            } else{
                data <- rv$resAnaDiff
                upItems1 <- which(-log10(data$P_Value) >= rv$seuilPVal)
                upItems2 <- which(abs(data$logFC) >= rv$seuilLogFC)
                selectedItems <- intersect(upItems1, upItems2)
                t <- data.frame(id =  
                                    rownames(Biobase::exprs(rv$current.obj))[selectedItems],
                                logFC = data$logFC,
                                P_Value = data$P_Value)
            }
            t
  #      }
   #     , warning = function(w) {
   #         shinyjs::info(conditionMessage(w))
   #     }, error = function(e) {
    #        shinyjs::info(paste("showSelectedItems",match.call()[[1]],":",
   #                             conditionMessage(e), 
   #                             sep=" "))
   #     }, finally = {
    #        #cleanup-code 
   #     })
    
})





# ---- Download of only significat data --------------
output$linkWelch <- renderUI({
    input$ExportWelchTest
    if (input$ExportWelchTest == 0) {return() }
    
    saveMSnset(input$filenameWelchData,
               gFileExtension$msnset,
               rv$current.obj[
                   which(Biobase::fData(rv$current.obj)$Significant.Welch == TRUE)])
    filename <- paste(input$filenameWelchData, gFileExtension$msnset, sep="")
    
    completeFilename <- paste(rv$dirnameforlink,filename, sep="/")
    a(filename, href=completeFilename)
    
})

# ---- Download of only significat data --------------
output$linkLimma <- renderUI({
    input$ExportdiffAnaLimma
    if (input$ExportdiffAnaLimma == 0) {return() }
    
    saveMSnset(input$filenameLimmaData, gFileExtension$msnset, 
               rv$current.obj[
                   which(Biobase::fData(rv$current.obj)$Significant.limma == TRUE)])
    filename <- paste(input$filenameLimmaData, gFileExtension$msnset, sep="")
    completeFilename <- paste(rv$dirnameforlink,filename, sep="/")
    a(filename, href=completeFilename)
    
})




isContainedIn <- function(strA, strB){
    return (all(strA %in% strB))
}



observeEvent(rv$current.obj,{
    
    if (is.null(rv$current.obj)) {return()}
    
    NA.count <- length(which(is.na(Biobase::exprs(rv$current.obj))))
    
    if (NA.count       >       0) {
        shinyjs::disable(input$diffAnaMethod)
        shinyjs::disable(input$seuilLogFC)
    } else {
        shinyjs::enable("diffAnaMethod")
    }
})
