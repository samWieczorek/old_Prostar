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






output$diffAnalysis_sidebarPanelTab1 <- renderUI({
    rv$current.obj
    if (is.null(rv$current.obj)) { return()}
    
    method <- "Limma"
    threshold_logFC <- 0
    if ("logFC" %in% names(Biobase::fData(rv$current.obj) )){
        method <- rv$current.obj@experimentData@other$method
        threshold_logFC <- rv$current.obj@experimentData@other$threshold_logFC
    }
    
    tagList(
        uiOutput("RenderLimmaCond1"),
        uiOutput("RenderLimmaCond2"),
        selectInput("diffAnaMethod","Choose the statistical test",
                                choices = anaDiffMethod_Choices,
                                selected = anaDiffMethod_Choices[1]),

        numericInput("seuilLogFC", "Define log(FC) threshold",
                                  min = 0,value = threshold_logFC, step=0.1),
        HTML("This corresponds to the ratio: <br>Condition 2 / Condition 1.")
    ) })







output$diffAnalysis_sidebarPanelTab2 <- renderUI({
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

output$diffAnalysis_sidebarPanelTab3 <- renderUI({
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
observe({
    input$diffAnaMethod
    rv$current.obj
    input$condition1
    input$condition2
    if (is.null(rv$current.obj)){ return()}
    
    if (is.null(input$diffAnaMethod)) {return ()}
    if (is.null(rv$current.obj)) {return ()}
    if (is.null(input$condition1)) {return ()}
    if (is.null(input$condition2)) {return ()}
    if (input$condition1 == input$condition2) {return ()}
    if (length(which(is.na(Biobase::exprs(rv$current.obj)))) > 0) {
        return()}
    
    data <- NULL
    
    isolate({
        result = tryCatch(
            {
                
                switch(input$diffAnaMethod,
                        Limma={
                            rv$resAnaDiff <- wrapper.diffAnaLimma(rv$current.obj, 
                                                          input$condition1, 
                                                          input$condition2)},
                        Welch={
                            rv$resAnaDiff <- wrapper.diffAnaWelch(rv$current.obj, 
                                                          input$condition1, 
                                                          input$condition2)
                })
                
                
                
                colnames(rv$resAnaDiff) <- gsub(".", "_", colnames(rv$resAnaDiff), fixed=TRUE)
                
            }
            #, warning = function(w) {
            #    shinyjs::info(conditionMessage(w))
            #}
            , error = function(e) {
                shinyjs::info(conditionMessage(e))
            }, finally = {
                #cleanup-code
            }
            
        )
        
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
    rv$resAnaDiff
    
    
    if (is.null(input$diffAnaMethod) || (input$diffAnaMethod == "None")) 
    {return()}
    if (is.null(rv$current.obj)) {return()}
    if (is.null(rv$resAnaDiff$logFC)) {return()}
    if (is.null(input$condition1) || is.null(input$condition2) ) 
    {return()}
    if (is.null(rv$seuilLogFC) ||is.na(rv$seuilLogFC)  ) 
    {return()}
    if (is.null(rv$seuilPVal) || is.na(rv$seuilPVal)) { return ()}
    if (length(which(is.na(Biobase::exprs(rv$current.obj)))) > 0) {return()}
    
    if ((input$condition1 == input$condition2)) {return()}
    
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
                                         rv$seuilLogFC, m)
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
    if (input$condition1 == input$condition2) {return()}
    
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
    input$condition1
    input$condition2
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
    input$condition1
    input$condition2
    input$diffAnaMethod
    rv$resAnaDiff
    rv$current.obj
    if (is.null(rv$current.obj) ) {return()}
    
    if (is.null(input$condition1) || is.null(input$condition2) ||
        is.null(rv$seuilLogFC) || is.na(rv$seuilLogFC) ||
        (input$condition1 == input$condition2) ||
        (length(rv$resAnaDiff$logFC) == 0)) { return()}
    if (length(which(is.na(Biobase::exprs(rv$current.obj)))) > 0) {
        return()}
    cond <- c(input$condition1, input$condition2)
    # ________
    
    if (is.null(input$calibrationMethod)  ) {return()}
    #if (input$condition1 == input$condition2) {return(NULL)}
    
    
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
    input$condition1
    input$condition2
    input$diffAnaMethod
    rv$resAnaDiff
    rv$current.obj
    if (is.null(rv$current.obj) ) {return()}
    
    if (is.null(input$condition1) || is.null(input$condition2) ||
        is.null(rv$seuilLogFC) || is.na(rv$seuilLogFC) ||
        (input$condition1 == input$condition2) ||
        (length(rv$resAnaDiff$logFC) == 0)) { return()}
    if (length(which(is.na(Biobase::exprs(rv$current.obj)))) > 0) {
        return()}
    cond <- c(input$condition1, input$condition2)
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
    if (is.null(rv$current.obj)){ return()}
    
    
    if ((input$ValidDiffAna == 0) ||  is.null(input$ValidDiffAna) ) {
        return()}
    if (input$condition1 == input$condition2) {return()}
    if (length(which(is.na(Biobase::exprs(rv$current.obj)))) > 0) {
        return()}
    isolate({
        
        result = tryCatch(
            {
                data <- rv$resAnaDiff
                
                if (is.null(data)) {return (NULL)}
                m <- NULL
                if (input$calibrationMethod == "Benjamini-Hochberg") 
                { m <- 1}
                else if (input$calibrationMethod == "numeric value") 
                {m <- as.numeric(input$numericValCalibration)}
                else {m <- input$calibrationMethod }
                
                rv$fdr <- DAPAR::diffAnaComputeFDR(data, rv$seuilPVal, rv$seuilLogFC, m)
                
                
                temp <- DAPAR::diffAnaSave(rv$dataset[[input$datasets]],
                                    data,
                                    input$diffAnaMethod,
                                    input$condition1,
                                    input$condition2,
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
                    writeToCommandLogFile(paste("cond1 <- '", input$condition1, "'", sep=""))
                    writeToCommandLogFile(paste("cond2 <- '", input$condition2, "'", sep=""))
                    writeToCommandLogFile(paste("method <- '", input$diffAnaMethod, "'", sep=""))
                    if (input$diffAnaMethod == "Limma"){
                        writeToCommandLogFile("data <- wrapper.diffAnaLimma(current.obj, cond1, cond2)")
                    } else if (input$diffAnaMethod == "Welch"){
                    writeToCommandLogFile( "data <- wrapper.diffAnaWelch(current.obj, cond1, cond2)")
                    }
                
                
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
            #, warning = function(w) {
            #    shinyjs::info(conditionMessage(w))
            #}
            , error = function(e) {
                shinyjs::info(paste("Valid Diff Ana",":",
                                    conditionMessage(e), sep=" "))
            }, finally = {
                #cleanup-code 
            })
        
        
        
    }) 
    
})


output$DiffAnalysisSaved <- renderUI({
    input$datasets
    rv$current.obj
    if (is.null(input$datasets) 
        || (length(grep("DiffAnalysis.",input$datasets)) !=1) ) {
        return()  }
    else if (grep("DiffAnalysis.",input$datasets) == 1 ) {
        h4("The differential analysis has been saved.")
    }
})





output$RenderLimmaCond1 <- renderUI({
    rv$current.obj
    if (is.null(rv$current.obj) ) {return()  }
    
    
    labels <- unique(Biobase::pData(rv$current.obj)[,"Label"])
    labels <- setNames(as.list(labels),labels)
    condition1 <- labels[[2]]
    if ("logFC" %in% names(Biobase::fData(rv$current.obj) )){
        condition1 <- rv$current.obj@experimentData@other$condition1
    }
    
    radioButtons("condition1", label = h4("Choose condition 1"), 
                 choices = labels, 
                 selected = condition1, 
                 inline=F)
    
})



output$RenderLimmaCond2 <- renderUI({
    rv$current.obj
    if (is.null(rv$current.obj) ) {return()  }
    
    isolate({
        labels <- unique(Biobase::pData(rv$current.obj)[,"Label"])
        labels <- setNames(as.list(labels),labels)
        condition2 <- labels[[1]]
        if ("logFC" %in% names(Biobase::fData(rv$current.obj) )){
            condition2 <- rv$current.obj@experimentData@other$condition2
        }
        
        
        radioButtons("condition2", label = h4("Choose condition 2"), 
                     choices = labels , 
                     selected = condition2,
                     inline=F)
    })
})






output$equivPVal <- renderText ({
    input$seuilPVal
    input$diffAnaMethod
    rv$current.obj
    if (is.null(rv$current.obj)){return()}
    if (is.null(input$condition1) || is.null(input$condition2))
    {return()}
    if (is.null(input$seuilPVal)){return()}
    if (is.null(input$diffAnaMethod) || (input$diffAnaMethod == G_noneStr))
    {return(NULL)}
    if ((input$condition1 == input$condition2)) {return()}
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
    if (is.null(input$condition1) || is.null(input$condition2)){return()}
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
    input$condition1
    input$condition2
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
    input$condition1
    input$condition2
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



output$infosVolcanoTable <- DT::renderDataTable({
    rv$current.obj
    input$eventPointClicked
    
    if (is.null(input$eventPointClicked)){return()}
    if (is.null(rv$current.obj)){return()}
    
    data <- as.matrix(rv$current.obj@experimentData@other$isMissingValues)[input$eventPointClicked,]
    id <-  which(data==1)
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
    input$condition1
    input$condition2
    input$diffAnaMethod
    rv$resAnaDiff
    rv$current.obj
    input$tooltipInfo
    
    if (is.null(input$condition1) || is.null(input$condition2) ){return()}
    if (is.null(rv$seuilLogFC) || is.na(rv$seuilLogFC) ){return()}
    if ((input$condition1 == input$condition2)  ){return()}
    if ((length(rv$resAnaDiff$logFC) == 0)  ){return()}
    if (is.null(rv$current.obj) ){return()}
    #if (is.null(input$tooltipInfo)) {        return()}
    
    if (length(which(is.na(Biobase::exprs(rv$current.obj)))) > 0) {
        return()}
    
    result = tryCatch(
        {
            
            if ("logFC" %in% names(fData(rv$current.obj) )){
                
                df <- data.frame(x=fData(rv$current.obj)$logFC, 
                                 y = -log10(fData(rv$current.obj)$P_Value),
                                 index = as.character(rownames(rv$current.obj)),
                                 stringsAsFactors = FALSE)
                if (!is.null(input$tooltipInfo)){
                    df <- cbind(df,fData(rv$current.obj)[input$tooltipInfo])
                }
                rownames(df) <- rownames(rv$current.obj)
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
                diffAnaVolcanoplot_rCharts(df,
                                           threshold_logFC = rv$current.obj@experimentData@other$threshold_logFC,
                                           conditions = cond,
                                           clickFunction=hc_clickFunction) 
            } else {
                df <- data.frame(x=rv$resAnaDiff$logFC, 
                                 y = -log10(rv$resAnaDiff$P_Value),
                                 index = 1:nrow(fData(rv$current.obj)),
                                 stringsAsFactors = FALSE)
                if (!is.null(input$tooltipInfo)){
                    df <- cbind(df,fData(rv$current.obj)[input$tooltipInfo])
                }
                rownames(df) <- rownames(rv$current.obj)
                colnames(df) <- gsub(".", "_", colnames(df), fixed=TRUE)
                if (ncol(df) > 3){
                    colnames(df)[4:ncol(df)] <- 
                        paste("tooltip_", colnames(df)[4:ncol(df)], sep="")
                }
                hc_clickFunction <- 
                    JS("function(event) {Shiny.onInputChange('eventPointClicked', [this.index]);}")
                #             print("avant 5")
                cond <- c(input$condition1, input$condition2)
                diffAnaVolcanoplot_rCharts(df,
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
    input$condition1
    input$condition2
    input$diffAnaMethod
    rv$resAnaDiff
    rv$current.obj
    
    if (is.null(input$condition1) || is.null(input$condition2) ||
        is.null(rv$seuilLogFC) || is.na(rv$seuilLogFC) ||
        (input$condition1 == input$condition2) ||
        (length(rv$resAnaDiff$logFC) == 0) || is.null(rv$current.obj)) { 
        return()}
    
    if (length(which(is.na(Biobase::exprs(rv$current.obj)))) > 0) {
        return()}
    
    cond <- c(input$condition1, input$condition2)
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
    input$condition1
    input$condition2
    input$diffAnaMethod
    rv$resAnaDiff
    input$seuilPVal
    
    if (is.null(input$condition1) ||  is.null(input$seuilPVal) ||is.null(input$condition2) ||
        is.null(rv$seuilLogFC) || is.na(rv$seuilLogFC) ||
        is.null(rv$seuilPVal) || is.na(rv$seuilPVal) ||
        (input$condition1 == input$condition2) ||
        (length(rv$resAnaDiff$logFC) == 0) ||  is.null(rv$current.obj)) { 
        return()}
    if (length(which(is.na(Biobase::exprs(rv$current.obj)))) > 0) {
        return()} 
    cond <- c(input$condition1, input$condition2)
    result = tryCatch(
        {
            
            if ("logFC" %in% names(fData(rv$current.obj) )){
                df <- data.frame(x=fData(rv$current.obj)$logFC, 
                                 y = -log10(fData(rv$current.obj)$P_Value),
                                 index = as.character(rownames(rv$current.obj)),
                                 stringsAsFactors = FALSE)
                
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
                
                diffAnaVolcanoplot_rCharts(df,
                                           threshold_logFC = rv$current.obj@experimentData@other$threshold_logFC,
                                           threshold_pVal = rv$current.obj@experimentData@other$threshold_p_value,
                                           conditions = c(rv$current.obj@experimentData@other$condition1,
                                                          rv$current.obj@experimentData@other$condition2),
                                           clickFunction=hc_clickFunction)
                
            }else{
                cond <- c(input$condition1, input$condition2)
                
                df <- data.frame(x=rv$resAnaDiff$logFC, 
                                 y = -log10(rv$resAnaDiff$P_Value),
                                 index = as.character(rownames(rv$current.obj)),
                                 stringsAsFactors = FALSE)
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
                
                diffAnaVolcanoplot_rCharts(df,
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



getDataInfosVolcano_Step3 <- reactive({
    rv$current$obj
    input$eventPointClicked
    if (is.null(input$eventPointClicked)){ return()}
    if (is.null(rv$current.obj)){ return()}
    
    test.table <- data.frame(
        lapply(
    Biobase::exprs(rv$current.obj)[as.character(input$eventPointClicked),],
                                    function(x) t(data.frame(x))))
    rownames(test.table) <- input$eventPointClicked
    test.table <- round(test.table, digits=3)
    test.table
    
    
})


output$infosVolcanoTableStep3 <- renderDataTable({
    rv$current.obj
    input$eventPointClicked
    
    if (is.null(input$eventPointClicked)){data.frame()}
    if (is.null(rv$current.obj)){ data.frame()}
    
    data <- 
as.matrix(rv$current.obj@experimentData@other$isMissingValues)[input$eventPointClicked,]
    id <-  which(data==1)
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
    input$condition1
    input$condition2
    input$diffAnaMethod
    rv$resAnaDiff
    
    if (is.null(input$condition1) || is.null(input$condition2) ||
        is.null(rv$seuilLogFC) || is.na(rv$seuilLogFC) ||
        is.null(rv$seuilPVal) || is.na(rv$seuilPVal) ||
        (input$condition1 == input$condition2) ||
        (length(rv$resAnaDiff$logFC) == 0) ||  is.null(rv$current.obj)) { 
        return()}
    if (length(which(is.na(Biobase::exprs(rv$current.obj)))) > 0) {
        return()} 
    cond <- c(input$condition1, input$condition2)
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
                cond <- c(input$condition1, input$condition2)
                
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
    
    if ( is.null(rv$current.obj) ||
         is.null(input$seuilLogFC)    ||
         is.null(input$seuilPVal)
    ) {return()}
    
    if (is.null(input$diffAnaMethod) || (input$diffAnaMethod == "None")) 
    {return()}
    
    if (length(which(is.na(Biobase::exprs(rv$current.obj)))) > 0) {
        return()}
    result = tryCatch(
        {
            
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
                                data[selectedItems,])
            }
            t
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



ConditionTabPanel <- reactive({
    rv$conditions
    rv$current$obj
    if (is.null(rv$current.obj)){return()}
    
    tabPanel(title="ConditionsSetup",
             value = "tabConditionsSetup",
             h3("Select conditions to perform the differential analysis"),
             helpText("Please choose the labels for condition to analyse"),
             if (GetNbNA() > 0){
                 h3("There are some NA in your data. Please impute before.")
             }
             else{
                 h3("Conditions setup")
                 helpText("Please choose the labels for condition to analyse")
             }
    )
})




isContainedIn <- function(strA, strB){
    return (all(strA %in% strB))
}



observeEvent(rv$current.obj,{
    
    if (is.null(rv$current.obj)) {return()}

    NA.count <- length(which(is.na(Biobase::exprs(rv$current.obj))))

    if (NA.count       >       0) {
        shinyjs::disable(input$condition1)
        #shinyjs::disable("condition2")
        shinyjs::disable(input$diffAnaMethod)
        shinyjs::disable(input$seuilLogFC)
    } else {
        shinyjs::enable("diffAnaMethod")
        #shinyjs::enable("seuilLogFC")
        #shinyjs::enable("condition1")
        #shinyjs::enable("condition2")
    }
})



