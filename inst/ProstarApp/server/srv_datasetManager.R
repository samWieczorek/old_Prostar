



output$chooseDataset <- renderUI({
    
    if(require("DAPARdata")){
        print("DAPARdata is loaded correctly")
        selectInput("demoDataset",
                    "Choose a demo dataset",
                    choices = utils::data(package="DAPARdata")$results[,"Item"]
        )
    } else {
        print("trying to install DAPARdata")
        install.packages("DAPARdata")
        if(require(DAPARdata)){
        print("DAPARdata installed and loaded")
        selectInput("demoDataset",
                    "Choose a demo dataset",
                    choices = utils::data(package='DAPARdata')$results[,"Item"])
        } else {
            stop("Could not install the package DAPARdata")
        }
    }
    
    
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
        rhandsontable(DT, fillHandle = list(direction='vertical', autoInsertRow=FALSE,
                                            maxRows=nrow(DT))) %>%
        hot_cols(colWidths = c(200, 100, 100, 100, 100) ) %>%
        hot_rows(rowHeights = 30) %>%
        hot_context_menu(allowRowEdit = TRUE, allowColEdit = FALSE,
                         allowInsertRow = FALSE,
                         allowInsertColumn = FALSE,
                         allowRemoveRow = TRUE,
                         allowRemoveColumn = FALSE,
                         autoInsertRow=FALSE     ) %>%
        hot_col(col = "Experiment", readOnly = TRUE)
        
})



output$warningNonUniqueID <- renderUI({
    input$idBox
    rv$tab1
    if (is.null(rv$tab1)) {return(NULL)  }
    if (is.null(input$idBox) || (input$idBox =="")) {return(NULL)  }
    
    t <- (length(as.data.frame(rv$tab1)[, input$idBox])
          == length(unique(as.data.frame(rv$tab1)[, input$idBox])))
    
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


output$eData <- renderUI({
    input$file1
    rv$tab1
    if (is.null(rv$tab1)) {return(NULL)  }
    
    choices <- colnames(rv$tab1)
    names(choices) <- colnames(rv$tab1)
    selectizeInput("eData.box",
                   label = "",
                   choices = choices,
                   multiple = TRUE, width='500px')
    
})




output$chooseOriginOfValues <- renderUI({
    input$eData.box
    input$file1
    rv$tab1
    if (is.null(rv$tab1)) {return(NULL)  }
    
    choices <- colnames(rv$tab1)
    names(choices) <- colnames(rv$tab1)
    
    tagList(
        if (length(input$eData.box) >= 1) {
            lapply(1:length(input$eData.box), function(entry) {
            selectInput(paste0("colForOriginValue_", entry), input$eData.box[entry], choices = choices,
                    multiple = FALSE, width='500px')
            }
        )
        }
  )  
})

output$helpTextDataID <- renderUI({
    input$typeOfData
    if (is.null(input$typeOfData)){return(NULL)}
    t <- ""
    switch(input$typeOfData,
           protein = {t <- "proteins"},
           peptide = {t <- "peptides"}
    )
    txt <- paste ("Please select among the columns of your data the one that 
                  corresponds to a unique ID of the ", t, ".", sep=" ")
    helpText(txt)
    
})





output$chooseExportFilename <- renderUI({
    
    textInput("nameExport", 
              label = "Enter the name of the files to be created",
              value = rv$current.obj.name)
})




observeEvent(input$loadDemoDataset,{
    input$showCommandLog
    if (is.null(input$demoDataset)){return (NULL)}
  
    
    result = tryCatch(
        {
            ClearMemory()
            ClearUI()
            utils::data(list = input$demoDataset)
            rv$current.obj <- get(input$demoDataset)
            rv$current.obj.name <- input$demoDataset
            rv$typeOfDataset <- rv$current.obj@experimentData@other$typeOfData
            rv$indexNA <- which(is.na(rv$current.obj))
            colnames(fData(rv$current.obj)) <- gsub(".", "_", colnames(fData(rv$current.obj)), fixed=TRUE)
            names(rv$current.obj@experimentData@other) <- gsub(".", "_", names(rv$current.obj@experimentData@other), fixed=TRUE)
            #colnames(exprs(rv$current.obj)) <- gsub(".", "_", colnames(exprs(rv$current.obj)), fixed=TRUE)
            #colnames(pData(rv$current.obj)) <- gsub(".", "_", colnames(pData(rv$current.obj)), fixed=TRUE)
            
            if (is.null(rv$current.obj@experimentData@other$RawPValues ))
                rv$current.obj@experimentData@other$RawPValues <- FALSE
            
            rv$current.obj <- addOriginOfValue(rv$current.obj)
            l.params <- list(filename = input$demoDataset)
            UpdateLog("Original",l.params)

            #if (input$showCommandLog){
                writeToCommandLogFile("library(DAPARdata)")
            writeToCommandLogFile(paste("utils::data(",
                                        input$demoDataset,")", 
                                        sep=""))
            writeToCommandLogFile(paste("current.obj <- ",
                                        input$demoDataset, 
                                        sep=""))
            #}
            
            loadObjectInMemoryFromConverter()
        }
        , warning = function(w) {
            shinyjs::info(paste("load Demo dataset",conditionMessage(w), sep=""))
        }, error = function(e) {
            shinyjs::info(paste("load Demo dataset",match.call()[[1]],":",
                                conditionMessage(e), 
                                sep=" "))
        }, finally = {
            #cleanup-code 
        })

    
})


########################################################
# Update the global variable log
UpdateLog <- function(name, l.params){
  rv$typeOfDataset
  print(rv$typeOfDataset)
    
  
  hist <- buildLogText(name, l.params, level=rv$typeOfDataset)
  rv$text.log <- rbind(rv$text.log,
                       c(Date=date(), Dataset=name, History=ifelse(is.null(hist), "",hist)))
  
  
}



##-- Open a MSnset File --------------------------------------------
observeEvent(input$file,ignoreInit =TRUE,{ 

    exts <- c("MSnset","MSnSet")
    if( is.na(match(GetExtension(input$file$name), exts))) {
        shinyjs::info("Warning : this file is not a MSnset file ! 
                      Please choose another one.")
    }
    else {
        ClearMemory()
        ClearUI()
        rv$current.obj <- readRDS(input$file$datapath)
        rv$current.obj.name <- DeleteFileExtension(input$file$name)
        rv$typeOfDataset <- rv$current.obj@experimentData@other$typeOfData
        rv$indexNA <- which(is.na(exprs(rv$current.obj)))
        
        colnames(fData(rv$current.obj)) <- gsub(".", "_", colnames(fData(rv$current.obj)), fixed=TRUE)
        names(rv$current.obj@experimentData@other) <- gsub(".", "_", names(rv$current.obj@experimentData@other), fixed=TRUE)
        
        if (is.null(rv$current.obj@experimentData@other$RawPValues ))
            rv$current.obj@experimentData@other$RawPValues <- FALSE
        
        
        #colnames(exprs(rv$current.obj)) <- gsub(".", "_", colnames(exprs(rv$current.obj)), fixed=TRUE)
        #colnames(pData(rv$current.obj)) <- gsub(".", "_", colnames(pData(rv$current.obj)), fixed=TRUE)
        
        rv$current.obj <- addOriginOfValue(rv$current.obj)
        l.params <- list(filename = rv$current.obj.name)
        UpdateLog("Original",l.params)
        
        
        #if (input$showCommandLog){
            writeToCommandLogFile(
            paste("current.obj <- readRDS('",input$file$name,"')", sep="")
            )
            #}
        
        loadObjectInMemoryFromConverter()
        
    }

})





output$viewProcessingData <- DT::renderDataTable({
    rv$current.obj
    if (is.null(rv$current.obj)) {return(NULL)}
    
    result = tryCatch(
        {
            data.frame(History=(rv$current.obj)@processingData@processing
            [-grep("Subset", (rv$current.obj)@processingData@processing)])
        }
        , warning = function(w) {
            shinyjs::info(conditionMessage(w))
        }, error = function(e) {
            shinyjs::info(paste("view processing data",":",
                                conditionMessage(e), 
                                sep=" "))
        }, finally = {
            #cleanup-code 
        })

},
option=list(pageLength=DT_pagelength,
            orderClasses = TRUE,
            autoWidth=FALSE,
            dom = 'R<"clear">lfrtip',
            columnDefs = list(list(columns.width=c("60px"),
                            columnDefs.targets= c(list(0),list(1),list(2)))))
)







output$downloadMSnSet <- downloadHandler(
    filename = function() { 
        #input$nameExport
        if (input$fileformatExport == gFileFormatExport$excel) {
            paste(input$nameExport,gFileExtension$excel,  sep="")}
        else if (input$fileformatExport == gFileFormatExport$msnset)
        {
            paste(input$nameExport,gFileExtension$msnset,  sep="")}
        else if (input$fileformatExport == gFileFormatExport$zip)
        {
            paste(input$nameExport,gFileExtension$zip,  sep="")}
        
    },
    content = function(file) {
        
        
        if (length(input$colsToExport) == 1){
            Biobase::fData(rv$current.obj) <- 
                data.frame(fData(rv$current.obj)[,input$colsToExport])
            colnames( Biobase::fData(rv$current.obj)) <- input$colsToExport
            #if (input$showCommandLog){
              t <- buildWritableVector(input$colsToExport)
              
              writeToCommandLogFile(
                paste("fData(current.obj) <- fData(current.obj)[,", t, "]", 
                      sep="")
            )
            #}
        }
        else if (length(input$colsToExport) > 1){
            Biobase::fData(rv$current.obj) <- 
                data.frame(fData(rv$current.obj)[,input$colsToExport])
            #if (input$showCommandLog){
                t <- buildWritableVector(input$colsToExport)
                writeToCommandLogFile(
                        paste("fData(current.obj) <- fData(current.obj)[,", t, "]", sep="")
            )
           # }
        }
        
        rv$current.obj@experimentData@other$Prostar_Version <- 
            installed.packages(lib.loc = Prostar.loc)["Prostar","Version"]
        rv$current.obj@experimentData@other$DAPAR_Version <- 
            installed.packages(lib.loc = DAPAR.loc)["DAPAR","Version"]
        colnames(fData(rv$current.obj)) <- gsub(".", "_", 
                                                colnames(fData(rv$current.obj)), 
                                                fixed=TRUE)
        names(rv$current.obj@experimentData@other) <- gsub(".", "_", names(rv$current.obj@experimentData@other), fixed=TRUE)
        
        #colnames(exprs(rv$current.obj)) <- gsub(".", "_", colnames(exprs(rv$current.obj)), fixed=TRUE)
        #colnames(pData(rv$current.obj)) <- gsub(".", "_", colnames(pData(rv$current.obj)), fixed=TRUE)
        
        # if (is.null(rv$current.obj@experimentData@other$OriginOfValues)){
        #     rv$current.obj@experimentData@other$OriginOfValues <- 
        #         Matrix(as.numeric(!is.na(rv$current.obj)),
        #                nrow = nrow(rv$current.obj), 
        #                sparse=TRUE)
        # }
        
        
        if (input$fileformatExport == gFileFormatExport$excel) {
            fname <- paste(input$nameExport,gFileExtension$excel,  sep="")
            writeMSnsetToExcel(rv$current.obj, input$nameExport)
            #if (input$showCommandLog){
                writeToCommandLogFile(
                paste("writeMSnsetToExcel(current.obj,\"", 
                      input$nameExport, "\")", 
                      sep="")
            )
            #}
            
            
            file.copy(fname, file)
            file.remove(fname)
        }
        
        else if  (input$fileformatExport == gFileFormatExport$msnset) {
            fname <- paste(input$nameExport,gFileExtension$msnset,  sep="")
            saveRDS(rv$current.obj,file=fname)
           # if (input$showCommandLog){
                writeToCommandLogFile(
                paste("saveRDS(current.obj, \"", fname, "\")", sep="")
            )
           # }
            file.copy(fname, file)
            file.remove(fname)
        }
        
        else if  (input$fileformatExport == gFileFormatExport$zip) {
            fname <- paste(input$nameExport,gFileExtension$zip,  sep="")
            writeMSnsetToCSV(rv$current.obj,fname)
            # if (input$showCommandLog){
            writeToCommandLogFile(
                paste("writeMSnsetToCSV(current.obj, \"", fname, "\")", sep="")
            )
            # }
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
output$MSnsetView <- renderPrint({
    rv$current.obj
    if (is.null(rv$current.obj)) {return(NULL)  }
    rv$current.obj
})






#######################################
observeEvent(input$createMSnsetButton,ignoreInit =  TRUE,{
    if(is.null(input$createMSnsetButton) || (input$createMSnsetButton == 0)) 
    {return(NULL)}
    
    isolate({
        result = tryCatch(
            {
                    ext <- GetExtension(input$file1$name)
                    txtTab <-  paste("tab1 <- read.csv(\"", input$file1$name,
                            "\",header=TRUE, sep=\"\t\", as.is=T)",  sep="")
                    txtXls <-  paste("tab1 <- read.xlsx(",input$file1$name,
                              ",sheet=", input$XLSsheets,")",sep="")
                    switch(ext,
                           txt = writeToCommandLogFile(txtTab),
                           csv = writeToCommandLogFile(txtTab),
                           tsv = writeToCommandLogFile(txtTab),
                           xls= writeToCommandLogFile(txtXls),
                           xlsx = writeToCommandLogFile(txtXls)
                            )

                input$filenameToCreate
                rv$tab1
                
                indexForEData <- match(input$eData.box, colnames(rv$tab1))
                indexForFData <- seq(1,ncol(rv$tab1))[-indexForEData]
                
                indexForIDBox <- NULL
                if (input$autoID == "user ID") {
                    indexForIDBox <- match(input$idBox, colnames(rv$tab1))
                    }
                
                
                metadata <- hot_to_r(input$hot)
                logData <- (input$checkDataLogged == "no")
                
                
                indexForOriginOfValue <- NULL
                for (i in 1:length(input$eData.box)){
                    indexForOriginOfValue <- c(indexForOriginOfValue, which(colnames(rv$tab1) == input[[paste0("colForOriginValue_", i)]]))
                }
               
                
                    tmp <- createMSnset(rv$tab1, 
                                               metadata, 
                                               indexForEData, 
                                               indexForFData, 
                                               indexForIDBox,
                                               indexForOriginOfValue,
                                               logData, 
                                               input$replaceAllZeros,
                                               pep_prot_data = input$typeOfData
                )
                ClearUI()
                ClearMemory()
                rv$current.obj <- tmp
                rv$current.obj.name <- input$filenameToCreate
                rv$indexNA <- which(is.na(exprs(rv$current.obj)))
                
                l.params <- list(filename = input$filenameToCreate)
                UpdateLog("Original",l.params)
                
                
                #if (input$showCommandLog){
                
                 metadata <- as.data.frame(metadata)
                 t <- "metadata <- data.frame("
                for (c in colnames(metadata)){
                    t <- paste(t,c, " = c(",sep="")
                    
                    for (i in 1:(nrow(metadata)-1)){
                        
                        car <- metadata[i,as.character(c)]
                        #if (car == " ") { car <- NA}
                        t <- paste(t,"\"",car, "\",",
                                   sep="")
                    }
                    
                    car <- last(metadata[,as.character(c)])
                    #if (car == " ") { car <- NA}
                    
                    t <- paste(t,"\"",car, "\")",
                               sep="")
                    if (c!= last(colnames(metadata))){t <- paste(t,", ") }
                    else {t <- paste(t,")") }
                }
                
                
                writeToCommandLogFile(t)
                
                t <- "rownames(metadata) <- c("
                for (i in rownames(metadata)){
                    t <- paste(t,"\"",as.character(i), "\"",sep="")
                    if (i != last(rownames(metadata))){t <- paste(t,", ") }
                    else {t <- paste(t,")") }
                }
                writeToCommandLogFile(t)
                
                
                p <- "c("
                for (i in 1:(length(indexForEData)-1)){
                    p <- paste(p,indexForEData[i], ",",sep="")}
                p <- paste(p, last(indexForEData), ")", sep="")
                writeToCommandLogFile(paste("indexForEData <- ",p, sep=""))
                
                p <- "c("
                for (i in 1:(length(indexForFData)-1)){
                    p <- paste(p,indexForFData[i], ",",sep="")}
                p <- paste(p, last(indexForFData), ")", sep="")
                writeToCommandLogFile(paste("indexForFData <- ",p, sep=""))
                
                car <- 'NULL'
                if (!is.null(indexForIDBox)) {
                    car <- indexForIDBox}
                writeToCommandLogFile(
                    paste("indexForIDBox <- ", car, sep="")
                )
                
                writeToCommandLogFile(
                    paste("logData <- ", logData, sep="")
                )
                
                writeToCommandLogFile(
                    paste("replaceZeros <- ",input$replaceAllZeros, sep = "")
                )
                
                writeToCommandLogFile(
                    paste("pep_prot_data <- \"",input$typeOfData, "\"", sep="")
                )
                
                writeToCommandLogFile(
                    paste("current.obj <- createMSnset(tab1, metadata, indexForEData, indexForFData, indexForIDBox,logData, replaceZeros, pep_prot_data)")
                )

                loadObjectInMemoryFromConverter()
                updateTabsetPanel(session, "tabImport", selected = "Convert")
            }
            , warning = function(w) {
                if (conditionMessage(w) %in% c("NaNs produced", "production de NaN")){
                    shinyjs::info(paste("Warning : Your original dataset may contain negative values",
                                        "so that they cannot be logged. Please check back the dataset or", 
                                        "the log option in the first tab.",
                                        sep=" "))
                } else {
                shinyjs::info(paste("Warning in CreateMSnSet",":",
                                    conditionMessage(w), 
                                    sep=" "))
                }
            }, error = function(e) {
                shinyjs::info(paste("Error :","CreateMSnSet",":",
                                    conditionMessage(e), 
                                    sep=" "))
            }, finally = {
                #cleanup-code 
            })
        
        
        
    })
})






output$chooseMetaDataExport <- renderUI({
    rv$current.obj
    if (is.null(rv$current.obj)) {return(NULL)  }
    
    choices <- colnames(fData(rv$current.obj))
    names(choices) <- colnames(fData(rv$current.obj))
    selectizeInput("colsToExport",
                   label = "",
                   choices = choices,
                   multiple = TRUE, width='500px')
    
})





output$logSession <- DT::renderDataTable({
    req(rv$text.log)
    
    dt <- DT::datatable(rv$text.log, 
                               options=list(pageLength=DT_pagelength,
                                            orderClasses = TRUE,
                                            autoWidth=FALSE,
                                            escape = FALSE))
    dt
})




output$showDatasetDoc <- renderUI({
    input$demoDataset
  input$showDemoDatasetPDF
    if (is.null(input$demoDataset)) { return(NULL)}
  if (!input$showDemoDatasetPDF) { return(NULL)}
  
    file<- paste(system.file(package = "DAPARdata"),"/doc/",
                 input$demoDataset,".pdf", sep="")
    cmd <- paste("cp ",file," www/.", sep="")
    system(cmd)
    tags$iframe(src=paste(input$demoDataset,".pdf", sep=""), 
                width="900", height="700")
    
})





# store the object in binary file
saveMSnset <- function(name, fileExt, obj ){
    saveRDS(obj,file=paste(rv$dirname,"/", name, fileExt,sep=""))
    return(obj)
}



#####-------------------------------------------------------
output$ManageXlsFiles <- renderUI({
    input$file1
    if (is.null(input$file1)){return(NULL)}
    
    .ext <- GetExtension(input$file1$name)
    if ((.ext == "xls") || (.ext == "xlsx")){ 
        sheets <- listSheets(input$file1$datapath)
        selectInput("XLSsheets", "sheets", choices = as.list(sheets))
    }
    
})


output$ConvertOptions <- renderUI({
    input$file1
    if (is.null(input$file1)){return(NULL)}
    
    tagList(
        radioButtons("typeOfData", 
                     "Is it a peptide or protein dataset ?", 
                     choices=c("peptide dataset" = "peptide", 
                               "protein dataset" = "protein")
        )
        
        ,radioButtons("checkDataLogged", 
                      "Are your data already log-transformed ?", 
                      #width = widthWellPanel, 
                choices=c("yes (they stay unchanged)" = "yes", 
                        "no (they wil be automatically transformed)"="no"), 
                      selected="no")
        ,br()
        ,checkboxInput("replaceAllZeros", 
                       "Replace all 0 and NaN by NA", 
                       value= TRUE)
    )
})

############ Read text file to be imported ######################
observe({
    input$file1
    input$XLSsheets
    if (is.null(input$file1) ) {return(NULL)  }
    if (((GetExtension(input$file1$name)== "xls") 
         || (GetExtension(input$file1$name) == "xlsx") ) 
        && is.null(input$XLSsheets)) {return(NULL)  }
    
    
    result = tryCatch(
        {
            ClearUI()
            ClearMemory()
            ext <- GetExtension(input$file1$name)
              
            switch(ext,
                    txt = { rv$tab1 <- read.csv(input$file1$datapath,  header=TRUE, sep="\t", as.is=T)},
                    csv = { rv$tab1 <- read.csv(input$file1$datapath,  header=TRUE, sep="\t", as.is=T)},
                    tsv = { rv$tab1 <- read.csv(input$file1$datapath,  header=TRUE, sep="\t", as.is=T)},
                    xls = {rv$tab1 <- readExcel(input$file1$datapath, ext,sheet=input$XLSsheets)},
                    xlsx = {rv$tab1 <- readExcel(input$file1$datapath, ext,sheet=input$XLSsheets)}
                    )
        }
        , warning = function(w) {
            shinyjs::info(conditionMessage(w))
        }, error = function(e) {
            shinyjs::info(paste("Read text file to convert",":",
                                conditionMessage(e), 
                                sep=" "))
        }, finally = {
            #cleanup-code 
        })
    
    
})






output$conversionDone <- renderUI({
    rv$current.obj
    if (is.null(rv$current.obj)) { return(NULL)}
    
    h4("The conversion is done. Your dataset has been automatically loaded 
       in memory. Now, you can switch to the Descriptive statistics panel to 
       vizualize your data.")
    
})







output$infoAboutAggregationTool <- renderUI({
    rv$current.obj
    rv$typeOfDataset
    if (is.null(rv$current.obj)) {return(NULL)    }
    NA.count <- length(which(is.na(Biobase::exprs(rv$current.obj))))
    
nb.empty.lines <- sum(apply(is.na(as.matrix(exprs(rv$current.obj))), 1, all))
    
    tagList(
        tags$h3("Info"),
        if (rv$typeOfDataset == "protein"){
            tags$h5("Note: the aggregation tool
                    has been disabled because the dataset contains 
                    protein quantitative data.")
        },
        
        if (NA.count > 0){
            tags$h5("As your dataset contains missing values, you should 
            impute them prior to proceed",br()," 
                    to the differential analysis.")
        },
        if (nb.empty.lines > 0){
            tags$h5("As your dataset contains lines with no values, you 
            should remove them with the filter",br()," tool
            prior to proceed to the analysis of the data.")
        }
        
    )
})





#-------------------------------------------------------------
LogTabPanel <- reactive({
    rv$text.log
    tabPanel(title="logTabPanel",
             value = "tabLogSession",
             h3(paste("R session",Sys.getpid(),sep=" ")),
             DT::dataTableOutput("log")
    )
})



observeEvent(input$fData.box,ignoreInit = TRUE,{

    choices = colnames(rv$tab1)[-which(colnames(rv$tab1) %in% input$fData.box)]
    names(choices) = 
        colnames(rv$tab1)[-which(colnames(rv$tab1) %in% input$fData.box)]
    updateSelectInput(session, "eData.box", 
                      label = "",
                      choices = choices,
                      selected = choices)
    
})


output$InfoTextSourceCode <- renderUI({
  txt <- "It may be noticed that this feature is quite difficult to maintain and it is still in Beta version. 
  If you are interseting in this feature, please let us know by an email so that we could evaluate
  the need to put efforts to maintain it up-to-date."
  helpText(txt)
})


output$code <- renderUI({
    rv$commandLog
    if (is.null(rv$commandLog)){return(NULL)}
    aceEditor("ui"
              , value = paste( rv$commandLog, collapse="\n")
              , mode = "r"
              , theme = "chrome"
              , height = "600px"
              , readOnly = TRUE
    )
    
    
})


output$choosedataTobuildReport <- renderUI({
  rv$dataset
  if (is.null(rv$dataset)){return (NULL)}
  
  checkboxGroupInput("chooseDatasetToExport", 
                     "Choose the datasets to export",
                     choices = names(rv$dataset),
                     selected = names(rv$dataset))
  
})
