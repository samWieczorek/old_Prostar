

output$optionsDemomode <- renderUI({
    
    req(input$demoDataset)
    tagList(
        checkboxInput("showDemoDatasetPDF", "Show PDF documentation", value=FALSE),
        actionButton("loadDemoDataset", "Load demo dataset")
    )
})

output$chooseDataset <- renderUI({
    
    if(require("DAPARdata")){
        print("DAPARdata is loaded correctly")
        selectInput("demoDataset",
                    "Demo dataset",
                    choices = utils::data(package="DAPARdata")$results[,"Item"]
        )
    } else {
        print("trying to install DAPARdata")
        install.packages("DAPARdata")
        if(require(DAPARdata)){
        print("DAPARdata installed and loaded")
        selectInput("demoDataset",
                    "Demo dataset",
                    choices = utils::data(package='DAPARdata')$results[,"Item"])
        } else {
            stop("Could not install the package DAPARdata")
        }
    }
    
  
})




# output$out <- renderPrint({
#     
#     
#     data.frame(v1=shinyValue("colForOriginValue_",nrow(quantiDataTable())))
#     
#     #print(shinyValue("colForOriginValue_",nrow(quantiDataTable())))
# })


# output$chooseOriginOfValues <- renderUI({
#     input$eData.box
#     input$file1
#     rv$tab1
#     if (is.null(rv$tab1)) {return(NULL)  }
#     
#     choices <- c("None",colnames(rv$tab1))
#     names(choices) <- c("None",colnames(rv$tab1))
#     
#     tagList(
#         if (length(input$eData.box) >= 1) {
#             lapply(1:length(input$eData.box), function(entry) {
#             selectInput(paste0("colForOriginValue_", entry), input$eData.box[entry], choices = choices,
#                     multiple = FALSE, width='500px')
#             }
#         )
#         }
#   )  
# })

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
              label = "",
              value = rv$current.obj.name)
})


# This function computes a new data set. It can optionally take a function,
# updateProgress, which will be called as each row of data is added.
compute_data <- function(updateProgress = NULL) {
    rv$indProgressDemomode
        # If we were passed a progress update function, call it
        if (is.function(updateProgress)) {
            text <- paste0("x:", round(new_row$x, 2), " y:", round(new_row$y, 2))
            updateProgress(detail = text)
        }
        

}


output$progressDemoMode <- renderUI({
    #rv$indProgressDemomode
    req(input$loadDemoDataset)
    
    if (!isTRUE(rv$indProgressDemomode)){
    withProgress(message = 'Initialization. Please wait...', value = 1, {
        Sys.sleep(2000)
    })
    }
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
           # rv$indProgressDemomode <- rv$indProgressDemomode +1
            
            
            
            
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
option=list(initComplete = JS(
    "function(settings, json) {",
    "$(this.api().table().header()).css({'background-color': 'darkgrey', 'color': 'black'});",
    "}"),
    pageLength=DT_pagelength,
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
                        escape = FALSE,
                        extensions = 'Scroller',
                        rownames = FALSE,
                        options=list(initComplete = JS(
                                   "function(settings, json) {",
                                   "$(this.api().table().header()).css({'background-color': 'darkgrey', 'color': 'black'});",
                                   "}"),
                                   pageLength=DT_pagelength,
                                   deferRender = TRUE,
                                   scrollY = 600,
                                   scroller = TRUE,
                                   orderClasses = TRUE,
                                   autoWidth=FALSE,
                                   columnDefs = list(
                                       list(columns.width=c("60px","60px"),
                                            columnDefs.targets= c(list(0),list(1))))
                                    #columnDefs = list(list(width='40px',targets= "1"),
                                    #                 list(width='40px',targets= "2"),
                                    #                 list(width='20px',targets= "3"))
                               ))
    dt
})




output$showDatasetDoc <- renderUI({
    req(input$demoDataset)
    req(input$showDemoDatasetPDF)
    
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
  txt <- "Upgrading this  feature from Beta-version to full release requires a lot of work.
  If you use it, please let us know by email so that we can better evaluate its priority."
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
                     "Datasets to export",
                     choices = names(rv$dataset),
                     selected = names(rv$dataset))
  
})
