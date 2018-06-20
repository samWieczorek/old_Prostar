




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
    #input$showCommandLog
    if (is.null(input$demoDataset)){return (NULL)}
  
    
   # result = tryCatch(
    #    {
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
            #    writeToCommandLogFile("library(DAPARdata)")
            #writeToCommandLogFile(paste("utils::data(",
            #                            input$demoDataset,")", 
            #                            sep=""))
            #writeToCommandLogFile(paste("current.obj <- ",
            #                            input$demoDataset, 
            #                            sep=""))
            #}
            
            loadObjectInMemoryFromConverter()
            

        # }
        # , warning = function(w) {
        #     shinyjs::info(paste("load Demo dataset",conditionMessage(w), sep=""))
        # }, error = function(e) {
        #     shinyjs::info(paste("load Demo dataset",match.call()[[1]],":",
        #                         conditionMessage(e), 
        #                         sep=" "))
        # }, finally = {
        #     #cleanup-code 
        # })

    
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
        rv$updateDesign_designChecked <- check.design(Biobase::pData(rv$current.obj))
        colnames(fData(rv$current.obj)) <- gsub(".", "_", colnames(fData(rv$current.obj)), fixed=TRUE)
        names(rv$current.obj@experimentData@other) <- gsub(".", "_", names(rv$current.obj@experimentData@other), fixed=TRUE)
        
        if (is.null(rv$current.obj@experimentData@other$RawPValues ))
            rv$current.obj@experimentData@other$RawPValues <- FALSE
        
                rv$current.obj <- addOriginOfValue(rv$current.obj)
        l.params <- list(filename = rv$current.obj.name)
        UpdateLog("Original",l.params)
        
        ### check if the design has to be updated
        if (NeedsUpdate()) {
          hideTab(inputId ="navPage", target = "FilterDataTab")
          hideTab(inputId ="navPage", target = "Normalization")
          hideTab(inputId ="navPage", target = "AggregationTab")
          hideTab(inputId ="navPage", target = "imputationTabs")
          hideTab(inputId ="navPage", target = "diffAnalysisTab")
          hideTab(inputId ="navPage", target = "GOAnalysisTab")
          hideTab(inputId ="navPage", target = "convertTab")
          hideTab(inputId ="navPage", target = "demoTab")
          hideTab(inputId ="navPage", target = "SessionLogsTab")
          
          showTab(inputId ="navPage", target = "updateDesignTab")
        } else {
            hideTab(inputId ="navPage", target = "updateDesignTab")
            showTab(inputId ="navPage", target = "FilterDataTab")
            showTab(inputId ="navPage", target = "Normalization")
            showTab(inputId ="navPage", target = "AggregationTab")
            showTab(inputId ="navPage", target = "imputationTabs")
            showTab(inputId ="navPage", target = "diffAnalysisTab")
            showTab(inputId ="navPage", target = "GOAnalysisTab")
            showTab(inputId ="navPage", target = "convertTab")
            showTab(inputId ="navPage", target = "demoTab")
            showTab(inputId ="navPage", target = "SessionLogsTab")
        }
        #if (input$showCommandLog){
            writeToCommandLogFile(
            paste("current.obj <- readRDS('",input$file$name,"')", sep="")
            )
            #}
        
        #loadObjectInMemoryFromConverter()
        loadObjectInMemoryFromConverter_2(rv$current.obj)
        
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
option=list(initComplete = initComplete(),
    pageLength=DT_pagelength,
            orderClasses = TRUE,
            autoWidth=FALSE,
            dom = 'R<"clear">lfrtip',
            columnDefs = list(list(columns.width=c("60px"),
                            columnDefs.targets= c(list(0),list(1),list(2)))))
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








output$logSession <- DT::renderDataTable({
    req(rv$text.log)
    
    dt <- DT::datatable(rv$text.log,
                        escape = FALSE,
                        extensions = 'Scroller',
                        rownames = FALSE,
                        options=list(initComplete = initComplete(),
                                   pageLength=DT_pagelength,
                                   deferRender = TRUE,
                                   bLengthChange = FALSE,
                                   scrollX = 200,
                                   scrollY = 600,
                                   scroller = TRUE,
                                   orderClasses = TRUE,
                                   autoWidth=FALSE,
                                   columnDefs = list(
                                       list(columns.width=c("60px","60px"),
                                            columnDefs.targets= c(list(0),list(1))))
                               ))
    dt
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


NeedsUpdate <- reactive({
    req(rv$current.obj)
    
    PROSTAR.version <- rv$current.obj@experimentData@other$Prostar_Version
    
    if (!is.null(PROSTAR.version) && PROSTAR.version >= "1.12.9"
        && (DAPAR::check.design(Biobase::pData(rv$current.obj))$valid))
    {return (FALSE)}
    
    else {
    return(TRUE)
    }
})




output$infoAboutAggregationTool <- renderUI({
    rv$typeOfDataset
    req(rv$current.obj)
    
    if (NeedsUpdate())
    {    
            tags$div(
                tags$div(style="display:inline-block; vertical-align: top;",
                         tags$img(src = "images/Problem.png", height=25)),
                tags$div(style="display:inline-block; vertical-align: top;",
                         tags$p("The dataset was created with a former version of ProStaR, which experimental design is not compliant with the current
                                       software functionalities. Please go to \"Update design\" in the \"Dataset manager\" menu tu update it."))
            )
    } else{
       
        
    NA.count <- length(which(is.na(Biobase::exprs(rv$current.obj))))
    
    nb.empty.lines <- sum(apply(is.na(as.matrix(exprs(rv$current.obj))), 1, all))
    
    tagList(
        tags$h3("Info"),
        if (rv$typeOfDataset == "protein"){
            tags$p("Note: the aggregation tool
                    has been disabled because the dataset contains 
                    protein quantitative data.")
        },
        
        if (NA.count > 0){
            tags$p("As your dataset contains missing values, you should 
            impute them prior to proceed",br()," 
                    to the differential analysis.")
        },
        if (nb.empty.lines > 0){
            tags$p("As your dataset contains lines with no values, you 
            should remove them with the filter",br()," tool
            prior to proceed to the analysis of the data.")
        }
        
    )
    
    }
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





