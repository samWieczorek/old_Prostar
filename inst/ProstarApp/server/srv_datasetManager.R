

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




# function for dynamic inputs in DT
shinyInput <- function(FUN,id,num,...) {
    inputs <- character(num)
    for (i in seq_len(num)) {
        inputs[i] <- as.character(FUN(paste0(id,i),label=NULL,...))
    }
    inputs
}


# function to read DT inputs
shinyValue <- function(id,num) {
    unlist(lapply(seq_len(num),function(i) {
        value <- input[[paste0(id,i)]]
        if (is.null(value)) NA else value
    }))
}




output$UI_generateSampleID  <- renderUI({
    req(input$hot) 
    tmp <- hot_to_r(input$hot)
    
    if (sum(tmp$Label == "")==0){
        actionButton("FilterConds", "Generate unique samples ID")
      
    }
})



observe({
  rv$newOrder
  
  if (is.null(rv$newOrder))
  {
    shinyjs::disable("createMSnsetButton")
  } else {
    shinyjs::enable("createMSnsetButton")
  }
}) 
  

color_renderer <- function(conds){
    
    if (sum(rv$hot$Label=="")>0) { return (NULL)}
    print(conds)
    nConds <- 
    pal <- brewer.pal(length(unique(conds)),"Dark2")
    print(pal)
    txt <- "function (instance, td, row, col, prop, value, cellProperties) {
    Handsontable.renderers.TextRenderer.apply(this, arguments);"
    for (i in 1:length(conds)){
        txt <- paste0(txt, "if(row==",(i-1)," && col==1) {td.style.background = '",pal[which(conds[i] == unique(conds))],"';}")
    }

    txt <- paste0(txt,"}")

    return (txt)
}


observeEvent(input$FilterConds,{
    req(input$hot) 
    rv$hot <- hot_to_r(input$hot)
    rv$newOrder <- order(rv$hot["Label"])
    rv$hot <- rv$hot[rv$newOrder,]
    rv$hot  <- cbind(rv$hot,
                     Analyt.Rep = seq(1:nrow(rv$hot)),
                          stringsAsFactors = FALSE)
   shinyjs::enable("createMSnsetButton")
    
})

#-------------------------------------------------------------
output$hot <- renderRHandsontable({
    req(input$eData.box)
    rv$hot
    
    if (is.null(rv$hot)){
        rv$hot  <- data.frame(Experiment = as.character(input$eData.box),
                         Label = rep("",length(input$eData.box)),
                         stringsAsFactors = FALSE)
        
        
        # rv$hot  <- data.frame(Experiment = as.character(input$eData.box),
        #                       Label = rep(" ",length(input$eData.box)),
        #                       Bio.Rep = rep(" ",length(input$eData.box)),
        #                       Tech.Rep = rep(" ",length(input$eData.box)),
        #                       Analyt.Rep = rep(" ",length(input$eData.box)),
        #                       stringsAsFactors = FALSE)

    }
    
    #print(rv$hot)
    #if (!is.null(rv$hot))
        
    
    # Custom renderer function
    color_renderer <- "
             function (instance, td, row, col, prop, value, cellProperties) {
    Handsontable.renderers.TextRenderer.apply(this, arguments);
    
    if(row == 3 && col==2) {
    td.style.background = 'pink';
    }
    
}"
    
   rhandsontable(rv$hot, fillHandle = list(direction='vertical', autoInsertRow=FALSE,
                                            maxRows=nrow(rv$hot))) %>%
        hot_rows(rowHeights = 30) %>%
        hot_context_menu(allowRowEdit = TRUE, allowColEdit = FALSE,
                         allowInsertRow = FALSE,
                         allowInsertColumn = FALSE,
                         allowRemoveRow = TRUE,
                         allowRemoveColumn = FALSE,
                         autoInsertRow=FALSE     ) %>%
        hot_col(col = "Experiment", readOnly = TRUE) %>%
        hot_cols(colWidths = c(200, 100, 100, 100, 100),renderer = color_renderer(rv$hot$Label))


    
    # 
    # DF = data.frame(val = 1:10, big = LETTERS[1:10])
    # col_highlight = c(0, 1)
    # row_highlight = c(3)
    # 
    # rhandsontable(DF, col_highlight = col_highlight, row_highlight = row_highlight) %>%
    #     hot_cols(renderer = "
    #              function(instance, td, row, col, prop, value, cellProperties) {
    #              Handsontable.NumericRenderer.apply(this, arguments);
    #              if (instance.params) {
    #              hcols = instance.params.col_highlight
    #              hcols = hcols instanceof Array ? hcols : [hcols]
    #              hrows = instance.params.row_highlight
    #              hrows = hrows instanceof Array ? hrows : [hrows]
    #              }
    #              if (instance.params && hcols.includes(col)) td.style.background = 'red';
    #              if (instance.params && hrows.includes(row)) td.style.background = 'yellow';
    #              }")
    # })


})



output$checkAll_convert <- renderUI({
  input$idBox
  rv$tab1
  
  Okimg <- "images/Ok.png"
  NOkimg <- "images/Problem.png"
  
  tagList(
    #text <- "Datafile uploaded",
    
    div(
      tags$div(
        style="display:inline-block; vertical-align: middle;",
        tags$img(src = Okimg, height=20)),
        
      tags$div(
        style="display:inline-block; vertical-align: middle;",
        tags$p("Datafile uploaded")
      )
    
    ),
    
    
    
    tags$br(),
    
    div(
      tags$div(
        style="display:inline-block; vertical-align: middle;",
        tags$img(src = Okimg, height=20)),
      
      tags$div(
        style="display:inline-block; vertical-align: middle;",
        tags$p("id configured")
      )
      
    )
  )
  
})

output$warningNonUniqueID <- renderUI({
    input$idBox
    rv$tab1
    if (is.null(rv$tab1)) {return(NULL)  }
    if (is.null(input$idBox) || (input$idBox =="")) {return(NULL)  }
    
    t <- (length(as.data.frame(rv$tab1)[, input$idBox])
          == length(unique(as.data.frame(rv$tab1)[, input$idBox])))
    
    if (!t){
        text <- "<img src=\"images/Problem.png\" height=\"24\"></img><font color=\"red\">
        Warning ! Your ID contains duplicate data.
        Please choose another one."

    }
    else {
      text <- "<img src=\"images/Ok.png\" height=\"24\"></img>"
    }
    HTML(text)
})



#########################################################
output$id <- renderUI({
  rv$tab1
  if (is.null(rv$tab1)) {return(NULL)  }
    
     .choices <- c("",colnames(rv$tab1))
    names(.choices) <- c("",colnames(rv$tab1))
      selectInput("idBox", label = "", choices = .choices , selected = NULL)
 
})



###########################################

#
#
##   Quanti data table

##
##
##############################################
output$eData <- renderUI({
    input$file1
    rv$tab1
    if (is.null(rv$tab1)) {return(NULL)  }
    
    choices <- colnames(rv$tab1)
    names(choices) <- colnames(rv$tab1)
    
    tagList(
        modulePopoverUI("modulePopover_convertDataQuanti"),
        selectInput("eData.box",
                   label = "",
                   choices = choices,
                   multiple = TRUE, width='200px',
                size = 20,
                selectize = FALSE)
    )
})



updateInputs <- function(id, n){
    for (i in seq_len(n)) {
        updateSelectInput(paste0(id,i),label=NULL,selected = input[[paste0(id,i)]])
    }
}


shinyOutput <- function(FUN,id,num,...) {
    inputs <- character(num)
    for (i in seq_len(num)) {
        inputs[i] <- as.character(FUN(paste0(id,i),label=NULL,...))
    }
    inputs
}

# function for dynamic inputs in DT
shinyInput <- function(FUN,id,num,...) {
    inputs <- character(num)
    for (i in seq_len(num)) {
        inputs[i] <- as.character(FUN(paste0(id,i),label=NULL,...))
    }
    inputs
}


# function to read DT inputs
shinyValue <- function(id,num) {
    unlist(lapply(seq_len(num),function(i) {
        value <- input[[paste0(id,i)]]
        if (is.null(value)) NA else value
    }))
}



iconDataTable <- reactive({
    temp <- shinyValue("colForOriginValue_",length(input$eData.box))
    
    if (length(which(temp == "None")) == length(temp))
    {
        status_list <- rep('<img src="images/Ok.png" height="24"></img>', 
                           nrow(as.data.frame(input$eData.box)))
    } else {
        status_list <- rep('<img src="images/Problem.png" height="24"></img>', 
                           nrow(as.data.frame(input$eData.box)))
    }
    
     if (length(which(is.na(temp))) ==0)
    {
        for (i in seq_len(length(temp))) {
            if (temp[i] != "None"){
                status_list[i] <- '<img src="images/Ok.png" height="24"></img>'
            }
        }
    }
    status_list
})


# reactive dataset
quantiDataTable <- reactive({
    req(input$eData.box)
    req(rv$tab1)
    
    session$sendCustomMessage('unbind-DT', 'x1')
    df <- NULL
    if (isTRUE(input$selectIdent)) {
        choices <- c("None",colnames(rv$tab1))
        names(choices) <- c("None",colnames(rv$tab1))

       status_list <- rep('<img src="images/Problem.png" height="24"></img>', 
                          nrow(as.data.frame(input$eData.box)))
       
        df <- data.frame(as.data.frame(input$eData.box),
                   shinyInput(selectInput,"colForOriginValue_",nrow(as.data.frame(input$eData.box)),choices=choices),
                   iconDataTable())
        colnames(df) <- c("Sample", "Identification method", "Status")
        
        for (i in seq_len(nrow(as.data.frame(input$eData.box)))) {
            updateSelectInput(session,paste0("colForOriginValue_",i),selected = input[[paste0("colForOriginValue_",i)]])
        }

    } else {
        df <- data.frame(Sample = as.data.frame(input$eData.box))
        colnames(df) <- c("Sample")
    }
df
})



output$x1 <- renderDataTable(
    quantiDataTable(),
              escape=FALSE,
              rownames = FALSE,
              extensions = 'Scroller',
              server=FALSE,
              selection='none', 
    class = 'compact',
options=list(
    preDrawCallback=JS(
    'function() {
    Shiny.unbindAll(this.api().table().node());}'),
    drawCallback= JS(
        'function(settings) {
        Shiny.bindAll(this.api().table().node());}'),
   # rowCallback = JS("function(r,d) {$(r).attr('height', '10px')}"),
    dom = 't',
    autoWidth=TRUE,
    deferRender = TRUE,
    scrollY = 500,
    scroller = TRUE,
   ajax = list(url = dataTableAjax(session, quantiDataTable()))
   
)

)


observeEvent(shinyValue("colForOriginValue_",nrow(quantiDataTable())),{

}
)



output$warningCreateMSnset <- renderUI({
    colNamesForOriginofValues <- shinyValue("colForOriginValue_",nrow(quantiDataTable()))
    if (length(which(colNamesForOriginofValues == "None")) >0){
        text <- "<font color=\"red\"> Warning: The MSnset cannot be created because the identification 
        method are not fully filled.  <br>"
        HTML(text)
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






#######################################
observeEvent(input$createMSnsetButton,ignoreInit =  TRUE,{
    if(is.null(input$createMSnsetButton) || (input$createMSnsetButton == 0)) 
    {return(NULL)}
    
    colNamesForOriginofValues <- shinyValue("colForOriginValue_",nrow(quantiDataTable()))
    
    if (length(which(colNamesForOriginofValues == "None")) >0){
        return (NULL)
    }
    
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
                
                tmp.eData.box <- input$eData.box
                indexForEData <- match(tmp.eData.box, colnames(rv$tab1))
                if (!is.null(rv$newOrder)){
                  tmp.eData.box <- tmp.eData.box[rv$newOrder]
                  indexForEData <- indexForEData[rv$newOrder]
                }
                
                indexForFData <- seq(1,ncol(rv$tab1))[-indexForEData]
                
                indexForIDBox <- NULL
                if (input$autoID == "user ID") {
                    indexForIDBox <- match(input$idBox, colnames(rv$tab1))
                    }
                
                
                metadata <- hot_to_r(input$hot)
                logData <- (input$checkDataLogged == "no")
                
                 
                indexForOriginOfValue <- NULL
                if ((length(grep("None", colNamesForOriginofValues))==0)  && (sum(is.na(colNamesForOriginofValues)) == 0)){
                    for (i in 1:length(tmp.eData.box)){
                    indexForOriginOfValue <- c(indexForOriginOfValue, which(colnames(rv$tab1) == input[[paste0("colForOriginValue_", i)]]))
                    }
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
