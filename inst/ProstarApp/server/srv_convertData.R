
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
    for (c in 0:1){
        for (i in 1:length(conds)){
        txt <- paste0(txt, "if(row==",(i-1)," && col==",c, ") {td.style.background = '",pal[which(conds[i] == unique(conds))],"';}")
    }
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

        tags$div(
            tags$div(
                style="display:inline-block;",
                tags$img(src = Okimg, height=15)),
            
            tags$div(
                style="display:inline-block;",
                tags$p("Datafile uploaded")
            )
            
        ),
        
        
        tags$div(
            tags$div(
                style="display:inline-block;",
                tags$img(src = Okimg, height=15)),
            
            tags$div(
                style="display:inline-block;",
                tags$p("Id configured")
            )
            
        ),
        tags$div(
            tags$div(
                style="display:inline-block;",
                tags$img(src = Okimg, height=15)),
            
            tags$div(
                style="display:inline-block;",
                tags$p("Exp. and feat data configured")
            )
            
        ),
        tags$div(
            tags$div(
                style="display:inline-block;",
                tags$img(src = Okimg, height=15)),
            
            tags$div(
                style="display:inline-block;",
                tags$p("Sample metadata configured")
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


output$UI_hierarchicalExp  <- renderUI({
    req(input$hot) 
    input$FilterConds
    tmp <- hot_to_r(input$hot)
    
   # if (input$FilterConds == 0) {return(NULL)}
    
    if (sum(tmp$Label == "")==0){
        radioButtons("chooseExpDesign", "Choose experimental design",
                     choices = c("Flat design" = "Flat design",
                                 "2 levels design" = "2 levels design (Bio Rep then Tech Rep)",
                                 "2 levels design" = "2 levels design (Tech Rep then Bio Rep)",
                                 "3 levels design" = "3 levels desig"))
        
    }
})



