
observe({
    rv$newOrder
    rv$designIsValid
    
    if (isTRUE(rv$designIsValid))
    {
        shinyjs::enable("createMSnsetButton")
    } else {
        shinyjs::disable("createMSnsetButton")
    }

}) 


observe({
  req(input$hot)
  
  rv$hot <-  hot_to_r(input$hot)
})



color_renderer <- reactive({
 # if(is.null(input$hot)){return(NULL)}
 # temp <- hot_to_r(input$hot)
  #rv$hot <- temp
 # print(temp)
   # if (sum(rv$hot$Label=="")>0) { return (NULL)}
  conds <- rv$hot$Label
   pal <- brewer.pal(length(unique(conds)),"Dark2")

    txt <- "function (instance, td, row, col, prop, value, cellProperties) {
    Handsontable.renderers.TextRenderer.apply(this, arguments);"
    c <- 1
        for (i in 1:length(conds)){
        if (conds[i] != "")
          txt <- paste0(txt, "if(row==",(i-1)," && col==",c, ") {td.style.background = '",pal[which(conds[i] == unique(conds))],"';}")
    }
    txt <- paste0(txt,"}")
    
    return (txt)
})


observeEvent(input$FilterConds,{
    req(input$hot) 
    rv$hot <- hot_to_r(input$hot)
    if (length(grep("Bio.Rep", colnames(rv$hot))) > 0) { return(NULL)}
    
    rv$newOrder <- order(rv$hot["Label"])
    rv$hot <- rv$hot[rv$newOrder,]

    
})


#-------------------------------------------------------------
output$hot <- renderRHandsontable({
    req(input$eData.box)
    rv$hot
    input$chooseExpDesign
    
    if (is.null(rv$hot)){
        rv$hot  <- data.frame(Experiment = as.character(input$eData.box),
                              Label = rep("",length(input$eData.box)),
                              stringsAsFactors = FALSE)
         }
    
    rhandsontable(rv$hot,rowHeaders=NULL, fillHandle = list(direction='vertical', autoInsertRow=FALSE,
                                            maxRows=nrow(rv$hot))) %>%
        hot_rows(rowHeights = 30) %>%
        hot_context_menu(allowRowEdit = TRUE, allowColEdit = FALSE,
                         allowInsertRow = FALSE,
                         allowInsertColumn = FALSE,
                         allowRemoveRow = TRUE,
                         allowRemoveColumn = FALSE,
                         autoInsertRow=FALSE     ) %>%
        hot_col(col = "Experiment", readOnly = TRUE) %>%
        hot_cols(renderer = color_renderer())

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
        bLengthChange = FALSE,
        scrollX = 200,
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
        tagList(
          actionButton("FilterConds", "Generate unique samples ID"),
          br(),
          br(),
          br()
        )
    } else {
      tagList(
        br(),br(),
        br(),
        br()
      )
      
    }
})




output$UI_hierarchicalExp <- renderUI({
    req(rv$newOrder)
tagList(
      div(
        div(
          # edit1
          style="display:inline-block; vertical-align: middle;",
          HTML("<strong><font size=\"5\">Experimental design</font></strong>")
           ),
        div(
          # edit2
          style="display:inline-block; vertical-align: middle;",
            tags$button(id="btn_helpDesign", tags$sup("[?]"), class="Prostar_tooltip")
        )
      ),
          
          radioButtons("chooseExpDesign", "",
                 choices = c("Flat design" = "FlatDesign" ,
                             "2 levels design" = "twoLevelsDesign" ,
                             "3 levels design" = "threeLevelsDesign" ))
)

  
})



output$designExamples <- renderUI({
  input$chooseExpDesign
  
  switch(input$chooseExpDesign,
         FlatDesign = {},
         twoLevelsDesign = rHandsontableOutput("twolevelsExample"),
         threeLevelsDesign = rHandsontableOutput("threelevelsExample"))
})

observe({
  shinyjs::onclick("btn_helpDesign",{
    shinyjs::toggle(id = "exLevels", anim = TRUE)}
    )
})

observeEvent(input$chooseExpDesign, {
    req(input$hot) 
    rv$hot <- hot_to_r(input$hot)
    rv$designChecked <- NULL
    switch(input$chooseExpDesign,
           FlatDesign = {
             #if (length(grep("Bio.Rep", colnames(rv$hot))) > 0) { return(NULL)}
             rv$hot  <- data.frame(rv$hot[,1:2],
                              Bio.Rep = seq(1:nrow(rv$hot)),
                              stringsAsFactors = FALSE)
            },
           twoLevelsDesign = {
               #if (length(grep("Tech.Rep", colnames(rv$hot))) > 0) { return(NULL)}
               rv$hot  <- data.frame(rv$hot[,1:2],Bio.Rep = rep("",nrow(rv$hot)),
                                Tech.Rep = seq(1:nrow(rv$hot)),
                                stringsAsFactors = FALSE)
               },
           threeLevelsDesign = {
               #if (length(grep("Tech.Rep", colnames(rv$hot))) > 0) { return(NULL)}
               rv$hot  <- data.frame(rv$hot[,1:2],
                                     Bio.Rep = rep("",nrow(rv$hot)),
                                      Tech.Rep = rep("",nrow(rv$hot)),
                                      Analyt.Rep = seq(1:nrow(rv$hot)),
                                stringsAsFactors = FALSE)
             }
           )
})



output$twolevelsExample <- renderRHandsontable({
  
  df <- data.frame(Sample.name= paste0("Sample ",as.character(1:14)),
                   Condition = c(rep( "A", 4), rep("B", 4), rep("C", 6)),
                   Bio.Rep = as.integer(c(1,1,2,2,3,3,4,4,5,5,6,6,7,7)),
                   Tech.Rep = c(1:14),
                   stringsAsFactors = FALSE)
  
  
  pal <- brewer.pal(3,"Dark2")
  
  color_rend <- "function (instance, td, row, col, prop, value, cellProperties) {
  Handsontable.renderers.TextRenderer.apply(this, arguments);

if(col==1 && (row>=0 && row<=3)) {td.style.background = '#1B9E77';}
if(col==1 && (row>=4 && row<=7)) {td.style.background = '#D95F02';}
if(col==1 && (row>=8 && row<=14)) {td.style.background = '#7570B3';}
  

if(col==2 && (row==0||row==1||row==4||row==5||row==8||row==9||row==12||row==13)) 
  {td.style.background = 'lightgrey';}

if(col==3 && (row==0||row==2||row==4||row==6||row==8||row==10||row==12)) 
  {td.style.background = 'lightgrey';}
  }"

  rhandsontable(df,rowHeaders=NULL, fillHandle = list(direction='vertical', autoInsertRow=FALSE,
                                                      maxRows=nrow(rv$hot))) %>%
    hot_rows(rowHeights = 30) %>%
    hot_context_menu(allowRowEdit = FALSE, allowColEdit = FALSE,
                     allowInsertRow = FALSE,allowInsertColumn = FALSE,
                     allowRemoveRow = FALSE,allowRemoveColumn = FALSE,
                     autoInsertRow=FALSE     ) %>%
    hot_cols(readOnly = TRUE,renderer = color_rend)
  
})



output$threelevelsExample <- renderRHandsontable({
  
  df <- data.frame(Sample.name= paste0("Sample ",as.character(1:16)),
                   Condition = c(rep( "A", 8), rep("B", 8)),
                   Bio.Rep = as.integer(c(rep(1,4),rep(2,4),rep(3,4),rep(4,4))),
                   Tech.Rep = as.integer(c(1,1,2,2,3,3,4,4,5,5,6,6,7,7,8,8)),
                   Analyt.Rep = c(1:16),
                   stringsAsFactors = FALSE)
  
  
  pal <- brewer.pal(2,"Dark2")
  
  color_rend <- "function (instance, td, row, col, prop, value, cellProperties) {
  Handsontable.renderers.TextRenderer.apply(this, arguments);

if(col==1 && (row>=0 && row<=7)) 
  {td.style.background = '#1B9E77';}
 
if(col==1 && (row>=8 && row<=15)) 
  {td.style.background = '#D95F02';}

if(col==2 && (row==0||row==1||row==2||row==3||
                row==8||row==9||row==10||row==11)) 
  {td.style.background = 'lightgrey';}

if(col==3 && (row==0||row==1||row==4||row==5||
                row==8||row==9||row==12||row==13)) 
  {td.style.background = 'lightgrey';}


  if(col==4 && (row==0||row==2||row==4||row==6||
                row==8||row==10||row==12||row==14)) 
  {td.style.background = 'lightgrey';}
}"
 rhandsontable(df,rowHeaders=NULL,fillHandle = list(direction='vertical', autoInsertRow=FALSE,
                                                   maxRows=nrow(rv$hot))) %>%
    hot_rows(rowHeights = 30) %>%
    hot_context_menu(allowRowEdit = FALSE, allowColEdit = FALSE,
                     allowInsertRow = FALSE,allowInsertColumn = FALSE,
                     allowRemoveRow = FALSE,allowRemoveColumn = FALSE,
                     autoInsertRow=FALSE     ) %>%
    hot_cols(readOnly = TRUE,renderer = color_rend)
  
  })



observeEvent(input$FilterConds,{
    rv$newOrder
    shinyjs::enable("chooseExpDesign")

}) 


# checkDesign <- reactive({
#   #input$hot
#   rv$hot
#   #check.design(hot_to_r(input$hot))
#   check.design(rv$hot)
#   
# })

observeEvent(input$btn_checkDesign,{
  rv$hot
  
  rv$designChecked <- check.design(rv$hot)
  
})

output$checkDesign <- renderUI({
  req(input$chooseExpDesign)
  rv$designChecked
  #req(input$hot) 
 # tmp <- hot_to_r(input$hot)
  rv$hot
  switch(input$chooseExpDesign,
         FlatDesign = {},
         twoLevelsDesign = { if (sum(rv$hot$Bio.Rep == "") > 0) {return(NULL)}},
         threeLevelsDesign = {if ((sum(rv$hot$Bio.Rep == "")+sum(rv$hot$Tech.Rep == "")) > 0) {return(NULL)}}
 )
 
  tagList(
    actionButton("btn_checkDesign", "Check design"),
  
  if(!is.null(rv$designChecked)){
    
    if (isTRUE(rv$designChecked$valid)){
      img <- "images/Ok.png"
      txt <- "Correct design"
    }else {
      img <- "images/Problem.png"
      txt <- "Invalid design"}
    tagList(
      tags$div(
        tags$div(style="display:inline-block;",tags$img(src = img, height=25)),
        tags$div(style="display:inline-block;",tags$p(txt))
      ),
      if(!isTRUE(rv$designChecked$valid)){
        tags$p(rv$designChecked$warn)
      }
    )
  }
  )
})

