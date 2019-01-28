callModule(modulePopover,"modulePopover_convertChooseDatafile", 
           data = reactive(list(title = HTML(paste0("<strong><font size=\"4\">Data file</font></strong>")), 
                                content="Select one (.txt, .csv, .tsv, .xls, .xlsx) file.")))

callModule(modulePopover,"modulePopover_convertIdType", 
           data = reactive(list(title = HTML(paste0("<strong><font size=\"4\">ID definition</font></strong>")), 
                                content="If you choose the automatic ID, Prostar will build an index.")))



callModule(modulePopover,"modulePopover_convertProteinID", 
           data = reactive(list(title = HTML(paste0("<strong><font size=\"4\">Select protein IDs</font></strong>")), 
                                content="Select the column containing the parent protein IDs.")))


callModule(modulePopover,"modulePopover_convertDataQuanti", 
           data = reactive(list(title = HTML(paste0("<strong><font size=\"4\">Quantitative data</font></strong>")), 
                                content="Select the columns that are quantitation values by clicking in the field below.")))

callModule(moduleStaticDataTable,"overview_convertData", table2show=reactive({GetDatasetOverview()}))




##--------------------------------------------------------------
## Gestion du slideshow
##--------------------------------------------------------------


output$checkConvertPanel <- renderUI({
  rv$tab1
  rv$pageConvert
  color <- rep("lightgrey",NUM_PAGES_CONVERT)
  
  ##Step 1
  if (rv$pageConvert >= 1){
    res <- !is.null(rv$tab1)
    ifelse(res, color[1] <- "green", color[1] <- "red")
    toggleState(id = "nextBtnConvert", condition = (rv$pageConvert < NUM_PAGES_CONVERT) && res)
    toggleState(id = "prevBtnConvert", condition = rv$pageConvert > 1)
    hide(selector = ".page")
  }
  
  ##Step 2: Choose data ID
  
  if (rv$pageConvert >= 2){
    res1 <- !is.null(input$idBox) && ((input$idBox == "Auto ID") || datasetID_Ok())
    res2 <- !is.null(input$convert_proteinId) && (input$convert_proteinId != "")
    
    ifelse(res1 && res2, color[2] <- "green", color[2] <- "red")
    toggleState(id = "nextBtnConvert", condition = (rv$pageConvert < NUM_PAGES_CONVERT) && res1 && res2)
    toggleState(id = "prevBtnConvert", condition = rv$pageConvert > 1)
    hide(selector = ".page")
  } 
  
  ## Step 3: Choose quantitative data
  if (rv$pageConvert >= 3){
    res <- !is.null(input$eData.box) && checkIdentificationMethod_Ok()
    
    ifelse(res, color[3] <- "green", color[3] <- "red")
    toggleState(id = "nextBtnConvert", condition = (rv$pageConvert < NUM_PAGES_CONVERT) && res)
    toggleState(id = "prevBtnConvert", condition = rv$pageConvert > 1)
    hide(selector = ".page")
  }
  
  if (rv$pageConvert >= 4){
    res <- isTRUE(rv$designChecked$valid)
    ifelse(res, color[4] <- "green", color[4] <- "red")
    toggleState(id = "nextBtnConvert", condition = (rv$pageConvert < NUM_PAGES_CONVERT) && res)
    toggleState(id = "prevBtnConvert", condition = rv$pageConvert > 1)
    hide(selector = ".page")
  }
  
  if (rv$pageConvert >= 5){
    res <- TRUE
    ifelse(!is.null(rv$current.obj), color <- rep("green",NUM_PAGES_CONVERT), color[5] <- "red")
    toggleState(id = "nextBtnConvert", condition = (rv$pageConvert < NUM_PAGES_CONVERT) && res)
    toggleState(id = "prevBtnConvert", condition = rv$pageConvert > 1)
    hide(selector = ".page")
  }
  
  txt <- c("Select file", "Select ID", "Select quantitative data", "Build design", "Convert")
  buildTable(txt, color)
})

NUM_PAGES_CONVERT <- 5

# observe({
#   toggleState(id = "prevBtnConvert", condition = rv$pageConvert > 1)
#  # toggleState(id = "nextBtnConvert", condition = rv$pageConvert < NUM_PAGES_CONVERT)
#   hide(selector = ".page")
# })

navPageConvert <- function(direction) {
  rv$pageConvert <- rv$pageConvert + direction
}

observeEvent(input$prevBtnConvert, navPageConvert(-1))
observeEvent(input$nextBtnConvert, navPageConvert(1))

##---------------------------------------------------------------
##------------------------------------------------------------------













#################################
output$Convert_SelectFile <- renderUI({
  #if (rv$pageConvert != 1){return()}
  
  
  tagList(br(), br(),
          fluidRow(
            column(width=2, modulePopoverUI("modulePopover_convertChooseDatafile")),
            column(width = 10, fileInput("file1", "", 
                                         multiple=FALSE, 
                                         accept=c(".txt", ".tsv", ".csv",".xls", ".xlsx")))),
          uiOutput("ManageXlsFiles"),
          # helpText("Hint : before importing quantification 
          #             file data, check the syntax of your text 
          #             file."),
          br(),
          uiOutput("ConvertOptions")
  )
})


output$Convert_DataId <- renderUI({
  #if (rv$pageConvert != 2){return()}
  
  tagList(
    
br(), br(),
#uiOutput("helpTextDataID"),

tags$div(
  tags$div( style="display:inline-block; vertical-align: top; padding-right: 100px;",
            uiOutput("id"),
            uiOutput("warningNonUniqueID")
  ),
  tags$div( style="display:inline-block; vertical-align: top;",
            uiOutput("convertChooseProteinID_UI")
  )
)
)
})




output$Convert_ExpFeatData <- renderUI({
  #if (rv$pageConvert != 3){return()}
  
  tagList(
    fluidRow(
      column(width=4,checkboxInput("selectIdent", 
                                   "Select columns for identification method", 
                                   value = FALSE)),
      column(width=4,uiOutput("checkIdentificationTab"))
    ),
    fluidRow(
      column(width=4,uiOutput("eData",width = "400px")),
      column(width=8,DT::dataTableOutput("x1", width='500px'))),
    tags$script(HTML("Shiny.addCustomMessageHandler('unbind-DT', function(id) {
                                   Shiny.unbindAll($('#'+id).find('table').DataTable().table().node());
                                   })"))
                           )
  })





output$Convert_BuildDesign <- renderUI({
  #if (rv$pageConvert != 4){return()}
  
  tagList(... = tagList(
    tags$p("If you do not know how to fill the experimental design, you can click
                                  on the '?' next to each design in the list that appear once the conditions 
                                  are checked or got to the ", 
           actionLink("linkToFaq1", "FAQ",style="background-color: white"), 
           " page."),
    fluidRow(
      column(width=6,tags$b("1 - Fill the \"Condition\" column to identify the conditions to compare.")),
      column(width=6,uiOutput("UI_checkConditions")  )
    ),
    fluidRow(
      column(width=6,uiOutput("UI_hierarchicalExp")),
      column(width=6,uiOutput("checkDesign") )
    )
  ),
  hr(),
  selectInput("convert_reorder", "Order by conditions ?",
              choices=c(" "="None","No"="No", "Yes"="Yes"),
              width="100px"),
  tags$div(
    
    tags$div(style="display:inline-block; vertical-align: top;",
             uiOutput("viewDesign",width="100%")
    ),
    tags$div(style="display:inline-block; vertical-align: top;",
             shinyjs::hidden(div(id = "showExamples", uiOutput("designExamples") ))
    )
    
    
  ))
  
})






output$Convert_Convert <- renderUI({
  #if (rv$pageConvert != 5){return()}
  
  tagList(
    br(), br(),
  
  uiOutput("convertFinalStep"),
  moduleStaticDataTableUI("overview_convertData"),
  uiOutput("conversionDone")
  
  )
})

output$warningNonUniqueID <- renderUI({
    req(input$idBox)
    req(rv$tab1)
    if (input$idBox =="Auto ID") {return(NULL)  }
    
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


output$convertChooseProteinID_UI <- renderUI({
  req(rv$tab1)
  
  if (input$typeOfData == "protein") {return(NULL)}
  
  .choices <- c("",colnames(rv$tab1))
  names(.choices) <- c("",colnames(rv$tab1))
  tagList(
    modulePopoverUI("modulePopover_convertProteinID"),
    selectInput("convert_proteinId", 
              "",
              choices =  .choices , selected = character(0))
  )
})


#########################################################
output$id <- renderUI({
  req(rv$tab1)
  
  .choices <- c("Auto ID",colnames(rv$tab1))
  names(.choices) <- c("Auto ID",colnames(rv$tab1))
  
  tagList(
    modulePopoverUI("modulePopover_convertIdType"),
    selectInput("idBox", label = "", choices = .choices)
  )
  
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


observeEvent(input$fData.box,ignoreInit = TRUE,{
  
  choices = colnames(rv$tab1)[-which(colnames(rv$tab1) %in% input$fData.box)]
  names(choices) = 
    colnames(rv$tab1)[-which(colnames(rv$tab1) %in% input$fData.box)]
  updateSelectInput(session, "eData.box", 
                    label = "",
                    choices = choices,
                    selected = choices)
  
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





readTextFile <- reactive({
  rv$tab1 <- read.csv(input$file1$datapath,  header=TRUE, sep="\t", as.is=T)
})

readXLSFile <- reactive({})

############ Read text file to be imported ######################
observeEvent(c(input$file1,input$XLSsheets),{
  
  input$XLSsheets
  if (((GetExtension(input$file1$name)== "xls")
       || (GetExtension(input$file1$name) == "xlsx") )
      && is.null(input$XLSsheets)) {return(NULL)  }

  authorizedExts <- c("txt","csv", "tsv","xls","xlsx")
  if( is.na(match(GetExtension(input$file1$name), authorizedExts))) {
    shinyjs::info("Warning : this file is not a text nor an Excel file ! 
                  Please choose another one.")
  }
  else {
  # result = tryCatch(
  #   {
      ClearUI()
      ClearMemory()
      ext <- GetExtension(input$file1$name)
      
      switch(ext,
             txt = { rv$tab1 <- read.csv(input$file1$datapath,  header=TRUE, sep="\t", as.is=T)},
             csv = { rv$tab1 <- read.csv(input$file1$datapath,  header=TRUE, sep="\t", as.is=T)},
             tsv = { rv$tab1 <- read.csv(input$file1$datapath,  header=TRUE, sep="\t", as.is=T)},
             xls = { rv$tab1 <- readExcel(input$file1$datapath, ext, sheet=input$XLSsheets)},
             xlsx = {rv$tab1 <- readExcel(input$file1$datapath, ext, sheet=input$XLSsheets)}
      )
  #   }
  #   , warning = function(w) {
  #     shinyjs::info(conditionMessage(w))
  #   }, error = function(e) {
  #     shinyjs::info(paste("Read text file to convert",":",
  #                         conditionMessage(e), 
  #                         sep=" "))
  #   }, finally = {
  #     #cleanup-code 
  #   })
   }
  
})






output$conversionDone <- renderUI({
  rv$current.obj
  if (is.null(rv$current.obj)) { return(NULL)}
  
  h4("The conversion is done. Your dataset has been automatically loaded 
       in memory. Now, you can switch to the Descriptive statistics panel to 
       vizualize your data.")
  
})








#####-------------------------------------------------------
output$ManageXlsFiles <- renderUI({
  req(input$file1)
  
  .ext <- GetExtension(input$file1$name)
  if ((.ext == "xls") || (.ext == "xlsx")){ 
    sheets <- listSheets(input$file1$datapath)
    selectInput("XLSsheets", "sheets", choices = as.list(sheets),
                width='200px')
  }
  
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





# updateInputs <- function(id, n){
#     for (i in seq_len(n)) {
#         updateSelectInput(paste0(id,i),label=NULL,selected = input[[paste0(id,i)]])
#     }
# }







output$checkIdentificationTab <- renderUI({
  req(input$selectIdent)
  if (!isTRUE(input$selectIdent)){return(NULL)}
  
  shinyValue("colForOriginValue_",length(input$eData.box))
    temp <- shinyValue("colForOriginValue_",length(input$eData.box))
    
    if ((length(which(temp == "None")) == length(temp)))
      {
        img <- "images/Ok.png"
        txt <- "Correct"
      }  else {
        if (length(which(temp == "None")) > 0)
          {
            img <- "images/Problem.png"
            txt <- "The identification method is not appropriately defined for each sample."
          } else {
            if(length(temp) != length(unique(temp))){
                img <- "images/Problem.png"
                txt <- "There are duplicates in identification columns."
            }else { 
              img <- "images/Ok.png"
              txt <- "Correct"
              }
          }
      }
      tags$div(
         tags$div(
              tags$div(style="display:inline-block;",tags$img(src = img, height=25)),
              tags$div(style="display:inline-block;",tags$p(txt))
            )
          )
   

})



# reactive dataset
quantiDataTable <- reactive({
    req(input$eData.box)
    req(rv$tab1)
    
    session$sendCustomMessage('unbind-DT', 'x1')
    df <- NULL
    choices <- c("None",colnames(rv$tab1))
    names(choices) <- c("None",colnames(rv$tab1))
    
    if (isTRUE(input$selectIdent)) {
        
        df <- data.frame(as.data.frame(input$eData.box),
                         shinyInput(selectInput,
                                    "colForOriginValue_",
                                    nrow(as.data.frame(input$eData.box)),
                                    choices=choices))
        colnames(df) <- c("Sample", "Identification method")
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
    extensions = c('Scroller', 'Buttons'),
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
        dom = 'Bfrtip',
        autoWidth=TRUE,
        deferRender = TRUE,
        bLengthChange = FALSE,
        scrollX = 200,
        scrollY = 500,
        scroller = TRUE,
        ajax = list(url = dataTableAjax(session, quantiDataTable()))
        
        )
    
    )


observeEvent(shinyValue("colForOriginValue_",nrow(quantiDataTable())),{})


checkIdentificationMethod_Ok <- reactive({
  #req(input$selectIdent)
  res <- TRUE
  tmp <- NULL
  if (isTRUE(input$selectIdent)) {
    tmp <- shinyValue("colForOriginValue_",nrow(quantiDataTable()))
    if ((length(grep("None", tmp)) > 0)  || (sum(is.na(tmp)) > 0)){ res <- FALSE }
  } 
  res
  
})


datasetID_Ok <- reactive({
  req(input$idBox)
  req(rv$tab1)
  if (input$idBox == "Auto ID") {t <- TRUE}
  else {
  t <- (length(as.data.frame(rv$tab1)[, input$idBox])
        == length(unique(as.data.frame(rv$tab1)[, input$idBox])))
  }
  t
})




output$warningCreateMSnset <- renderUI({
    if (isTRUE(input$selectIdent)){
        colNamesForOriginofValues <- shinyValue("colForOriginValue_",nrow(quantiDataTable()))
        if (length(which(colNamesForOriginofValues == "None")) >0){
            text <- "<font color=\"red\"> Warning: The MSnset cannot be created because the identification 
            method are not fully filled.  <br>"
            HTML(text)
        }
    }
})






#######################################
observeEvent(input$createMSnsetButton,ignoreInit =  TRUE,{
    # if(is.null(input$createMSnsetButton) || (input$createMSnsetButton == 0)) 
    #{return(NULL)}
    
    colNamesForOriginofValues <- NULL
    if (isTRUE(input$selectIdent)) {
        colNamesForOriginofValues <- shinyValue("colForOriginValue_",nrow(quantiDataTable()))
        if (length(which(colNamesForOriginofValues == "None")) >0){ return (NULL)   }
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
                if (input$idBox !="Auto ID") {
                    indexForIDBox <- match(input$idBox, colnames(rv$tab1))
                }
                
                
                metadata <- hot_to_r(input$hot)
                logData <- (input$checkDataLogged == "no")
                
                
                indexForOriginOfValue <- NULL
                if (!is.null(colNamesForOriginofValues) && (length(grep("None", colNamesForOriginofValues))==0)  && (sum(is.na(colNamesForOriginofValues)) == 0)){
                    for (i in 1:length(tmp.eData.box)){
                        indexForOriginOfValue <- c(indexForOriginOfValue, which(colnames(rv$tab1) == input[[paste0("colForOriginValue_", i)]]))
                    }
                }
                
                
                versions <- list(Prostar_Version = 
                                   installed.packages(lib.loc = Prostar.loc)["Prostar","Version"],
                                 DAPAR_Version = 
                                   installed.packages(lib.loc = DAPAR.loc)["DAPAR","Version"]
                )
                tmp <- createMSnset(rv$tab1, 
                                    metadata, 
                                    indexForEData, 
                                    indexForFData, 
                                    indexForIDBox,
                                    indexForOriginOfValue,
                                    logData, 
                                    input$replaceAllZeros,
                                    pep_prot_data = input$typeOfData,
                                    proteinId =  gsub(".", "_", input$convert_proteinId, fixed=TRUE),
                                    versions
                )
                ClearUI()
                ClearMemory()
                rv$current.obj <- tmp
                
                rv$current.obj.name <- input$filenameToCreate
                rv$indexNA <- which(is.na(exprs(rv$current.obj)))
                
                l.params <- list(filename = input$filenameToCreate)
                 
                loadObjectInMemoryFromConverter()
                
                updateTabsetPanel(session, "tabImport", selected = "Convert")
                rv$pageConvert <- 5
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





