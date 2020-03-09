

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


callModule(modulePopover,"modulePopover_sepProteinID", 
           data = reactive(list(title = HTML(paste0("<strong><font size=\"3\">Delimiter in Protein IDs</font></strong>")), 
                                content="Select one (\" \", \".\", \",\", \";\", \"-\") separator.")))


callModule(moduleStaticDataTable,"overview_convertData", table2show=reactive({GetDatasetOverview()}))



observeEvent(input$selectIdent,{ rv.convert$selectIdent <- input$selectIdent})
observeEvent(input$convert_proteinId,{ rv.convert$convert_proteinId <- input$convert_proteinId})
observeEvent(input$idBox,{ rv.convert$idBox <- input$idBox})
observeEvent(input$eData.box,{ rv.convert$eDatabox <- input$eData.box})
observeEvent(input$typeOfData,{ rv.convert$typeOfData <- input$typeOfData})
observeEvent(input$checkDataLogged,{ rv.convert$checkDataLogged <- input$checkDataLogged})
observeEvent(input$replaceAllZeros,{ rv.convert$replaceAllZeros <- input$replaceAllZeros})
observeEvent(input$convert_reorder,{ rv.convert$convert_reorder <- input$convert_reorder})
observeEvent(input$XLSsheets,{ rv.convert$XLSsheets <- input$XLSsheets})
observeEvent(input$noSepProteinID,{ rv.convert$noSepProteinID <- input$noSepProteinID})
observeEvent(input$sepProteinID,{ rv.convert$sepProteinID <- input$sepProteinID})
observeEvent(input$checkBoxRemoveOrphanPept,{ rv.convert$checkBoxRemoveOrphanPept <- input$checkBoxRemoveOrphanPept})
observeEvent(input$convert_confirmSep,{ rv.convert$convert_confirmSep <- input$convert_confirmSep})

observeEvent(input$file1,{  
  print("update de rv.convert$tab1")
  isolate({
    rv.convert$datafile <- input$file1
    })
})




rv.convert <- reactiveValues(
  extension = NULL,
  datafile = NULL,
  convert_confirmSep = NULL,
  selectIdent = NULL,
  convert_proteinId = NULL,
  idBox = NULL,
  eData.box = NULL,
  typeOfData = NULL,
  checkDataLogged = NULL,
  replaceAllZeros = NULL,
  convert_reorder = NULL,
  XLSsheets = NULL,
  noSepProteinID = NULL,
  sepProteinID = NULL,
  checkBoxRemoveOrphanPept = NULL,
  datafile = NULL,
  
  designChecked = NULL,
  conditionsChecked = NULL
  
)


###
### SCREEN 1
###
output$Convert_SelectFile <- renderUI({
  tagList(br(), br(),
          fluidRow(
            column(width=2, modulePopoverUI("modulePopover_convertChooseDatafile")),
            column(width = 10, fileInput("file1", rv.convert$datafile$name, 
                                         multiple=FALSE, 
                                         accept=c(".txt", ".tsv", ".csv",".xls", ".xlsx"))
                   )
            ),
          #actionButton("loadData2Convert", "Load data file",class = actionBtnClass),
          uiOutput("ManageXlsFiles"),
          # helpText("Hint : before importing quantification 
          #             file data, check the syntax of your text 
          #             file."),
          br(),
          uiOutput("ConvertOptions")
  )
})




###
### SCREEN 2
###
output$Convert_DataId <- renderUI({
  
  tagList(
    
    br(), br(),
    #uiOutput("helpTextDataID"),
    
    tags$div(
      tags$div( style="display:inline-block; vertical-align: top; padding-right: 100px;",
                uiOutput("id"),
                uiOutput("warningNonUniqueID")
      ),
      tags$div( style="display:inline-block; vertical-align: top;",
                uiOutput("convertChooseProteinID_UI"),
                uiOutput("previewProteinID_UI")
      )
    )
  )
})


###
### SCREEN 3
###
output$Convert_ExpFeatData <- renderUI({
 
    fluidRow(
      column(width=4,uiOutput("eData",width = "400px")),
      column(width=8,
             tagList(
               uiOutput("checkIdentificationTab"),
               checkboxInput("selectIdent", 
                             "Select columns for identification method", 
                             value = rv.convert$selectIdent),
               tags$script(HTML("Shiny.addCustomMessageHandler('unbind-DT', function(id) {
                                   Shiny.unbindAll($('#'+id).find('table').DataTable().table().node());
                                   })")),
               uiOutput("showX1", width='500px')
               )
     
    )
    )
})


###
### SCREEN 5
###
output$convertFinalStep <- renderUI({
  req(rv$designChecked)
  if (!(rv$designChecked$valid)){return(NULL)}
  tagList(
    uiOutput("checkAll_convert", width="50"),
    htmlOutput("msgAlertCreateMSnset"),
    hr(),
    textInput("filenameToCreate","Enter the name of the study"),
    actionButton("createMSnsetButton","Convert data", class = actionBtnClass),
    uiOutput("warningCreateMSnset")
    
  )
})





output$Convert_BuildDesign <- renderUI({
  req(rv.convert$datafile)
  
  tagList(
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
    ),
    hr(),
    selectInput("convert_reorder", "Order by conditions ?",
                selected=rv.convert_reorder,
                choices=c("No"="No", "Yes"="Yes"),
                width="100px"),
    tags$div(
      
      tags$div(style="display:inline-block; vertical-align: top;",
               uiOutput("viewDesign",width="100%")
      ),
      tags$div(style="display:inline-block; vertical-align: top;",
               shinyjs::hidden(div(id = "showExamples", uiOutput("designExamples") ))
      )
    )
    
  )
  
})




output$Convert_Convert <- renderUI({
  #if (rv$pageConvert != 5){return()}
  
  tagList(
    br(), br(),
    
    uiOutput("convertFinalStep"),
    moduleStaticDataTableUI("overview_convertData"),
    uiOutput("conversionDone"),
    p("Once the 'Load' button (above) clicked, you will be automatically redirected to Prostar home page. The dataset will be accessible within Prostar 
    interface and processing menus will be enabled. However, all importing functions ('Open MSnset', 'Demo data' and 'Convert data') will be disabled 
    (because successive dataset loading can make Prostar unstable). To work on another dataset, use first the 'Reload Prostar' functionality from 
    the 'Dataset manager' menu: it will make Prostar restart with a fresh R session where import functions are enabled.")
    
  )
})

output$warningNonUniqueID <- renderUI({
  req(rv.convert$idBox)
  req(rv.convert$tab1)
  
  isolate({
    if (rv.convert$idBox =="Auto ID") {
    text <- "<img src=\"images/Ok.png\" height=\"24\"></img>" }
  else {
  t <- (length(as.data.frame(rv.convert$tab1)[, rv.convert$idBox])
        == length(unique(as.data.frame(rv.convert$tab1)[, rv.convert$idBox])))
  
  if (!t){
    text <- "<img src=\"images/Problem.png\" height=\"24\"></img><font color=\"red\">
        Warning ! Your ID contains duplicate data.
        Please choose another one."
    
  }
  else {
    text <- "<img src=\"images/Ok.png\" height=\"24\"></img>"
  }
  }
  HTML(text)
  })
})




output$convertChooseProteinID_UI <- renderUI({
  req(rv.convert$tab1)
  
  .choices <- c("",colnames(rv.convert$tab1))
  names(.choices) <- c("",colnames(rv.convert$tab1))
  if (rv.convert$typeOfData == "protein") {
    txt <- "Select the column containing the unique ID of the proteins."
    title <-paste0("<strong><font size=\"4\">Select unique ID of the proteins</font></strong>")
  } else if (rv.convert$typeOfData == "peptide"){
    txt <- "Select the column containing the parent protein IDs."
    title <- paste0("<strong><font size=\"4\">Select IDs of the parent proteins</font></strong>")
  }
  callModule(modulePopover,"modulePopover_convertProteinID", 
             data = reactive(list(title = HTML(title), 
                                  content=txt)))
  
  tagList(
    modulePopoverUI("modulePopover_convertProteinID"),
    selectInput("convert_proteinId","",choices =  .choices , selected = rv.convert$convert_proteinId )
  )
})



#########################################################
output$id <- renderUI({
  print(rv.convert$tab1)
  #req(rv.convert$tab1)
  
  .choices <- c("Auto ID",colnames(rv.convert$tab1))
  names(.choices) <- c("Auto ID",colnames(rv.convert$tab1))
  
  tagList(
    modulePopoverUI("modulePopover_convertIdType"),
    selectInput("idBox", label = "", 
                selected = rv.convert$idBox,
                choices = .choices)
  )
  
})



output$ConvertOptions <- renderUI({
  req(rv.convert$datafile)
  
  tagList(
    radioButtons("typeOfData", 
                 "Is it a peptide or protein dataset ?", 
                 choices=c("peptide dataset" = "peptide", 
                           "protein dataset" = "protein"),
                 selected = rv.convert$typeOfData
    )
    
    ,radioButtons("checkDataLogged", 
                  "Are your data already log-transformed ?", 
                  #width = widthWellPanel, 
                  choices=c("yes (they stay unchanged)" = "yes", 
                            "no (they wil be automatically transformed)"="no"), 
                  selected=rv.convert$checkDataLogged)
    ,br()
    ,checkboxInput("replaceAllZeros", 
                   "Replace all 0 and NaN by NA", 
                   value= rv.convert$replaceAllZeros)
  )
})


observeEvent(rv.convert$fData.box,ignoreInit = TRUE,{
  
  choices = colnames(rv.convert$tab1)[-which(colnames(rv.convert$tab1) %in% rv.convert$fData.box)]
  names(choices) = 
    colnames(rv.convert$tab1)[-which(colnames(rv.convert$tab1) %in% rv.convert$fData.box)]
  updateSelectInput(session, "eData.box", 
                    label = "",
                    choices = choices,
                    selected = rv.convert$eDatabox)
  
})



output$showX1 <- renderUI({
  req(rv.convert$eDatabox)
  req(rv.convert$selectIdent)
  
  if (length(rv.convert$eDatabox) == 0 || !isTRUE(rv.convert$selectIdent)){
    return(NULL)
  }
  DT::dataTableOutput("x1", width='500px')
})


output$helpTextDataID <- renderUI({
  req(rv.convert$typeOfData)

  t <- ""
  switch(rv.convert$typeOfData,
         protein = {t <- "proteins"},
         peptide = {t <- "peptides"}
  )
  txt <- paste ("Please select among the columns of your data the one that 
                corresponds to a unique ID of the ", t, ".", sep=" ")
  helpText(txt)
  
})




observeEvent(req(rv.convert$datafile), {
  authorizedExts <- c("txt", "csv", "tsv", "xls", "xlsx")
  .ext <- strsplit(rv.convert$datafile$name, '.', fixed=TRUE)[[1]][2]
  if( is.na(match(.ext, authorizedExts))) {
    shinyjs::info("Warning : this file is not a text nor an Excel file !
                   Please choose another one.")
    return(NULL)
  } else {
    rv.convert$extension <- .ext
  }
})


############ Read text file to be imported ######################
observeEvent(rv.convert$datafile,ignoreInit = TRUE,{
  
  # rv.convert$XLSsheets
  if (rv.convert$extension %in% c("xls", "xlsx") && is.null(rv.convert$XLSsheets)) {
    return(NULL)
    }

  authorizedExts <- c("txt","csv", "tsv","xls","xlsx")
  if( is.na(match(rv.convert$extension, authorizedExts))) {
    shinyjs::info("Warning : this file is not a text nor an Excel file !
                  Please choose another one.")
  }
  print(rv.convert$datafile)
 
   tryCatch(
     {
    #shinyjs::disable("file1")
       isolate({
         switch(rv.convert$extension,
           txt = { rv.convert$tab1 <- read.csv(rv.convert$datafile$datapath,  header=TRUE, sep="\t", as.is=T)},
           csv = { rv.convert$tab1 <- read.csv(rv.convert$datafile$datapath,  header=TRUE, sep="\t", as.is=T)},
           tsv = { rv.convert$tab1 <- read.csv(rv.convert$datafile$datapath,  header=TRUE, sep="\t", as.is=T)},
           xls = { rv.convert$tab1 <- readExcel(rv.convert$datafile$datapath, rv.convert$extension, sheet=rv.convert$XLSsheets)},
           xlsx = {rv.convert$tab1 <- readExcel(rv.convert$datafile$datapath, rv.convert$extension, sheet=rv.convert$XLSsheets)}
    )
      })
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
  
  #shinyjs::disable('file1')
  

  print(rv.convert$tab1)
})






output$conversionDone <- renderUI({
  req(rv$current.obj)

  h4("The conversion is done. Your dataset has been automatically loaded 
       in memory. Now, you can switch to the Descriptive statistics panel to 
       vizualize your data.")
  
})








#####-------------------------------------------------------
output$ManageXlsFiles <- renderUI({
  req(rv.convert$datafile)
  
  if (rv.convert$extension %in% c("xls","xlsx")){ 
    sheets <- listSheets(rv.convert$datafile$datapath)
    selectInput("XLSsheets", "sheets", choices = as.list(sheets),
                selected=rv.convert$XLSsheets,
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
  req(rv.convert$tab1)
  
  choices <- colnames(rv.convert$tab1)
  names(choices) <- colnames(rv.convert$tab1)
  
  tagList(
    modulePopoverUI("modulePopover_convertDataQuanti"),
    selectInput("eData.box",
                label = "",
                choices = choices,
                selected = rv.convert$eDatabox,
                multiple = TRUE, 
                width='200px',
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
  req(rv.convert$selectIdent)
  req(rv.convert$eDatabox)
  
  shinyValue("colForOriginValue_",length(rv.convert$eData.box))
  temp <- shinyValue("colForOriginValue_",length(rv.convert$eData.box))
  
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
  req(rv.convert$eData.box)
  req(rv.convert$tab1)
  
  session$sendCustomMessage('unbind-DT', 'x1')
  df <- NULL
  choices <- c("None",colnames(rv.convert$tab1))
  names(choices) <- c("None",colnames(rv.convert$tab1))
  
  if (isTRUE(rv.convert$selectIdent)) {
    
    df <- data.frame(as.data.frame(rv.convert$eData.box),
                     shinyInput(selectInput,
                                "colForOriginValue_",
                                nrow(as.data.frame(rv.convert$eData.box)),
                                choices=choices))
    colnames(df) <- c("Sample", "Identification method")
  } else {
    df <- data.frame(Sample = as.data.frame(rv.convert$eData.box))
    colnames(df) <- c("Sample")
  }
  df
})


observeEvent(shinyValue("colForOriginValue_",nrow(quantiDataTable())),{})


observeEvent(req(rv.convert$eDatabox), {
  print(rv.convert$eDatabox)
  shinyjs::toggle('selectIdent', condition= length(rv.convert$eDatabox)>0)
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
  #req(rv.convert$selectIdent)
  res <- TRUE
  tmp <- NULL
  if (isTRUE(rv.convert$selectIdent)) {
    tmp <- shinyValue("colForOriginValue_",nrow(quantiDataTable()))
    if ((length(grep("None", tmp)) > 0)  || (sum(is.na(tmp)) > 0)){ res <- FALSE }
  } 
  res
  
})


datasetID_Ok <- reactive({
  req(rv.convert$idBox)
  req(rv.convert$tab1)
  test1 <- test2 <- TRUE
  
  if(rv.convert$typeOfData == "peptide"){
    test1 <- !(rv.convert$convert_proteinId == "") && !is.null(rv.convert$convert_proteinId) && isTRUE(rv.convert$convert_confirmSep)
    
  }
  
  if (rv.convert$idBox =="Auto ID") {
    test2 <- TRUE
  }
  else {
    test2 <- (length(as.data.frame(rv.convert$tab1)[, rv.convert$idBox])
              == length(unique(as.data.frame(rv.convert$tab1)[, rv.convert$idBox])))
  }
  
 
  t <-  test1 && test2
  
  t
})



output$previewProteinID_UI <- renderUI({
  req(rv.convert$convert_proteinId)
  
  tagList(
    p(style="color: black;", 'Preview'),
    tableOutput("previewProtID")
  )
  
})



output$previewProtID <- renderTable(
  req(rv.convert$convert_proteinId),
  head(rv.convert$tab1[,rv.convert$convert_proteinId]),
  colnames = FALSE
)




output$warningCreateMSnset <- renderUI({
  if (isTRUE(rv.convert$selectIdent)){
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
  allDone <- sum(rvModProcess$moduleConvertDone[1:4]) 
  if (allDone !=4){
    shinyjs::info('At least one of the previous step has not been done.')
    return(NULL)
  }
  
  colNamesForOriginofValues <- NULL
  if (isTRUE(rv.convert$selectIdent)) {
    colNamesForOriginofValues <- shinyValue("colForOriginValue_",nrow(quantiDataTable()))
    if (length(which(colNamesForOriginofValues == "None")) >0){ return (NULL)   }
  } 
  
 
    result = tryCatch(
      {
        isolate({
          
        tmp.eData.box <- rv.convert$eData.box
        indexForEData <- match(tmp.eData.box, colnames(rv.convert$tab1))
        if (!is.null(rv.convert$newOrder)){
          tmp.eData.box <- tmp.eData.box[rv.convert$newOrder]
          indexForEData <- indexForEData[rv.convert$newOrder]
        }
        
        indexForFData <- seq(1,ncol(rv.convert$tab1))[-indexForEData]
        
        indexForIDBox <- NULL
        if (rv.convert$idBox !="Auto ID") {
          indexForIDBox <- match(rv.convert$idBox, colnames(rv.convert$tab1))
        }
        
        
        metadata <- hot_to_r(input$hot)
        logData <- (rv.convert$checkDataLogged == "no")
        
        
        indexForOriginOfValue <- NULL
        if (!is.null(colNamesForOriginofValues) && (length(grep("None", colNamesForOriginofValues))==0)  && (sum(is.na(colNamesForOriginofValues)) == 0)){
          for (i in 1:length(tmp.eData.box)){
            indexForOriginOfValue <- c(indexForOriginOfValue, which(colnames(rv.convert$tab1) == input[[paste0("colForOriginValue_", i)]]))
          }
        }
        
        
        versions <- list(Prostar_Version = 
                           installed.packages(lib.loc = Prostar.loc)["Prostar","Version"],
                         DAPAR_Version = 
                           installed.packages(lib.loc = DAPAR.loc)["DAPAR","Version"]
        )
        options(digits=15)
        rv$current.obj  <- DAPAR::createMSnset(rv.convert$tab1, 
                                   metadata, 
                                   indexForEData, 
                                   indexForFData, 
                                   indexForIDBox,
                                   indexForOriginOfValue,
                                   (rv.convert$checkDataLogged == "no"), 
                                   rv.convert$replaceAllZeros,
                                   pep_prot_data = rv.convert$typeOfData,
                                   proteinId =  gsub(".", "_", rv.convert$convert_proteinId, fixed=TRUE),
                                   versions
        )
        
        rv$current.obj.name <- input$filenameToCreate
        rv$indexNA <- which(is.na(exprs(rv$current.obj)))
        
        rv$typeOfDataset <- rv.convert$typeOfData
        rv$current.obj <- addOriginOfValue(rv$current.obj)
        colnames(fData(rv$current.obj)) <- gsub(".", "_", colnames(fData(rv$current.obj)), fixed=TRUE)
        names(rv$current.obj@experimentData@other) <- gsub(".", "_", names(rv$current.obj@experimentData@other), fixed=TRUE)
        rv$current.obj <- addOriginOfValue(rv$current.obj)
        
        rv$widgets$aggregation$proteinId <- rv$current.obj@experimentData@other$proteinId
        rv$proteinId <- rv$current.obj@experimentData@other$proteinId
        
        loadObjectInMemoryFromConverter()
        
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




checkSep <- function(sepUser){
  inputUser <- sepUser
  separators <- c(' ', '.', ",", ";", "-",'')
  if (length(which(inputUser == separators))>=1) {
    separators <- separators[-which(inputUser == separators)]
  }
  
  sepToCheck <- character()
  for (i in separators) {
    sepToCheck <- paste0( sepToCheck, gsub('"',"",i),"|" )
  }
  sepToCheck <- substr(sepToCheck,1,nchar(sepToCheck)-1)
  sepToCheck <- gsub("\\.", "\\\\.", sepToCheck)
  liste <- sapply(rv.convert$tab1[,rv.convert$convert_proteinId], function(x) strsplit(x, sepToCheck))
  subliste <- liste[lengths(liste)>1]
  listeSepPLus <- c()
  for (i in 1:length(subliste)){
    listeSepPLus <- c(listeSepPLus, intersect(unlist(strsplit(separators,"")),unlist(strsplit(names(subliste)[i],""))))
  }
  
  if (length(listeSepPLus)>0) {
    text <- paste0("<img src=\"images/Problem.png\" height=\"24\"></img><font color=\"red\">Others separators (",c(unlist(unique(listeSepPLus))), ") found!")
  }
  else { 
    text <- "<img src=\"images/Ok.png\" height=\"24\"></img> No others separators detected."
  }
  return(text)
}


