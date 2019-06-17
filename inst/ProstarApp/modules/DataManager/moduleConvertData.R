source(file.path(".", "modules/moduleNavigation.R"),  local = TRUE)$value
source(file.path(".", "modules/moduleNavigation2.R"),  local = TRUE)$value
#source(file.path(".", "modules/DataManager/srv_BuildDesign.R"),  local = TRUE)$value

moduleConvertDataUI <- function(id){
  ns <- NS(id)
  tagList(
    br(),br(),br(),
    uiOutput(ns('bars')),
    hr(),
    uiOutput(ns('screens'))
  )
}





moduleConvertData <- function(input, output, session){
  ns <- session$ns
  #source(file.path(".", "srv_BuildDesign.R"),  local = TRUE)$value
  source(file.path(".", "modules/DataManager/srv_BuildDesign.R"),  local = TRUE)$value
  
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
  
   callModule(moduleStaticDataTable,"overview_convertData", 
             table2show=reactive({
               req(rv.convert$dataOut)
               GetDatasetOverview2(rv.convert$dataOut@datasets[[1]])
               }))
  
   callModule(moduleInfoDataset, "infoAboutMSnset",
              obj = reactive({
                req(rv.convert$dataOut)
                rv.convert$dataOut@datasets[[1]]
              }))
  
  
  
  ###### definition of RV for navigation process
  rvNavProcess <- reactiveValues(
    Done = rep(FALSE,5),
    def = list(name = "Convert",
               stepsNames = c("Select file", "Data Id", "Epx. & feat. data", "Build design", "Convert"),
               isMandatory = rep(TRUE,5),
               ll.UI = list( screenStep1 = uiOutput(ns("Convert_SelectFile")),
                             screenStep2 = uiOutput(ns("Convert_DataId")),
                             screenStep3 = uiOutput(ns("Convert_ExpFeatData")),
                             screenStep4 = uiOutput(ns("Convert_BuildDesign")),
                             screenStep5 = uiOutput(ns("Convert_Convert"))
               ),
               rstFunc = reactive({resetModuleConvert()}))
  )
  
 
  
  observe({
        rv.convert$nav2 <- callModule(moduleNavigation2, "moduleProcess_Convert", 
                               isDone = reactive({rvNavProcess$Done}), 
                               pages = reactive({rvNavProcess$def}),
                               rstFunc = resetModuleConvert,
                               type = reactive({'bubble'}))
      })

  
  ### Definition of rv for actual module
  rv.convert <- reactiveValues(
    tab1 = NULL,
    obj =  NULL,
    dataOut = NULL, 
    name = "processConvert",
    nav2 = NULL)
  
################################################
  
  

resetModuleConvert<- reactive({  
  ## update widgets values (reactive values)
  resetModuleProcess("Convert")
  
  ## update widgets in UI
  updateCheckboxInput(session,"selectIdent", value = FALSE)
  updateSelectInput(session,"convert_proteinId",selected = character(0))
  updateSelectInput(session,"idBox", selected = "Auto ID")
  updateRadioButtons(session, "typeOfData", selected="peptide")
  updateRadioButtons(session, "checkDataLogged", selected="no")
  updateCheckboxInput(session,"replaceAllZeros", value= TRUE)
  
  rvNavProcess$Done = rep(FALSE, 5)
})



  output$bars <- renderUI({
    rv.convert$nav2()$bars
  })
  
  
  output$screens <- renderUI({
    rv.convert$nav2()$screens
  })

#################################
### Screen 1
output$Convert_SelectFile <- renderUI({
  tagList(
          fluidRow(
            column(width=2, modulePopoverUI(ns("modulePopover_convertChooseDatafile"))),
            column(width = 10, fileInput(ns("file2Convert"), "", 
                                         multiple=FALSE, 
                                         accept=c(".txt", ".tsv", ".csv",".xls", ".xlsx")))),
          uiOutput(ns("ManageXlsFiles")),
          # helpText("Hint : before importing quantification 
          #             file data, check the syntax of your text 
          #             file."),
          br(),
          uiOutput(ns("ConvertOptions"))
  )
})

#################################
### Screen 2
output$Convert_DataId <- renderUI({
  
  tagList(
    #uiOutput("helpTextDataID"),
    
    tags$div(
      tags$div( style="display:inline-block; vertical-align: top; padding-right: 100px;",
                uiOutput(ns("id")),
                uiOutput(ns("warningNonUniqueID"))
      ),
      tags$div( style="display:inline-block; vertical-align: top;",
                uiOutput(ns("convertChooseProteinID_UI"))
      )
    )
  )
})



#################################
### Screen 3
output$Convert_ExpFeatData <- renderUI({
  
  tagList(
    fluidRow(
      column(width=4,checkboxInput(ns("selectIdent"), 
                                   "Select columns for identification method", 
                                   value = FALSE)),
      column(width=4,uiOutput(ns("checkIdentificationTab")))
    ),
    fluidRow(
      column(width=4,uiOutput(ns("eData"),width = "400px")),
      column(width=8,DT::dataTableOutput(ns("x1"), width='500px'))),
    tags$script(HTML("Shiny.addCustomMessageHandler('unbind-DT', function(id) {
                     Shiny.unbindAll($('#'+id).find('table').DataTable().table().node());
})"))
                           )
  })




#################################
### Screen 4 : Build design
output$Convert_BuildDesign <- renderUI({
  
  tagList(
    tags$p("If you do not know how to fill the experimental design, you can click
           on the '?' next to each design in the list that appear once the conditions 
           are checked or got to the ", 
           actionLink(ns("linkToFaq1"), "FAQ",style="background-color: white"), 
           " page."),
    fluidRow(
      column(width=6,tags$b("1 - Fill the \"Condition\" column to identify the conditions to compare.")),
      column(width=6,uiOutput(ns("UI_checkConditions"))  )
    ),
    fluidRow(
      column(width=6,uiOutput(ns("UI_hierarchicalExp"))),
      column(width=6,uiOutput(ns("checkDesign") ))
    ),
  hr(),
  selectInput(ns("convert_reorder"), "Order by conditions ?",
              choices=c("No"="No", "Yes"="Yes"),
              width="100px"),
  tags$div(
    
    tags$div(style="display:inline-block; vertical-align: top;",
             uiOutput(ns("viewDesign"),width="100%")
    ),
    tags$div(style="display:inline-block; vertical-align: top;",
             #shinyjs::hidden(div(id = ns("showExamples"), uiOutput("designExamples") ))
             shinyjs::hidden(uiOutput(ns("designExamples")))
    )
  )
)

  
})





#################################
### Screen 5
output$Convert_Convert <- renderUI({
  tagList(
    uiOutput(ns("convertFinalStep")),
    moduleStaticDataTableUI(ns("overview_convertData")),
    uiOutput(ns("conversionDone")),
    moduleInfoDatasetUI(ns("infoAboutMSnset"))
    
  )
})


############################################################################
####### ENd definitino of UI   ##################
#############################################################################

observe({
  req(input$idBox)
  req(rv.convert$tab1)
  test1 <- test2 <- FALSE
  
  test1 <- (input$typeOfData == "peptide") && !(input$convert_proteinId == "")
  
  
  if (input$idBox =="Auto ID") {
    test2 <- TRUE
  }
  else {
    test2 <- (length(as.data.frame(rv.convert$tab1)[, input$idBox])
              == length(unique(as.data.frame(rv.convert$tab1)[, input$idBox])))
  }
  
  rvNavProcess$Done[2] <- test1 && test2
})



output$warningNonUniqueID <- renderUI({
  req(input$idBox)
  req(rv.convert$tab1)
  
  isolate({
    if (input$idBox =="Auto ID") {
      text <- "<img src=\"images/Ok.png\" height=\"24\"></img>"
    }
    else {
      t <- (length(as.data.frame(rv.convert$tab1)[, input$idBox])
            == length(unique(as.data.frame(rv.convert$tab1)[, input$idBox])))
      
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
  
  if (input$typeOfData == "protein") {return(NULL)}
  
  .choices <- c("",colnames(rv.convert$tab1))
  names(.choices) <- c("",colnames(rv.convert$tab1))
  tagList(
    modulePopoverUI(ns("modulePopover_convertProteinID")),
    selectInput(ns("convert_proteinId"),"",choices =  .choices , selected = character(0))
  )
})


#########################################################
output$id <- renderUI({
  req(rv.convert$tab1)
  
  .choices <- c("Auto ID",colnames(rv.convert$tab1))
  names(.choices) <- c("Auto ID",colnames(rv.convert$tab1))
  
  tagList(
    modulePopoverUI(ns("modulePopover_convertIdType")),
    selectInput(ns("idBox"), label = "", choices = .choices)
  )
  
})



output$ConvertOptions <- renderUI({
  input$file2Convert
  if (is.null(input$file2Convert)){return(NULL)}
  
  tagList(
    radioButtons(ns("typeOfData"), 
                 "Choose the pipeline", 
                 choices=c("peptide" = "peptide", 
                           "protein" = "protein",
                           "peptide to protein (p2p)" = "p2p")
    )
    
    ,radioButtons(ns("checkDataLogged"), 
                  "Are your data already log-transformed ?", 
                  #width = widthWellPanel, 
                  choices=c("yes (they stay unchanged)" = "yes", 
                            "no (they wil be automatically transformed)"="no"), 
                  selected="no")
    ,br()
    ,checkboxInput(ns("replaceAllZeros"), 
                   "Replace all 0 and NaN by NA", 
                   value= TRUE)
  )
})


observeEvent(input$fData.box,ignoreInit = TRUE,{
  
  choices = colnames(rv.convert$tab1)[-which(colnames(rv.convert$tab1) %in% input$fData.box)]
  names(choices) = 
    colnames(rv.convert$tab1)[-which(colnames(rv.convert$tab1) %in% input$fData.box)]
  updateSelectInput(session, "eData.box", 
                    label = "",
                    choices = choices,
                    selected = choices)
  
})




output$helpTextDataID <- renderUI({
  req(input$typeOfData)

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
  rv.convert$tab1 <- read.csv(input$file2Convert$datapath,  header=TRUE, sep="\t", as.is=T)
})


############ Read text file to be imported ######################
observeEvent(c(input$file2Convert,input$XLSsheets),{
  
  input$XLSsheets
  if (((GetExtension(input$file2Convert$name)== "xls")
       || (GetExtension(input$file2Convert$name) == "xlsx") )
      && is.null(input$XLSsheets)) {return(NULL)  }
  
  authorizedExts <- c("txt", "csv", "tsv", "xls", "xlsx")
  if( is.na(match(GetExtension(input$file2Convert$name), authorizedExts))) {
    shinyjs::info("Warning : this file is not a text nor an Excel file ! 
                  Please choose another one.")
  }
  else {
    # result = tryCatch(
    #   {
    #ClearUI()
   # ClearMemory()
    ext <- GetExtension(input$file2Convert$name)
    
    switch(ext,
           txt = { rv.convert$tab1 <- read.csv(input$file2Convert$datapath,  header=TRUE, sep="\t", as.is=T)},
           csv = { rv.convert$tab1 <- read.csv(input$file2Convert$datapath,  header=TRUE, sep="\t", as.is=T)},
           tsv = { rv.convert$tab1 <- read.csv(input$file2Convert$datapath,  header=TRUE, sep="\t", as.is=T)},
           xls = { rv.convert$tab1 <- readExcel(input$file2Convert$datapath, ext, sheet=input$XLSsheets)},
           xlsx = {rv.convert$tab1 <- readExcel(input$file2Convert$datapath, ext, sheet=input$XLSsheets)}
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
  req(rv.convert$obj)
  
  h4("The conversion is done. Your dataset has been automatically loaded 
     in memory. Now, you can switch to the Descriptive statistics panel to 
     vizualize your data.")
  
  
})



observe({
  rvNavProcess$Done[1] <- !is.null(input$file2Convert)
})




#####-------------------------------------------------------
output$ManageXlsFiles <- renderUI({
  req(input$file2Convert)
  
  .ext <- GetExtension(input$file2Convert$name)
  if ((.ext == "xls") || (.ext == "xlsx")){ 
    sheets <- listSheets(input$file2Convert$datapath)
    selectInput(ns("XLSsheets"), "sheets", choices = as.list(sheets),
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
  input$file2Convert
  rv.convert$tab1
  if (is.null(rv.convert$tab1)) {return(NULL)  }
  
  choices <- colnames(rv.convert$tab1)
  names(choices) <- colnames(rv.convert$tab1)
  
  tagList(
    modulePopoverUI(ns("modulePopover_convertDataQuanti")),
    selectInput(ns("eData.box"),
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



observe({
  rvNavProcess$Done[3] <- length(input$eData.box)>0
})


observe({
  rvNavProcess$Done[4] <- !is.null(rv.buildDesign$designChecked$valid) && isTRUE(rv.buildDesign$designChecked$valid)
})


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
  req(rv.convert$tab1)
  
  session$sendCustomMessage('unbind-DT', 'x1')
  df <- NULL
  choices <- c("None",colnames(rv.convert$tab1))
  names(choices) <- c("None",colnames(rv.convert$tab1))
  
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
  req(rv.convert$tab1)
  if (input$idBox == "Auto ID") {t <- TRUE}
  else {
    t <- (length(as.data.frame(rv.convert$tab1)[, input$idBox])
          == length(unique(as.data.frame(rv.convert$tab1)[, input$idBox])))
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
observeEvent(input$createMSnsetButton,{
  #req(rv.convert$obj)
  
  colNamesForOriginofValues <- NULL
  if (isTRUE(input$selectIdent)) {
    colNamesForOriginofValues <- shinyValue("colForOriginValue_",nrow(quantiDataTable()))
    if (length(which(colNamesForOriginofValues == "None")) >0){ return (NULL)   }
  } 
  
  #isolate({
  result = tryCatch(
    {
      ext <- GetExtension(input$file2Convert$name)
     
      
      input$filenameToCreate
      rv.convert$tab1
      
      tmp.eData.box <- input$eData.box
      indexForEData <- match(tmp.eData.box, colnames(rv.convert$tab1))
      if (!is.null(rv.buildDesign$newOrder)){
        tmp.eData.box <- tmp.eData.box[rv.buildDesign$newOrder]
        indexForEData <- indexForEData[rv.buildDesign$newOrder]
      }
      
      indexForFData <- seq(1,ncol(rv.convert$tab1))[-indexForEData]
      
      indexForIDBox <- NULL
      if (input$idBox !="Auto ID") {
        indexForIDBox <- match(input$idBox, colnames(rv.convert$tab1))
      }
      
      
      metadata <- hot_to_r(input$hot)
      logData <- (input$checkDataLogged == "no")
      
      
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
      tmp <- createMSnset(rv.convert$tab1, 
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
      
      
      ll.process <- type <- NULL
      switch(input$typeOfData,
             peptide = {
               rv.convert$obj <- pepPipeline()
               ll.process <- pipeline.def$peptide
             },
             protein = {
               rv.convert$obj <- protPipeline()
               ll.process <- pipeline.def$protein
               
             }, 
             p2p = {
               rv.convert$obj <- p2pPipeline()
               ll.process <- pipeline.def$p2p
               
             }
      )
      
      rv.convert$obj <- initialize(rv.convert$obj, 
                                   c('original',ll.process), 
                                            tmp,
                                            input$filenameToCreate, 
                                            input$typeOfData )
      
      
      
      print("New dataset converted")
      print(rv.convert$obj)
      ClearConvertUI()
      #ClearMemory()
      
      l.params <- list(filename = input$filenameToCreate)
      
      #loadObjectInMemoryFromConverter()
      
      updateTabsetPanel(session, "tabImport", selected = "Convert")
      rv.convert$dataOut <- rv.convert$obj
      
      rvNavProcess$Done[5] <- TRUE
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
  
  
  
  #})
})






###-------------------------------------------------------------------
ClearConvertUI <- reactive({
  
  updateSelectInput(session, 
                    "datasets",  
                    choices = G_noneStr)
  #updateRadioButtons(session,"typeOfData",selected = typePeptide )
  updateRadioButtons(session, "checkDataLogged", selected="no")
  
  updateSelectInput(session, "idBox", selected = NULL)
  
  updateSelectizeInput(session,"eData.box",choices = NULL, selected=NULL)
  updateTextInput(session,"filenameToCreate",value= "")
  updateTextInput(session,"nameExport",value= "")
  
  updateCheckboxInput(session, "replaceAllZeros",value = TRUE)
  updateRadioButtons(session,
                     inputId = "ChooseFilters", 
                     selected = gFilterNone)
  
})





#### srv_BuildDesign.R

source(file.path(".", "modules/DataManager/moduleDesignExample.R"),  local = TRUE)$value



rv.buildDesign <- reactiveValues(
  hot = NULL,
  designChecked = NULL,
  newOrder = NULL,
  conditionsChecked = NULL,
  designSaved = NULL
)



observeEvent(input$linkToFaq1, {
  updateTabsetPanel(session, 'navPage', "faqTab")
})




color_renderer <- reactive({
  conds <- rv.buildDesign$hot$Condition
  pal <- rv.prostar$settings()$examplePalette
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



output$convertFinalStep <- renderUI({
  req(rv.buildDesign$designChecked)
  if (!(rv.buildDesign$designChecked$valid)){return(NULL)}
  tagList(
    uiOutput(ns("checkAll_convert"), width="50"),
    htmlOutput("msgAlertCreateMSnset"),
    hr(),
    textInput(ns("filenameToCreate"),"Enter the name of the study"),
    actionButton(ns("createMSnsetButton"),"Convert data", class = actionBtnClass),
    uiOutput(ns("warningCreateMSnset"))
    
  )
})

#----------------------------------------------------------
observeEvent(input$btn_checkConds,{
  input$convert_reorder
  
  if (length(grep("Bio.Rep", colnames(rv.buildDesign$hot))) > 0)  { return(NULL)}
  
  if (isTRUE(input$convert_reorder)) {
    rv.buildDesign$newOrder <- order(rv.buildDesign$hot["Condition"])
    rv.buildDesign$hot <- rv.buildDesign$hot[rv.buildDesign$newOrder,]
  }
  
  rv.buildDesign$conditionsChecked <- DAPAR::check.conditions(rv.buildDesign$hot$Condition)
  
})



#----------------------------------------------------------
observeEvent(input$eData.box,{
  rv.buildDesign$hot  <- data.frame(Sample.name = as.character(input$eData.box),
                                    Condition = rep("",length(input$eData.box)),
                                    stringsAsFactors = FALSE)
  
  
})

#-------------------------------------------------------------
output$hot <- renderRHandsontable({
  rv.buildDesign$hot
  input$chooseExpDesign
  
  if (is.null(rv.buildDesign$hot)){
    rv.buildDesign$hot  <- data.frame(Sample.name = as.character(input$eData.box),
                                      Condition = rep("",length(input$eData.box)),
                                      stringsAsFactors = FALSE)
  }
  
  hot <- rhandsontable::rhandsontable(rv.buildDesign$hot,rowHeaders=NULL, 
                                      fillHandle = list(direction='vertical', 
                                                        autoInsertRow=FALSE,
                                                        maxRows=nrow(rv.buildDesign$hot))) %>%
    rhandsontable::hot_rows(rowHeights = 30) %>%
    rhandsontable::hot_context_menu(allowRowEdit = TRUE, 
                                    allowColEdit = FALSE,
                                    allowInsertRow = FALSE,
                                    allowInsertColumn = FALSE,
                                    allowRemoveRow = TRUE,
                                    allowRemoveColumn = FALSE,
                                    autoInsertRow=FALSE     ) %>%
    rhandsontable:: hot_cols(renderer = color_renderer()) %>%
    rhandsontable::hot_col(col = "Sample.name", readOnly = TRUE)
  
  if (!is.null(input$chooseExpDesign)) {
    switch(input$chooseExpDesign,
           FlatDesign = {
             if ("Bio.Rep" %in% colnames(rv.buildDesign$hot))
               hot <- hot %>% rhandsontable::hot_col(col = "Bio.Rep", readOnly = TRUE)
           },
           twoLevelsDesign = {
             if ("Tech.Rep" %in% colnames(rv.buildDesign$hot))
               hot <- hot %>% rhandsontable::hot_col(col =  "Tech.Rep", readOnly = TRUE)
           } ,
           threeLevelsDesign = {
             if ("Analyt.Rep" %in% colnames(rv.buildDesign$hot))
               hot <- hot %>% rhandsontable::hot_col(col = "Analyt.Rep", readOnly = TRUE)
           }
    )
  }
  hot
  
})





#----------------------------------------------------------
output$UI_checkConditions  <- renderUI({
  
  req(rv.buildDesign$hot)
  rv.buildDesign$conditionsChecked
  input$convert_reorder
  
  if ((sum(rv.buildDesign$hot$Condition == "")==0) && (input$convert_reorder != "None")){
    tags$div(
      tags$div(style="display:inline-block;",
               actionButton(ns("btn_checkConds"), "Check conditions", class = actionBtnClass)
      ),
      
      tags$div(style="display:inline-block;",
               if(!is.null(rv.buildDesign$conditionsChecked)){
                 
                 if (isTRUE(rv.buildDesign$conditionsChecked$valid)){
                   img <- "images/Ok.png"
                   txt <- "Correct conditions"
                 }else {
                   img <- "images/Problem.png"
                   txt <- "Invalid conditions"
                 }
                 tagList(
                   tags$div(
                     tags$div(style="display:inline-block;",tags$img(src = img, height=25)),
                     tags$div(style="display:inline-block;",tags$p(txt))
                   ),
                   if(!isTRUE(rv.buildDesign$conditionsChecked$valid)){
                     tags$p(rv.buildDesign$conditionsChecked$warn)
                   }
                 )
               }
      )
    )
  } else {
    tagList(
      br(),
      br()
    )
    
  }
})



#------------------------------------------------------------------------------
output$UI_hierarchicalExp <- renderUI({
  req(rv.buildDesign$conditionsChecked)
  if (!isTRUE(rv.buildDesign$conditionsChecked$valid)){return(NULL)
  } else {
    tagList(
      div(
        div(
          # edit1
          style="display:inline-block; vertical-align: middle;",
          tags$b("2 - Choose the type of experimental design and complete it accordingly")
        ),
        div(
          # edit2
          style="display:inline-block; vertical-align: middle;",
          tags$button(id=ns("btn_helpDesign"), tags$sup("[?]"), class="Prostar_tooltip")
        )
      ),
      
      radioButtons(ns("chooseExpDesign"), "",
                   choices = c("Flat design (automatic)" = "FlatDesign" ,
                               "2 levels design (complete Bio.Rep column)" = "twoLevelsDesign" ,
                               "3 levels design (complete Bio.Rep and Tech.Rep columns)" = "threeLevelsDesign" ),
                   selected=character(0))
    )
  }
  
})






#------------------------------------------------------------------------------
output$viewDesign <- renderUI({
  
  rv.buildDesign$designSaved
  if (isTRUE(rv.buildDesign$designSaved)){return(NULL)}
  
  tagList(
    h4("Design"),
    rHandsontableOutput(ns("hot"))
  )
})


callModule(moduleDesignExample,"buildDesignExampleThree", reactive({3}), n_rows = reactive({nrow(rv.buildDesign$hot)}))
callModule(moduleDesignExample,"buildDesignExampleTwo", reactive({2}), n_rows = reactive({nrow(rv.buildDesign$hot)}))


#------------------------------------------------------------------------------
output$designExamples <- renderUI({
  input$chooseExpDesign
  print(input$chooseExpDesign)
  switch(input$chooseExpDesign,
         FlatDesign = 
           {
             tags$p("There is nothing to do for the flat design: the 'Bio.Rep' column is already filled.")
           },
         twoLevelsDesign =  {
           tagList(
             h4("Example for a 2-levels design"),
             moduleDesignExampleUI(ns("buildDesignExampleTwo"))
           )
         },
         threeLevelsDesign =  {
           tagList(
             h4("Example for a 3-levels design"),
             moduleDesignExampleUI(ns("buildDesignExampleThree"))
           )
         }
  )
})


#------------------------------------------------------------------------------
observe({
  shinyjs::onclick("btn_helpDesign",{
    print("onClick : btn_helpDesign")
    if (!is.null(input$chooseExpDesign))
      {
      shinyjs::toggle(id = "designExamples", anim = TRUE)
    }
    }
  )
})




#------------------------------------------------------------------------------
observeEvent(input$chooseExpDesign, {
  rv.buildDesign$hot
  rv.buildDesign$designChecked <- NULL
  switch(input$chooseExpDesign,
         FlatDesign = {
           rv.buildDesign$hot  <- data.frame(rv.buildDesign$hot[,1:2],
                                             Bio.Rep = seq(1:nrow(rv.buildDesign$hot)),
                                             stringsAsFactors = FALSE)
         },
         twoLevelsDesign = {
           rv.buildDesign$hot  <- data.frame(rv.buildDesign$hot[,1:2],Bio.Rep = rep("",nrow(rv.buildDesign$hot)),
                                             Tech.Rep = seq(1:nrow(rv.buildDesign$hot)),
                                             stringsAsFactors = FALSE)
         },
         threeLevelsDesign = {
           #if (length(grep("Tech.Rep", colnames(rv.buildDesign$hot))) > 0) { return(NULL)}
           rv.buildDesign$hot  <- data.frame(rv.buildDesign$hot[,1:2],
                                             Bio.Rep = rep("",nrow(rv.buildDesign$hot)),
                                             Tech.Rep = rep("",nrow(rv.buildDesign$hot)),
                                             Analyt.Rep = seq(1:nrow(rv.buildDesign$hot)),
                                             stringsAsFactors = FALSE)
         }
  )
})




#------------------------------------------------------------------------------
observeEvent(input$hot,{ rv.buildDesign$hot <-  hot_to_r(input$hot)})



#------------------------------------------------------------------------------
observeEvent(input$btn_checkDesign,{ 
  rv.buildDesign$designChecked <- DAPAR::check.design(rv.buildDesign$hot)
  print(rv.buildDesign$designChecked)
  
})

#------------------------------------------------------------------------------
output$checkDesign <- renderUI({
  req(input$chooseExpDesign)
  rv.buildDesign$designChecked
  req(rv.buildDesign$conditionsChecked)
  
  if(!isTRUE(rv.buildDesign$conditionsChecked$valid)){return(NULL)}
  switch(isolate({input$chooseExpDesign}),
         FlatDesign = {},
         twoLevelsDesign = { if (sum(rv.buildDesign$hot$Bio.Rep == "") > 0) {return(NULL)}},
         threeLevelsDesign = {if ((sum(rv.buildDesign$hot$Bio.Rep == "")+sum(rv.buildDesign$hot$Tech.Rep == "")) > 0) {return(NULL)}}
  )
  
  
  tags$div(
    tags$div(
      style="display:inline-block;",
      actionButton(ns("btn_checkDesign"), "Check design", class = actionBtnClass)
    ),
    
    tags$div(
      style="display:inline-block;",
      if(!is.null(rv.buildDesign$designChecked)){
        
        if (isTRUE(rv.buildDesign$designChecked$valid)){
          shinyjs::enable("createMSnsetButton")
          img <- "images/Ok.png"
          txt <- "Correct design"
          #rvNavProcess$Done[4] <- TRUE
        }else {
          img <- "images/Problem.png"
          txt <- "Invalid design"}
        tagList(
          tags$div(
            tags$div(style="display:inline-block;",tags$img(src = img, height=25)),
            tags$div(style="display:inline-block;",tags$p(txt))
          ),
          if(!isTRUE(rv.buildDesign$designChecked$valid)){
            shinyjs::disable("createMSnsetButton")
            tags$p(rv.buildDesign$designChecked$warn)
          } else {
            shinyjs::enable("createMSnsetButton")
            #rvNavProcess$Done[4] <- TRUE
          }
        )
      } else {
        shinyjs::disable("createMSnsetButton")
      }
    )
    
  )
  
  
})






return(reactive({rv.convert$dataOut}))

}






