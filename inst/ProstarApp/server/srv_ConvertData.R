# callModule(modulePopover,"modulePopover_convertChooseDatafile", 
#            data = reactive(list(title = HTML(paste0("<strong><font size=\"4\">Data file</font></strong>")), 
#                                 content="Select one (.txt, .csv, .tsv, .xls, .xlsx) file.")))
# 
# callModule(modulePopover,"modulePopover_convertIdType", 
#            data = reactive(list(title = HTML(paste0("<strong><font size=\"4\">ID definition</font></strong>")), 
#                                 content="If you choose the automatic ID, Prostar will build an index.")))
# 
# 
# 
# callModule(modulePopover,"modulePopover_convertProteinID", 
#            data = reactive(list(title = HTML(paste0("<strong><font size=\"4\">Select protein IDs</font></strong>")), 
#                                 content="Select the column containing the parent protein IDs.")))
# 
# 
# callModule(modulePopover,"modulePopover_convertDataQuanti", 
#            data = reactive(list(title = HTML(paste0("<strong><font size=\"4\">Quantitative data</font></strong>")), 
#                                 content="Select the columns that are quantitation values by clicking in the field below.")))
# 
# callModule(moduleStaticDataTable,"overview_convertData", table2show=reactive({GetDatasetOverview()}),
#            filename='ConvertData_overview')
# 
# 
# 
# callModule(moduleProcess, "moduleProcess_Convert", 
#            isDone = reactive({rvModProcess$moduleConvertDone}), 
#            pages = reactive({rvModProcess$moduleConvert}),
#            rstFunc = resetModuleConvert,
#            forceReset = reactive({rvModProcess$moduleConvertForceReset}))
# 
# 
# resetModuleConvert<- reactive({  
#   resetModuleProcess("Convert")
# 
#    rv$widgets$Convert$datafile <- NULL
#   rv$widgets$Convert$selectIdent <- FALSE
#   rv$widgets$Convert$convert_proteinId <- character(0)
#   rv$widgets$Convert$idBox <- "Auto ID"
#   rv$widgets$Convert$eDatabox <- character(0)
#   rv$widgets$Convert$typeOfData <- "peptide"
#   rv$widgets$Convert$checkDataLogged <- "no"
#   rv$widgets$Convert$replaceAllZeros <- TRUE
#   rv$widgets$Convert$convert_reorder <- "no"
#   rv$widgets$Convert$XLSsheets <- character(0)
#   rv$hot <- NULL
#   rv$tab1 <- NULL
#   rv$designChecked <- NULL
#   #rv$current.obj <- rv$dataset[[input$datasets]]
#   rvModProcess$moduleConvertDone <- rep(FALSE, 5)
#   
# })
# 
# 
# observeEvent(req(input$file1),{
#   rv$widgets$Convert$datafile <- input$file1
#   rvModProcess$moduleConvertDone[1] <- TRUE
#   })
# 
# 
# observeEvent(rvModProcess$moduleConvertForceReset,{
#   rv$widgets$Convert$datafile <- NULL
#  
# })
# 
# 
# observeEvent(input$selectIdent,{ rv$widgets$Convert$selectIdent <- input$selectIdent})
# observeEvent(input$convert_proteinId,{ rv$widgets$Convert$convert_proteinId <- input$convert_proteinId})
# observeEvent(input$idBox,{ rv$widgets$Convert$idBox <- input$idBox})
# observeEvent(input$eDatabox,{ rv$widgets$Convert$eDatabox <- input$eDatabox})
# observeEvent(input$typeOfData,{ rv$widgets$Convert$typeOfData <- input$typeOfData})
# observeEvent(input$checkDataLogged,{ rv$widgets$Convert$checkDataLogged <- input$checkDataLogged})
# observeEvent(input$checkDataLogged,{ rv$widgets$Convert$checkDataLogged <- input$checkDataLogged})
# observeEvent(input$replaceAllZeros,{ rv$widgets$Convert$replaceAllZeros <- input$replaceAllZeros})
# observeEvent(input$convert_reorder,{ rv$widgets$Convert$convert_reorder <- input$convert_reorder})
# observeEvent(input$XLSsheets,{ rv$widgets$Convert$XLSsheets <- input$XLSsheets})
# 
# 
# 
# output$resettableInput <- renderUI({
#   rv$widgets$Convert$datafile
#   fileInput("file1", rv$widgets$Convert$datafile$name, 
#             multiple=FALSE, 
#             accept=c(".txt", ".tsv", ".csv",".xls", ".xlsx")) 
# })
# 
# 
# #################################
# output$Convert_SelectFile <- renderUI({
#    tagList(br(), br(),
#           fluidRow(
#             column(width=2, modulePopoverUI("modulePopover_convertChooseDatafile")),
#             column(width = 10, uiOutput('resettableInput') )),
#           uiOutput("ManageXlsFiles"),
#           # helpText("Hint : before importing quantification 
#           #             file data, check the syntax of your text 
#           #             file."),
#           br(),
#           uiOutput("ConvertOptions")
#   )
# })
# 
# 
# 
# ### SCREEN 2
# output$Convert_DataId <- renderUI({
#   
#   tagList(
#     
# br(), br(),
# #uiOutput("helpTextDataID"),
# 
# tags$div(
#   tags$div( style="display:inline-block; vertical-align: top; padding-right: 100px;",
#             uiOutput("id"),
#             uiOutput("warningNonUniqueID")
#   ),
#   tags$div( style="display:inline-block; vertical-align: top;",
#             uiOutput("convertChooseProteinID_UI"),
#             uiOutput("previewProteinID_UI")
#   )
# )
# )
# })
# 
# 
# 
# 
# output$Convert_ExpFeatData <- renderUI({
#   
#   tagList(
#     
#     fluidRow(
#       column(width=4,uiOutput("eData",width = "400px")),
#       column(width=8,
#              tagList(
#                uiOutput("checkIdentificationTab"),
#                 
#                checkboxInput("selectIdent", 
#                   "Select columns for identification method", 
#                   value = rv$widgets$Convert$selectIdent),
#                tags$script(HTML("Shiny.addCustomMessageHandler('unbind-DT', function(id) {
#                                    Shiny.unbindAll($('#'+id).find('table').DataTable().table().node());
#                                    })")),
#                shinyjs::hidden(DT::dataTableOutput("x1", width='500px'))
#                            )
#     )
#   )
#   )
#   })
# 
# 
# 
#   observeEvent(req(rv$widgets$Convert$eDatabox), {
#     shinyjs::toggle('selectIdent', condition= length(rv$widgets$Convert$eDatabox)>0)
#   })
# 
#   
#   ### SCREEN 4
# output$Convert_BuildDesign <- renderUI({
#   req(rv$widgets$Convert$datafile)
#   tagList(... = tagList(
#     tags$p("If you do not know how to fill the experimental design, you can click
#                                   on the '?' next to each design in the list that appear once the conditions 
#                                   are checked or got to the ", 
#            tags$a(href="http://prostar-proteomics.org/#how-to-build-a-valid-experimental-design", target='_blank', 'FAQ'),
#     
#            " page."),
#     fluidRow(
#       column(width=6,tags$b("1 - Fill the \"Condition\" column to identify the conditions to compare.")),
#       column(width=6,uiOutput("UI_checkConditions")  )
#     ),
#     fluidRow(
#       column(width=6,uiOutput("UI_hierarchicalExp")),
#       column(width=6,uiOutput("checkDesign") )
#     )
#   ),
#   hr(),
#   selectInput("convert_reorder", "Order by conditions ?",
#               selected=rv$widgets$Convert$convert_reorder,
#               choices=c("No"="No", "Yes"="Yes"),
#               width="100px"),
#   tags$div(
#     
#     tags$div(style="display:inline-block; vertical-align: top;",
#              uiOutput("viewDesign",width="100%")
#     ),
#     tags$div(style="display:inline-block; vertical-align: top;",
#              shinyjs::hidden(div(id = "showExamples", uiOutput("designExamples") ))
#     )
#     
#     
#   ))
#   
# })
# 
# 
# 
# 
# 
# 
# output$Convert_Convert <- renderUI({
#   tagList(
#     br(), br(),
#   
#   uiOutput("convertFinalStep"),
#   moduleStaticDataTableUI("overview_convertData"),
#   uiOutput("conversionDone")
#   
#   )
# })
# 
# 
# 
# observe({
#   req(input$idBox)
#   req(rv$tab1)
#   test1 <- test2 <- TRUE
#   
#   test1 <- (input$typeOfData == "peptide") && !(input$convert_proteinId == "")
#  
#   
#   if (input$idBox =="Auto ID") {
#     test2 <- TRUE
#   }
#   else {
#     test2 <- (length(as.data.frame(rv$tab1)[, input$idBox])
#           == length(unique(as.data.frame(rv$tab1)[, input$idBox])))
#   }
#   rvModProcess$moduleConvertDone[2] <- test1 && test2
# })
# 
# 
# 
# output$warningNonUniqueID <- renderUI({
#     req(input$idBox)
#     req(rv$tab1)
#     
#     isolate({
#       if (input$idBox =="Auto ID") {
#       text <- "<img src=\"images/Ok.png\" height=\"24\"></img>"
#  }
#     else {
#       t <- (length(as.data.frame(rv$tab1)[, input$idBox])
#           == length(unique(as.data.frame(rv$tab1)[, input$idBox])))
#     
#       if (!t){
#         text <- "<img src=\"images/Problem.png\" height=\"24\"></img><font color=\"red\">
#         Warning ! Your ID contains duplicate data.
#         Please choose another one."
#          }
#       else {
#         text <- "<img src=\"images/Ok.png\" height=\"24\"></img>"
#         }
#     }
#     HTML(text)
#     
#     })
# })
# 
# 
# 
# 
# output$convertChooseProteinID_UI <- renderUI({
#   req(rv$tab1)
#   
#   if (input$typeOfData == "protein") {return(NULL)}
#   
#   .choices <- c("",colnames(rv$tab1))
#   names(.choices) <- c("",colnames(rv$tab1))
#   tagList(
#     modulePopoverUI("modulePopover_convertProteinID"),
#     selectInput("convert_proteinId","",choices =  .choices , selected = rv$widgets$Convert$convert_proteinId )
#   )
# })
# 
# 
# 
# output$previewProteinID_UI <- renderUI({
#   req(rv$widgets$Convert$convert_proteinId)
#   if (rv$widgets$Convert$convert_proteinId == "") {return (NULL)}
#   
#   tagList(
#     p(style="color: black;", 'Preview'),
#     tableOutput("previewProtID")
#   )
#   # tags$head(tags$style("#previewProtID{color:red; font-size:12px; font-style:italic; 
#   #                      overflow-y:scroll; width: 100px; max-height: 200px; background: ghostwhite;}"))
#   
#   })
# 
# 
# 
# output$previewProtID <- renderTable(
#  # req(input$convert_proteinId),
#   head(rv$tab1[,rv$widgets$Convert$convert_proteinId]),
#  colnames = FALSE
# )
# 
# 
# #########################################################
# output$id <- renderUI({
#   req(rv$tab1)
#   
#   .choices <- c("Auto ID",colnames(rv$tab1))
#   names(.choices) <- c("Auto ID",colnames(rv$tab1))
#   
#   tagList(
#     modulePopoverUI("modulePopover_convertIdType"),
#     selectInput("idBox", label = "", choices = .choices, selected=rv$widgets$Convert$idBox)
#   )
#   
# })
# 
# 
# 
# output$ConvertOptions <- renderUI({
#   #req(rv$widgets$Convert$datafile )
#   
#   tagList(
#     radioButtons("typeOfData", 
#                  "Is it a peptide or protein dataset ?", 
#                  choices=c("peptide dataset" = "peptide", 
#                            "protein dataset" = "protein"),
#                  selected=rv$widgets$Convert$typeOfData
#     )
#     
#     ,radioButtons("checkDataLogged", 
#                   "Are your data already log-transformed ?", 
#                   #width = widthWellPanel, 
#                   choices=c("yes (they stay unchanged)" = "yes", 
#                             "no (they wil be automatically transformed)"="no"), 
#                   selected=rv$widgets$Convert$checkDataLogged)
#     ,br()
#     ,checkboxInput("replaceAllZeros", 
#                    "Replace all 0 and NaN by NA", 
#                    value= rv$widgets$Convert$replaceAllZeros)
#   )
# })
# 
# 
# observeEvent(input$fData.box,ignoreInit = TRUE,{
#   
#   choices = colnames(rv$tab1)[-which(colnames(rv$tab1) %in% input$fData.box)]
#   names(choices) = 
#     colnames(rv$tab1)[-which(colnames(rv$tab1) %in% input$fData.box)]
#   updateSelectInput(session, "eData.box", 
#                     label = "",
#                     choices = choices,
#                     selected = rv$widgets$Convert$eDatabox)
#   
# })
# 
# 
# 
# 
# output$helpTextDataID <- renderUI({
#   rv$widgets$Convert$typeOfData
#   if (is.null(rv$widgets$Convert$typeOfData)){return(NULL)}
#   t <- ""
#   switch(rv$widgets$Convert$typeOfData,
#          protein = {t <- "proteins"},
#          peptide = {t <- "peptides"}
#   )
#   txt <- paste ("Please select among the columns of your data the one that 
#                 corresponds to a unique ID of the ", t, ".", sep=" ")
#   helpText(txt)
#   
# })
# 
# 
# 
# 
# 
# readTextFile <- reactive({
#   req(rv$widgets$Convert$datafile)
#   rv$tab1 <- read.csv(rv$widgets$Convert$datafile$datapath,  header=TRUE, sep="\t", as.is=T)
# })
# 
# readXLSFile <- reactive({})
# 
# ############ Read text file to be imported ######################
# observeEvent(req(rv$widgets$Convert$datafile,rv$widgets$Convert$XLSsheets),{
#   
#   if (((GetExtension(rv$widgets$Convert$datafile$name)== "xls")
#        || (GetExtension(rv$widgets$Convert$datafile$name) == "xlsx") )
#       && is.null(rv$widgets$Convert$XLSsheets)) {return(NULL)  }
# 
#   authorizedExts <- c("txt","csv", "tsv","xls","xlsx")
#   if( is.na(match(GetExtension(rv$widgets$Convert$datafile$name), authorizedExts))) {
#     shinyjs::info("Warning : this file is not a text nor an Excel file ! 
#                   Please choose another one.")
#   }
#   else {
#   # result = tryCatch(
#   #   {
#       ClearUI()
#       ClearMemory()
#       ext <- GetExtension(rv$widgets$Convert$datafile$name)
#       
#       switch(ext,
#              txt = { rv$tab1 <- read.csv(rv$widgets$Convert$datafile$datapath,  header=TRUE, sep="\t", as.is=T)},
#              csv = { rv$tab1 <- read.csv(rv$widgets$Convert$datafile$datapath,  header=TRUE, sep="\t", as.is=T)},
#              tsv = { rv$tab1 <- read.csv(rv$widgets$Convert$datafile$datapath,  header=TRUE, sep="\t", as.is=T)},
#              xls = { rv$tab1 <- readExcel(rv$widgets$Convert$datafile$datapath, ext, sheet=rv$widgets$Convert$XLSsheets)},
#              xlsx = {rv$tab1 <- readExcel(rv$widgets$Convert$datafile$datapath, ext, sheet=rv$widgets$Convert$XLSsheets)}
#       )
#   #   }
#   #   , warning = function(w) {
#   #     shinyjs::info(conditionMessage(w))
#   #   }, error = function(e) {
#   #     shinyjs::info(paste("Read text file to convert",":",
#   #                         conditionMessage(e), 
#   #                         sep=" "))
#   #   }, finally = {
#   #     #cleanup-code 
#   #   })
#    }
#   
# })
# 
# 
# 
# 
# 
# 
# output$conversionDone <- renderUI({
#   req(rv$current.obj)
#   
#   h4("The conversion is done. Your dataset has been automatically loaded 
#        in memory. Now, you can switch to the Descriptive statistics panel to 
#        vizualize your data.")
# })
# 
# 
# # 
# # observe({
# #   rvModProcess$moduleConvertDone[1] <- !is.null(rv$widgets$Convert$datafile)
# # })
# # 
# 
# 
# 
# #####-------------------------------------------------------
# output$ManageXlsFiles <- renderUI({
#   req(rv$widgets$Convert$datafile)
#   
#   .ext <- GetExtension(rv$widgets$Convert$datafile$name)
#   if ((.ext == "xls") || (.ext == "xlsx")){ 
#     sheets <- listSheets(rv$widgets$Convert$datafile$datapath)
#     selectInput("XLSsheets", "sheets", choices = as.list(sheets),
#                 selected=rv$widgets$Convert$XLSsheets,
#                 width='200px')
#   }
#   
# })
# 
# 
# 
# 
# 
# 
# 
# ###########################################
# 
# #
# #
# ##   Quanti data table
# 
# ##
# ##
# ##############################################
# output$eData <- renderUI({
#     #rv$widgets$Convert$datafile
#     req(rv$tab1)
#     
#     choices <- colnames(rv$tab1)
#     names(choices) <- colnames(rv$tab1)
#     
#     tagList(
#         modulePopoverUI("modulePopover_convertDataQuanti"),
#         selectInput("eData.box",
#                     label = "",
#                     choices = choices,
#                     selected = rv$widgets$Convert$eDatabox,
#                     multiple = TRUE, width='200px',
#                     size = 20,
#                     selectize = FALSE)
#     )
# })
# 
# 
# 
# 
# 
# # updateInputs <- function(id, n){
# #     for (i in seq_len(n)) {
# #         updateSelectInput(paste0(id,i),label=NULL,selected = input[[paste0(id,i)]])
# #     }
# # }
# 
# 
# 
# observe({
#   rvModProcess$moduleConvertDone[1] <- !is.null(rv$widgets$Convert$datafile)
#   rvModProcess$moduleConvertDone[3] <- length(rv$widgets$Convert$eDatabox)>0
#   rvModProcess$moduleConvertDone[4] <- rvModProcess$moduleConvertDone[4] || (!is.null(rv$designChecked$valid) && isTRUE(rv$designChecked$valid))
# })
# 
# 
# # observe({
# #   rvModProcess$moduleConvertDone[4] <- !is.null(rv$designChecked$valid) && isTRUE(rv$designChecked$valid)
# # })
# 
# 
# output$checkIdentificationTab <- renderUI({
#   req(rv$widgets$Convert$selectIdent)
#   if (!isTRUE(rv$widgets$Convert$selectIdent)){return(NULL)}
#   
#   shinyValue("colForOriginValue_",length(rv$widgets$Convert$eDatabox))
#     temp <- shinyValue("colForOriginValue_",length(rv$widgets$Convert$eDatabox))
#     
#     if ((length(which(temp == "None")) == length(temp)))
#       {
#         img <- "images/Ok.png"
#         txt <- "Correct"
#       }  else {
#         if (length(which(temp == "None")) > 0)
#           {
#             img <- "images/Problem.png"
#             txt <- "The identification method is not appropriately defined for each sample."
#           } else {
#             if(length(temp) != length(unique(temp))){
#                 img <- "images/Problem.png"
#                 txt <- "There are duplicates in identification columns."
#             }else { 
#               img <- "images/Ok.png"
#               txt <- "Correct"
#               }
#           }
#       }
#       tags$div(
#          tags$div(
#               tags$div(style="display:inline-block;",tags$img(src = img, height=25)),
#               tags$div(style="display:inline-block;",tags$p(txt))
#             )
#           )
#    
# 
# })
# 
# 
# 
# # reactive dataset
# quantiDataTable <- reactive({
#     req(rv$widgets$Convert$eDatabox)
#     req(rv$tab1)
#     
#     session$sendCustomMessage('unbind-DT', 'x1')
#     df <- NULL
#     choices <- c("None",colnames(rv$tab1))
#     names(choices) <- c("None",colnames(rv$tab1))
#     
#     if (isTRUE(rv$widgets$Convert$selectIdent)) {
#         
#         df <- data.frame(as.data.frame(rv$widgets$Convert$eDatabox),
#                          shinyInput(selectInput,
#                                     "colForOriginValue_",
#                                     nrow(as.data.frame(rv$widgets$Convert$eDatabox)),
#                                     choices=choices))
#         colnames(df) <- c("Sample", "Identification method")
#     } else {
#         df <- data.frame(Sample = as.data.frame(rv$widgets$Convert$eDatabox))
#         colnames(df) <- c("Sample")
#     }
#     df
# })
# 
# 
# observeEvent(rv$widgets$Convert$selectIdent, {
#   shinyjs::toggle('x1', condition=isTRUE(rv$widgets$Convert$selectIdent))
#   })
# 
# 
# output$x1 <- renderDataTable(
#     quantiDataTable(),
#     escape=FALSE,
#     rownames = FALSE,
#     extensions = c('Scroller'),
#     server=FALSE,
#     selection='none', 
#     class = 'compact',
#     options=list(
#         preDrawCallback=JS(
#             'function() {
#             Shiny.unbindAll(this.api().table().node());}'),
#         drawCallback= JS(
#             'function(settings) {
#             Shiny.bindAll(this.api().table().node());}'),
#         # rowCallback = JS("function(r,d) {$(r).attr('height', '10px')}"),
#         dom='Bfrtip',
#         autoWidth=TRUE,
#         deferRender = TRUE,
#         bLengthChange = FALSE,
#         scrollX = 200,
#         scrollY = 500,
#         scroller = TRUE,
#         ajax = list(url = dataTableAjax(session, quantiDataTable()))
#         
#         )
#     
#     )
# 
# 
# observeEvent(shinyValue("colForOriginValue_",nrow(quantiDataTable())),{})
# 
# 
# checkIdentificationMethod_Ok <- reactive({
#   #req(input$selectIdent)
#   res <- TRUE
#   tmp <- NULL
#   if (isTRUE(rv$widgets$Convert$selectIdent)) {
#     tmp <- shinyValue("colForOriginValue_",nrow(quantiDataTable()))
#     if ((length(grep("None", tmp)) > 0)  || (sum(is.na(tmp)) > 0)){ res <- FALSE }
#   } 
#   res
#   
# })
# 
# 
# datasetID_Ok <- reactive({
#   req(rv$widgets$Convert$idBox)
#   req(rv$tab1)
#   if (rv$widgets$Convert$idBox == "Auto ID") {t <- TRUE}
#   else {
#   t <- (length(as.data.frame(rv$tab1)[, rv$widgets$Convert$idBox])
#         == length(unique(as.data.frame(rv$tab1)[, rv$widgets$Convert$idBox])))
#   }
#   t
# })
# 
# 
# 
# 
# output$warningCreateMSnset <- renderUI({
#     if (isTRUE(rv$widgets$Convert$selectIdent)){
#         colNamesForOriginofValues <- shinyValue("colForOriginValue_",nrow(quantiDataTable()))
#         if (length(which(colNamesForOriginofValues == "None")) >0){
#             text <- "<font color=\"red\"> Warning: The MSnset cannot be created because the identification 
#             method are not fully filled.  <br>"
#             HTML(text)
#         }
#     }
# })
# 
# 
# output$convertFinalStep <- renderUI({
#   req(rv$designChecked)
#   if (!(rv$designChecked$valid)){return(NULL)}
#   tagList(
#     uiOutput("checkAll_convert", width="50"),
#     htmlOutput("msgAlertCreateMSnset"),
#     hr(),
#     textInput("filenameToCreate","Enter the name of the study"),
#     actionButton("createMSnsetButton","Convert data", class = actionBtnClass),
#     uiOutput("warningCreateMSnset")
#     
#   )
# })
# 
# 
# #######################################
# observeEvent(input$createMSnsetButton,{
#      if(!is.null(rv$current.obj)){return(NULL)}
#     print("In observeEvent(input$createMSnsetButton")
#     colNamesForOriginofValues <- NULL
#     if (isTRUE(rv$widgets$Convert$selectIdent)) {
#         colNamesForOriginofValues <- shinyValue("colForOriginValue_",nrow(quantiDataTable()))
#         if (length(which(colNamesForOriginofValues == "None")) >0){ return (NULL)   }
#     }
# 
# 
#         result = tryCatch(
#             {
#               isolate({
#                 ext <- GetExtension(rv$widgets$Convert$datafile$name)
#                 txtTab <-  paste("tab1 <- read.csv(\"", rv$widgets$Convert$datafile$name,
#                                  "\",header=TRUE, sep=\"\t\", as.is=T)",  sep="")
#                 txtXls <-  paste("tab1 <- read.xlsx(",rv$widgets$Convert$datafile$name,
#                                  ",sheet=", rv$widgets$Convert$XLSsheets,")",sep="")
#                 switch(ext,
#                        txt = writeToCommandLogFile(txtTab),
#                        csv = writeToCommandLogFile(txtTab),
#                        tsv = writeToCommandLogFile(txtTab),
#                        xls= writeToCommandLogFile(txtXls),
#                        xlsx = writeToCommandLogFile(txtXls)
#                 )
# 
#                 tmp.eData.box <- rv$widgets$Convert$eDatabox
#                 indexForEData <- match(tmp.eData.box, colnames(rv$tab1))
#                 if (!is.null(rv$newOrder)){
#                     tmp.eData.box <- tmp.eData.box[rv$newOrder]
#                     indexForEData <- indexForEData[rv$newOrder]
#                 }
# 
#                 indexForFData <- seq(1,ncol(rv$tab1))[-indexForEData]
# 
#                 indexForIDBox <- NULL
#                 if (rv$widgets$Convert$idBox !="Auto ID") {
#                     indexForIDBox <- match(rv$widgets$Convert$idBox, colnames(rv$tab1))
#                 }
# 
# 
#                 metadata <- hot_to_r(input$hot)
#                 logData <- (rv$widgets$Convert$checkDataLogged == "no")
# 
# 
#                 indexForOriginOfValue <- NULL
#                 if (!is.null(colNamesForOriginofValues) && (length(grep("None", colNamesForOriginofValues))==0)  && (sum(is.na(colNamesForOriginofValues)) == 0)){
#                     for (i in 1:length(tmp.eData.box)){
#                         indexForOriginOfValue <- c(indexForOriginOfValue, which(colnames(rv$tab1) == input[[paste0("colForOriginValue_", i)]]))
#                     }
#                 }
# 
# 
#                 versions <- list(Prostar_Version =
#                                    installed.packages(lib.loc = Prostar.loc)["Prostar","Version"],
#                                  DAPAR_Version =
#                                    installed.packages(lib.loc = DAPAR.loc)["DAPAR","Version"]
#                 )
# 
#                 rv$current.obj <- Exp1_R25_pept
#                 # rv$current.obj <- DAPAR::createMSnset(rv$tab1,
#                 #                     metadata,
#                 #                     indexForEData,
#                 #                     indexForFData,
#                 #                     indexForIDBox,
#                 #                     indexForOriginOfValue,
#                 #                     (rv$widgets$Convert$checkDataLogged == "no"),
#                 #                     rv$widgets$Convert$replaceAllZeros,
#                 #                     pep_prot_data = rv$widgets$Convert$typeOfData,
#                 #                     proteinId =  gsub(".", "_", rv$widgets$Convert$convert_proteinId, fixed=TRUE),
#                 #                     versions
#                 #                     )
# 
#                #rv$current.obj <- tmp
#                #rv$current.obj <- get("Exp1_R25_pept")
#                 rv$current.obj.name <- input$filenameToCreate
#                 rv$indexNA <- which(is.na(exprs(rv$current.obj)))
#                 rv$typeOfDataset <- rv$widgets$Convert$typeOfData
#                 rv$current.obj <- addOriginOfValue(rv$current.obj)
# 
#                 rvModProcess$moduleConvertDone[5] <- TRUE
#                 
#                loadObjectInMemoryFromConverter()
# 
#                 print("after loadObjectInMemoryFromConverter")
#                 print(rv$current.obj)
#                 #updateTabsetPanel(session, "tabImport", selected = "Convert")
#             }) ## end of isolate
#             }
#             , warning = function(w) {
#                 if (conditionMessage(w) %in% c("NaNs produced", "production de NaN")){
#                     shinyjs::info(paste("Warning : Your original dataset may contain negative values",
#                                         "so that they cannot be logged. Please check back the dataset or",
#                                         "the log option in the first tab.",
#                                         sep=" "))
#                 }
#               }, error = function(e) {
#                 shinyjs::info(paste("Error :","CreateMSnSet",":",
#                                     conditionMessage(e),
#                                     sep=" "))
#             }, finally = {
#                 #cleanup-code
#             })
# })
# 
# 
# 
# 
# 



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
          #actionButton("loadData2Convert", "Load data file",class = actionBtnClass),
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
                uiOutput("convertChooseProteinID_UI"),
                uiOutput("previewProteinID_UI")
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
  req(input$file1)
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
    shinyjs::disable("file1")
    switch(ext,
           txt = { rv$tab1 <- read.csv(input$file1$datapath,  header=TRUE, sep="\t", as.is=T)},
           csv = { rv$tab1 <- read.csv(input$file1$datapath,  header=TRUE, sep=";", as.is=T)},
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
  #shinyjs::disable('file1')
  
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
  df <- NULL
  if (is.null(input$eData.box)) return(NULL)
  #req(input$eData.box)
  req(rv$tab1)
  
  session$sendCustomMessage('unbind-DT', 'x1')
  choices <- c("None",colnames(rv$tab1))
  names(choices) <- c("None",colnames(rv$tab1))
  
  if (isTRUE(input$selectIdent)) {
    
    df <- data.frame(as.data.frame(input$eData.box),
                     shinyInput(selectInput,
                                "colForOriginValue_",
                                nrow(input$eData.box),
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


observeEvent(shinyValue("colForOriginValue_",
                        nrow(as.data.frame(quantiDataTable()))),{})


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



output$previewProteinID_UI <- renderUI({
  req(input$convert_proteinId)
  if (input$convert_proteinId == "") {return (NULL)}
  
  tagList(
    p(style="color: black;", 'Preview'),
    tableOutput("previewProtID")
  )
  
})



output$previewProtID <- renderTable(
  # req(input$convert_proteinId),
  head(rv$tab1[,input$convert_proteinId]),
  colnames = FALSE
)




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
        options(digits=15)
        tmp <- DAPAR::createMSnset(rv$tab1, 
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