tabPanel("Convert data",

         value = "import",
         helpText("These steps allow to create a MSnSet file 
                  from a tabulated-text file."),
         tabsetPanel(
             id = "convertData",
             tabPanel(
                 "1 - Select file",
                 value = "SelectFile2Import",
                 br(), br(),
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
                 
             ),
             tabPanel( "2 - Data Id",
                       value = "ID",
                       br(), br(),
                       #uiOutput("helpTextDataID"),
                       modulePopoverUI("modulePopover_convertIdType"),
                       radioButtons("autoID", width="500px",
                                    "", 
                                    choices=G_ConvertDataID_Choices),
                       conditionalPanel(
                           condition = 'input.autoID == "user ID"',
                           uiOutput("id"),
                           uiOutput("warningNonUniqueID"))
             ),
             
             tabPanel( "3 - Exp. and feat. data",
                       value = "Import1",
                       br(), br(),
                       
                       tagList(
                           checkboxInput("selectIdent", "Select columns for identification method", value = FALSE),
                           fluidRow(
                               column(width=4,uiOutput("eData",width = widthWellPanel)),
                               column(width=8,dataTableOutput("x1", width='500px'))),
                           tags$script(HTML("Shiny.addCustomMessageHandler('unbind-DT', function(id) {
                                   Shiny.unbindAll($('#'+id).find('table').DataTable().table().node());
                                   })"))
                           #verbatimTextOutput("out")
                          #  uiOutput("chooseOriginOfValues")
                       )
                       ),
             
             tabPanel( "4 - Samples metadata",
                       value = "Import2",
                       #width = widthWellPanel,
                       br(), br(),
                       helpText("Warning : it is mandatory that the column 
                                \"Label\" is filled."),
                       rHandsontableOutput("hot"
                                           ,width = widthWellPanel
                                           ,height = "100%"),
                       br(),
                       uiOutput("UI_generateSampleID"),
                       uiOutput("UI_hierarchicalExp"),
                       actionButton("checkDesign", "Check the design")
                       
             ),
             
             
             tabPanel( "5 - Convert",
                       value = "Convert",
                       br(), br(),
                       
                       uiOutput("checkAll_convert", width="50"),
                       htmlOutput("msgAlertCreateMSnset"),
                       hr(),
                       textInput("filenameToCreate",
                                 "Enter the name of the study"),
                       busyIndicator(WaitMsgCalc,wait = 0),
                       actionButton("createMSnsetButton","Convert data"),
                       uiOutput("warningCreateMSnset"),
                       moduleDatasetOverviewUI("overview_convertData"),
                       uiOutput("conversionDone")
                       
                       
             )
         )
)
