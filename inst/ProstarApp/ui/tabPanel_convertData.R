tabPanel("Convert data",

         value = "import",
         helpText("These steps allow to create a MSnSet file 
                  from a tabulated-text file."),
         tabsetPanel(
             id = "convertData",
             tabPanel(
                 "1 - Select file",
                 value = "SelectFile2Import",
                 fileInput("file1", "Data file (.txt, .csv, .tsv, .xls, .xlsx files)", 
                           multiple=FALSE, 
                           accept=c(".txt", ".tsv", ".csv",".xls", ".xlsx")),
                 uiOutput("ManageXlsFiles"),
                 # helpText("Hint : before importing quantification 
                 #             file data, check the syntax of your text 
                 #             file."),
                 br(),
                 uiOutput("ConvertOptions")
                 
             ),
             tabPanel( "2 - Data Id",
                       value = "ID",
                       uiOutput("helpTextDataID"),
                       radioButtons("autoID", width="500px",
                                    "If you choose the automatic ID, Prostar will build an index.", 
                                    choices=G_ConvertDataID_Choices),
                       conditionalPanel(
                           condition = 'input.autoID == "user ID"',
                           uiOutput("id"),
                           uiOutput("warningNonUniqueID"))
             ),
             
             tabPanel( "3 - Exp. and feat. data",
                       value = "Import1",
                       helpText("Select the columns that are quantitation values 
                                by clicking in the field below."),
                       div(class="row"),
                       div(class="span5", "Quantitative  Data",
                           uiOutput("eData",width = widthWellPanel))
                       ),
             
             tabPanel( "4 - Samples metadata",
                       value = "Import2",
                       #width = widthWellPanel,
                       helpText("Warning : it is mandatory that the column 
                                \"Label\" is filled."),
                       br(),
                       rHandsontableOutput("hot"
                                           ,width = widthWellPanel
                                           ,height = "100%")
             ),
             
             
             tabPanel( "5 - Convert",
                       value = "Convert",
                       htmlOutput("msgAlertCreateMSnset"),
                       textInput("filenameToCreate",
                                 "Enter the name of the study"),
                       busyIndicator(WaitMsgCalc,wait = 0),
                       actionButton("createMSnsetButton","Convert data")
                       ,uiOutput("conversionDone")
             )
         )
)
