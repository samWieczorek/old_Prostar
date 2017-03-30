navbarMenu("Dataset manager",
           tabPanel("Open MSnset file",
                    value = "open",
                    sidebarCustom(),
                    splitLayout(cellWidths = c(widthLeftPanel, widthRightPanel),
                                wellPanel(id = "wellPanelFileOpen"
                                          ,fileInput("file", 
                                                     "Open a MSnset file",
                                                     multiple = FALSE)
                                ),
                                tagList(
                                                 h3("Quick overview of the dataset"),
                                                 uiOutput("overview"),
                                                 uiOutput("infoAboutAggregationTool")
                                )
                    )
           ),
           tabPanel("Convert data",
                    icon = icon("download"),
                    value = "import",
                    helpText("These steps allow to create a MSnSet file 
                             from a tabulated-text file."),
                    tabsetPanel(
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
                                               choices=c("Auto ID" = "Auto ID", "User ID" = "user ID")),
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
                                  busyIndicator("Calculation in progress",wait = 0),
                                  actionButton("createMSnsetButton","Convert data")
                                  ,uiOutput("conversionDone")
                        )
                    )
           ),
           
           tabPanel("Export",
                    value = "export",
                    icon = icon("upload"),
                    helpText("Choose the export format of the dataset and choose a name."),
                    selectInput("fileformatExport", "File format", 
                                choices=  c( "MSnset","Excel")),
                    
                    br(),
                    helpText("Select the columns you want to keep as metadata. 
                             By default, in any column is specified, all meta data in your dataset
                             will be exported."),
                    
                    uiOutput("chooseMetaDataExport",width = widthWellPanel),
                    br(),br(),
                    uiOutput("chooseExportFilename"),
                    
                    br(),
                    downloadButton('downloadMSnSet', 'Download'),
                    
                    br(),br()
                    #radioButtons('format', 'Document format', c('PDF', 'HTML', 'Word'),
                    #             inline = TRUE),
                    #downloadButton('downloadReport', "Download report")
                    ),
           
           tabPanel("Demo mode",
                    id = "demo",
                    sidebarCustom(),
                    splitLayout(cellWidths = c(widthLeftPanel, widthRightPanel),
                                wellPanel(id = "chooseDatasetFromDAPARdata_wellPanel"
                                          ,uiOutput("chooseDataset")
                                          ,actionButton("loadDemoDataset", "Load demo dataset")
                                ),
                                tagList(
                                                 h3("Quick overview of the dataset"),
                                                 uiOutput("overviewDemoDataset"),
                                                 uiOutput("showDatasetDoc")
                                                 
                                                 # uiOutput("infoAboutDemoDataset")
                                )
                    )),
           tabPanel("Log session",
                    value = "ChangeDataset",
                    
                    tabsetPanel(
                        "test",
                        tabPanel("Log session",
                                 value = "ChangeDataset",
                                 sidebarCustom(),
                                 tagList(
                                     width=widthWellPanel,
                                     DT::dataTableOutput("logSession")
                                 )
                        ),
                        tabPanel("R source code", 
                                 uiOutput("code")
                        )
                    )
           )
           )

