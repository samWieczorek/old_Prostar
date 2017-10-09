tabPanel("Export",
         tabsetPanel(
             id = "Export_tabSetPanel",
             tabPanel("Export to file",
         value = "export",
         helpText("Choose the export format of the dataset and choose a name."),
         selectInput("fileformatExport", "File format", 
                     choices=  G_exportFileFormat_Choices),
         
         br(),
         helpText("Select the columns you want to keep as metadata. 
                  By default, if any column is specified, all metadata in your dataset
                  will be exported."),
         
         uiOutput("chooseMetaDataExport",width = widthWellPanel),
         br(),br(),
         uiOutput("chooseExportFilename"),
         
         br(),
         downloadButton('downloadMSnSet', 'Download')
             ),
         tabPanel("Generate report (Beta)",
                  tagList(
                      uiOutput("choosedataTobuildReport"),
         br(),
         actionButton("generateReport", "Generate report"),
         
         
             br(),radioButtons('format', 'Choose the report document format', c('PDF', 'HTML', 'Word'),
                                           inline = TRUE),
             busyIndicator("Calculation in progress",wait = 0),
             br(),
             downloadButton('downloadReport', "Download report"),
             uiOutput("test")
                    )
         )
         
         )
         )
