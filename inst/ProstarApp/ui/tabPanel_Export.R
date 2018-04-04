tabPanel("Export",
         tabsetPanel(
             id = "Export_tabSetPanel",
             tabPanel("Export to file",
         value = "export",
         helpText("Choose the export format of the dataset and choose a name."),
         selectInput("fileformatExport", "File format", 
                     choices=  gFileFormatExport),
         
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
                      fluidRow(
                          column(width=4, uiOutput("choosedataTobuildReport")),
                          column(width=4, tagList(
                              selectInput("sizePNGplots", "Choose the size of images (PNG)", choices = c("1200 * 800")),
                              selectInput("resoPNGplots", "Choose the resolution", choices = c(150)))
                              ),
                           column(width=4, tagList(
                                  radioButtons('format', 'Choose the report document format', c('PDF', 'HTML', 'Word'),
                                               inline = TRUE),
                                  br(),
                                  textInput("reportFilename",
                                            label = "Enter the name of the report",
                                            value = ""))
                      )
                      ),

             br(),
             #busyIndicator(WaitMsgCalc,wait = 0),
         br(),br(),
         br(),br(),
         #busyIndicator("Building graphics, please wait...",wait = 0),
         actionButton("generateReport", "Generate report"),
         busyIndicator("Compiling report, please wait...",wait = 0),
             downloadButton('downloadReport', "Download report"),
             uiOutput("test")
                    )
         )
         
         )
         )
