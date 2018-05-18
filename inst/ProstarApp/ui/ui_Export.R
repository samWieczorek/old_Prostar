tabPanel("Export",
         tabsetPanel(
             id = "Export_tabSetPanel",
             tabPanel("Export to file",
         value = "export",
         helpText("Export format of the dataset and filename."),
         fluidRow(
             column(width=2,modulePopoverUI("modulePopover_exportFileFormat")),
             column(width=10,selectInput("fileformatExport", "",choices=  gFileFormatExport))
         ),
         
         br(),
         fluidRow(
             column(width=2,modulePopoverUI("modulePopover_exportMetaData")),
             column(width=10,uiOutput("chooseMetaDataExport",width = widthWellPanel))
             ),
         br(),br(),
         fluidRow(
             column(width=2,modulePopoverUI("modulePopover_exportFilename")),
             column(width=10,uiOutput("chooseExportFilename"))
         ),

         br(),
         downloadButton('downloadMSnSet', 'Download')
             ),
         
         
         tabPanel("Generate report (Beta)",
                  tagList(
                      fluidRow(
                          column(width=4, uiOutput("choosedataTobuildReport")),
                          column(width=4, tagList(
                              selectInput("sizePNGplots", "Size of images (PNG)", choices = c("1200 * 800")),
                              selectInput("resoPNGplots", "Resolution", choices = c(150)))
                              ),
                           column(width=4, tagList(
                                  radioButtons('format', 'Report document format', c('PDF', 'HTML', 'Word'),
                                               inline = TRUE),
                                  br(),
                                  textInput("reportFilename",
                                            label = "Name of the report",
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
