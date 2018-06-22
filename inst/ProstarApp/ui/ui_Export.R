tabPanel("Export dataset",
         value = "ExportTab",
         tabsetPanel(
           id = "Export_tabSetPanel",
           tabPanel("Export to file",
                    value = "export",
                    helpText("Export format of the dataset and filename."),
                    
                    uiOutput("choosedataToExportMSnset"),
                    hr(),
                    uiOutput("exportOptions")
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
