tabPanel("Export",
         value = "export",
         icon = icon("upload"),
         h3("Export dataset"),
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
         downloadButton('downloadMSnSet', 'Download'),
         br(),br(),hr(),
         h3("Generate report"),
         
          
         br(),br(),
         fluidRow(
             column(width= 4, radioButtons('format', 'Choose the report document format', c('PDF', 'HTML', 'Word'),
                                           inline = TRUE)),
             column(width= 4, downloadButton('downloadReport', "Download report"))
         )
         
         )