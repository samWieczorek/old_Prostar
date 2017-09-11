tabPanel("Export",
         value = "export",
         icon = icon("upload"),
         helpText("Choose the export format of the dataset and choose a name."),
         selectInput("fileformatExport", "File format", 
                     choices=  c( "MSnset","Excel")),
         
         br(),
         helpText("Select the columns you want to keep as metadata. 
                  By default, if any column is specified, all metadata in your dataset
                  will be exported."),
         
         uiOutput("chooseMetaDataExport",width = widthWellPanel),
         br(),br(),
         uiOutput("chooseExportFilename"),
         
         br(),
         downloadButton('downloadMSnSet', 'Download'),
         
         br(),br(),
         radioButtons('format', 'Document format', c('PDF', 'HTML', 'Word'),
                      inline = TRUE),
         downloadButton('downloadReport', "Download report")
         )