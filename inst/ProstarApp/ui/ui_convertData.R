tabPanel("Convert data",
        value = "convertTab",
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
                         fluidRow(
                           column(width=4,checkboxInput("selectIdent", 
                                                      "Select columns for identification method", 
                                                      value = FALSE)),
                         column(width=4,uiOutput("checkIdentificationTab"))
                         ),
                           fluidRow(
                               column(width=4,uiOutput("eData",width = "400px")),
                               column(width=8,dataTableOutput("x1", width='500px'))),
                           tags$script(HTML("Shiny.addCustomMessageHandler('unbind-DT', function(id) {
                                   Shiny.unbindAll($('#'+id).find('table').DataTable().table().node());
                                   })"))
                           )
                       ),
             
             tabPanel( "4 - Samples metadata",
                       value = "buildDesign_Tab",
                       br(), br(),
                       tagList(
                           fluidRow(
                               column(width=6,tags$b("1 - Fill the \"Condition\" column to identify the conditions to compare.")),
                               column(width=6,uiOutput("UI_checkConditions")  )
                       ),
                       fluidRow(
                           column(width=6,uiOutput("UI_hierarchicalExp")),
                           column(width=6,uiOutput("checkDesign") )
                       )
                       ),
                       hr(),
                       
                       busyIndicator(WaitMsgCalc,wait = 0),
                       tags$div(
                         
                         tags$div(style="display:inline-block; vertical-align: top;",
                                  uiOutput("viewDesign",width="100%")
                         ),
                         tags$div(style="display:inline-block; vertical-align: top;",
                                  shinyjs::hidden(
                                    div(id = "exLevels",uiOutput("designExamples")))
                         )
                         
                         
                       )
             ),
             
             
             tabPanel( "5 - Convert",
                       value = "Convert",
                       br(), br(),
                       
                       uiOutput("convertFinalStep")
                       
                       
                       
             )
         )
)
