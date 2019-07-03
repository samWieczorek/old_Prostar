tabPanel("Bug report",
         value="bugReportTab",
         tagList(
            tags$p("If you encounter an issue with Prostar, you can send an email to the maintainer so he will have sufficient informations
                    to start identify the problem. In addition, you can add the MSnset as an attachment to the mail."),
            tags$br(),
            uiOutput("BugReport_output"),
            tags$br(),
         
            tags$head(tags$style("#fileReaderText{font-size:12px; font-style:italic;overflow-y:scroll; max-height: 400px; background: ghostwhite;}")),
            verbatimTextOutput("fileReaderText")
         )
         
)