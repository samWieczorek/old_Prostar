tabPanel("Bug report",
         value="bugReportTab",
         tags$p("If you encounter an issue with Prostar, you can send an email to the maintainer so he will have sufficient informations
           to start identify the problem. In addition, you can add the MSnset as an attachment to the mail."),
         tags$br(),
         fluidRow(
           column(width=3,uiOutput("BugReport_output")),
           column(width=3,uiOutput("clip"))
         ),
         tags$br(),
         
         verbatimTextOutput("fileReaderText"),
         tags$head(tags$style("#fileReaderText{font-size:12px; font-style:italic;overflow-y:scroll; max-height: 400px; background: ghostwhite;}"))
         #uiOutput("pasteToMail"),
         #tags$head(tags$style("#pasteToMail{font-size:12px; font-style:italic; overflow-y:scroll; max-height: 200px; background: ghostwhite;}"))
         
)