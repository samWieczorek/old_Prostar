moduleBugReportUI <- function(id)
  {
  ns <- NS(id)
  
  tagList(
          tags$p("If you encounter an issue with Prostar, you can send an email to the maintainer so he will have sufficient informations
                  to start identify the problem. In addition, you can add the MSnset as an attachment to the mail."),
          tags$br(),
          fluidRow(
                   column(width=3,uiOutput(ns("BugReport_output"))),
                   column(width=3,uiOutput(ns("clip")))
                   ),
           tags$br(),
          tags$head(tags$style("#fileReaderText{font-size:12px; font-style:italic;overflow-y:scroll; max-height: 400px; background: ghostwhite;}")),
          verbatimTextOutput(ns("fileReaderText"))
          )

}



moduleBugReport <- function(input, output, session,logfile){
  ns <- session$ns
  
observe({
  logfile()
  print(paste0("logfile =", logfile()))
  })


# Add clipboard buttons
output$clip <- renderUI({
  rclipButton(ns("clipbtn"), "Copy to clipboard", isolate({fileReaderData()}), icon("clipboard"))
})

# Workaround for execution within RStudio
observeEvent(input$clipbtn, clipr::write_clip(fileReaderData()))


# ============================================================
# This part of the code monitors the file for changes once per
# 0.5 second (500 milliseconds).
fileReaderData <- reactiveFileReader(500, session,logfilename, readLines)

output$fileReaderText <- renderText({
  # Read the text, and make it a consistent number of lines so
  # that the output box doesn't grow in height.
  text <- fileReaderData()
  paste(text, collapse = '\n')
})



output$BugReport_output <- renderUI({
  rv$current.obj
  
  mail <- unlist(strsplit(maintainer("Prostar"), "<"))[2]
  mail <- unlist(strsplit(mail, ">"))[1]
  
  tagList(
    
    
    a(actionButton(inputId = ns("email1"), label = "Contact maintainer", 
                   icon = icon("envelope", lib = "font-awesome"), class = actionBtnClass),
      href=paste0("mailto:", mail,"?subject=[Prostar bug report]&body="))
  ) 
  
})

}