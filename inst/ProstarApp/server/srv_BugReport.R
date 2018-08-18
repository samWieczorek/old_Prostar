# Add clipboard buttons
output$clip <- renderUI({
  rclipButton("clipbtn", "Copy to clipboard", isolate({fileReaderData()}), icon("clipboard"))
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
  paste(fileReaderData(), collapse = '\n')
})



output$BugReport_output <- renderUI({
  rv$current.obj
  
  mail <- unlist(strsplit(maintainer("Prostar"), "<"))[2]
  mail <- unlist(strsplit(mail, ">"))[1]
  
  tagList(
    
    
    a(actionButton(inputId = "email1", label = "Contact maintainer", 
                   icon = icon("envelope", lib = "font-awesome")),
      href=paste0("mailto:", mail,"?subject=[Prostar bug report]&body="))
      ) 

})
# 
# output$pasteToMail <- renderUI({
#   
#   tagList(
#     tags$h4("pData(obj)"),
#     HTML(pData(rv$current.obj)),
#     tags$h4("colnames(fData(obj))"),
#     HTML(colnames(Biobase::fData(rv$current.obj))),
#     tags$h4("colnames(exprs(obj))"),
#     HTML(colnames(Biobase::exprs(rv$current.obj)))
#   )
#   
#   
# })