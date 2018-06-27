output$BugReport_output <- renderUI({
  rv$current.obj
  
  mail <- unlist(strsplit(maintainer("Prostar"), "<"))[2]
  mail <- unlist(strsplit(mail, ">"))[1]
  
  
  body <- rv$current.obj
  
  tagList(
    
    tags$p("If you encounter an issue with Prostar, you can send a prefilled mail to the maintainer so he will have sufficient informations
           to start identify the problem. In addition, you can choose to add the MSnset as an attachment to the mail."),
    
    
    a(actionButton(inputId = "email1", label = "Contact maintainer", 
                   icon = icon("envelope", lib = "font-awesome")),
      href=paste0("mailto:", mail,"?subject=[Prostar bug report]&body=", body)
      ) 
    
    
    )
})