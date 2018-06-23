
output$FAQ_output <- renderUI({
  
  tagList(
    tags$br(),tags$br(),tags$br(),
    tags$h4("1 - Why the table in experimental design blinks when I am editing it?"),
    tags$p("When you edit the experimental design (during converting a text file to MSnset or during the update of the design),
           it may happen that the cells begin to blink in a random order. Then, no more operation is possible in the table. 
           This happens if you edit the cells too fast w.r.t. the speed of update of the table. We apologize for this caveat : this is a known bug of the package used to
           provide the table. No fix is available yet. The only workaround is to close then reopen Prostar."),
    tags$br(),
    tags$h4("2 - How to build a valid experimental design?"),
    tags$p("The differential analysis of Prostar now integrates hierarchical designs when using limma. TODO")
    
    
    )
})