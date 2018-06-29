
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
    tags$p("The differential analysis of Prostar now integrates ",tags$b("hierarchical paired designs")," with limma. The way of numbering the samples
 is important because it differs whether your samples are paired or not."),

    tags$p("In a ",tags$b("paired experimental design"), ", the biological sample from Condition 1  and Condition 2
is the same. For instance, you have 4 patients, and you monitor them Before (Condition 1) and After (Condition 2) 
some event. In this case, the correct practice is to restart numbering at each condition:"),
      tags$ul(tags$li("Condition 1: 1,2,3,4,"),tags$li("Condition 2: 1,2,3,4")),
      
      tags$p("In an ",tags$b("unpaired experimental design"), ",the biological sample from Condition 1 and Condition 2
is different. For instance, if you have 8 patients in 2 conditions, there is no link between 
              the first patient of each condition. Here, the numbering is just a convention, you could give them any name, as there is no link
              between the samples."),
    tags$ul(tags$li("Condition 1: 1,2,3,4,"),tags$li("Condition 2: 5,6,7,8"))
    )
})