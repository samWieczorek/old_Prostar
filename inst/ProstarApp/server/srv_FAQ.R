
output$FAQ_output <- renderUI({
  
  tagList(
    tags$br(),tags$br(),tags$br(),
    tags$h4("1 - Why does the experimental design table blink when I am editing it?"),
    tags$p("When you edit the experimental design (during CSV file conversion into an MSnset, or during design update of an older MSnset),
           it may happen that the cells begin to blink in a random order. Then, no more operation is possible in the table. 
           This happens if you edit the cells too fast with respect to the reaction capability of your machine. 
           We apologize for this caveat : this is a known bug of the package used to
           provide the table, but no patch is available to date. The only workaround is to close ProStaR and to restart."),
    tags$br(),
    
    
    tags$h4("2 - How to build a valid experimental design?"),
    tags$p("The differential analysis with ProStaR is devoted to the processing of", tags$b("hierarchical unpaired experimental designs"), 
            ". However, in former versions, this was not explicit enough, so that users with paired samples could used ProStaR 
            with wrong assumptions. To clear this out, we have changed the experimental design construction step so that 
            it explicitly appears unpaired."),

    tags$p("As a result, the samples must now be numbered as in the following example:"),
    tags$ul(tags$li("Condition 1: 1 - 2 - 3 - 4,"),tags$li("Condition 2: 5 - 6 - 7 - 8")),
    tags$p("As opposed to:"),
    tags$ul(tags$li("Condition 1: 1 - 2 - 3 - 4,"),tags$li("Condition 2: 1 - 2 - 3 - 4")),
    tags$p("Which, depending on the context, could suggest that the 8 samples comes only from 
            4 different biological subjects, and thus leading to paired tests - For instance,
           patients that are compared between Before (Condition 1) and After (Condition 2) some treatment."),
    tags$p("However, one should note that even if the experimental design now looks different, this is just 
           due to a numbering convention, and the statistical test is not impacted.")
    )
})
