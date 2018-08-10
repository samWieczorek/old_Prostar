
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
    tags$p("The differential analysis with ProStaR is devoted to the processing of", tags$b("hierarchical unpaired experimental designs"), 
            ". However, in former versions, this was not explicit enough, so that users with paired samples could used ProStaR 
            with wrong assumptions. To clear this out, we have changed the experimental design construction step so that 
            its explicitly appears unpaired."),

    tags$p("As a result, the samples must now be numbered as in the following example:"),
    tags$ul(tags$li("Condition 1: 1 - 2 - 3 - 4,"),tags$li("Condition 2: 5 - 6 - 7 - 8")),
    tags$p("As opposed to:"),
    tags$ul(tags$li("Condition 1: 1 - 2 - 3 - 4,"),tags$li("Condition 2: 1 - 2 - 3 - 4")),
    tags$p("Which, depending on the context, could suggest that the 8 samples comes only from 
            4 different biological subjects, and thus leading to paired tests - For instance,
           patients that are compared between Before (Condition 1) and After (Condition 2) some treatment."),
    tags$p("However, one should note that even if the experimental design now looks different, this is just 
           due to a numbering convention, and the statistical test is not impacted."),
    tags$br(),
    
    tags$h4("3 - The items of the contextual menus for plots are 'undefined'"),
    tags$p("This happens if the version of the package 'highcharter' is less or equal to 0.5.0. To fix this issue, you
           should install the devel version of the package by typing the following command in a R console:
           devtools::install_github('jbkunst/highcharter')"),
    tags$br()
    )
})