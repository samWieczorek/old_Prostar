
callModule(moduleInsertMarkdown, "FAQ_MD",URL_FAQ)

# 
# 
# output$FAQ_output <- renderUI({
#   
#   tagList(
#     
#     ## table of contents
#     tags$h3("Table of contents"),
#     tags$ol(
#       tags$li(tags$a("Why does the table in experimental design blink during edition?", href="#tableBlinks")),
#       tags$li(tags$a("How to build a valid experimental design?", href="#buildDesign")),
#       tags$li(tags$a("Why do the items of the contextual menus for plots remain 'undefined'?", href="#undefinedItems")),
#       tags$li(tags$a("Why does my volcano plot look so aligned?", href="#alignedVolcano"))
#     ),
#     
#     
#     
#     
#     tags$hr(),
#     tags$h4(id="tableBlinks", "1 - Why does the table in experimental design blink during edition?"),
#     tags$p("When you edit the experimental design (during converting a text file to MSnset or during the update of the design),
#            it may happen that the cells begin to blink in a random order. Then, no more operation is possible in the table. 
#            This happens if you edit the cells too fast with respect to the table update speed. We apologize for this caveat : this is a known bug of the package used to
#            provide the table. No patch is available yet. The only workaround is to close then reopen Prostar."),
#     tags$br(),
#     
#     
#     tags$h4( id="buildDesign", "2 - How to build a valid experimental design?"),
#     tags$p("In Prostar, the differential analysis is devoted is devoted to the processing of", tags$b("hierarchical unpaired experimental designs"), 
#            ". However, in former versions, this was not explicit enough, so that users with paired samples could used Prostar 
#            with wrong assumptions. To clear this out, we have changed the experimental design construction step so that 
#            its explicitly appears unpaired."),
#     
#     tags$p("As a result, the samples must now be numbered as in the following example:"),
#     tags$ul(tags$li("Condition 1: 1 - 2 - 3 - 4,"),tags$li("Condition 2: 5 - 6 - 7 - 8")),
#     tags$p("As opposed to:"),
#     tags$ul(tags$li("Condition 1: 1 - 2 - 3 - 4,"),tags$li("Condition 2: 1 - 2 - 3 - 4")),
#     tags$p("Which, depending on the context, could suggest that the 8 samples comes only from 
#             4 different biological subjects, and thus leading to paired tests - for instance,
#            patients that are compared between Before (Condition 1) and After (Condition 2) some treatment."),
#     tags$p("However, one should note that even if the experimental design now looks different, this is just 
#            due to a numbering convention, and the statistical test is not impacted."),
#     tags$br(),
#     
#     tags$h4(id="undefinedItems", "3 - Why do the items of the contextual menus for plots remain 'undefined'?"),
#     tags$p("This happens if the version of the package 'highcharter' is less or equal to 0.5.0. To fix this issue, you
#            should install the devel version of the package by typing the following command in a R console:
#            devtools::install_github('jbkunst/highcharter')"),
#     tags$br(),
#     
#     tags$h4(id="alignedVolcano", "4 - Why does my volcano plot look so aligned?"),
#     tags$p("In very uncommun situations, one may obtain a bowl shape volcano plot such as depicted above. 
#             This is due to using Limma on a dataset for which it is not adapted: 
#             Briefly, the numerical values in the quantitative matrix appears to have a repetitive pattern 
#             that prevent Limma routines to compute the number of degrees of freedom of the Chi2 distribution 
#             on which the protein variances should be fitted. As a result, Limma returns a result directly 
#             proportional to the fold-change, and the p-values are none-informative. 
#             In such cases, which are fortunately extremely odd, we advise to replace Limma test by a classical t-test."),
#     tags$img(src='images/dfPriorIssue.png', 
#              style="display: block; margin-left: auto; margin-right: auto; box-shadow:inset 0px 2px 8px #333;"),
#     tags$br()
#     )
# })
