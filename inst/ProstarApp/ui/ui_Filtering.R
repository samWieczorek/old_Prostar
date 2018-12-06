

tabPanel("Filter data",
         value = "FilterDataTab",

         tags$div(tags$div( style="align: center;display:inline-block; vertical-align: top; padding: 7px",
                            actionButton("prevBtnFiltering", "<<", class = PrevNextBtnClass,style='padding:4px; font-size:80%')),
                  tags$div( style="align: center;display:inline-block; vertical-align: top;",
                            uiOutput("checkFilteringPanel" )),
                  tags$div( style="align: center;display:inline-block; vertical-align: top; padding: 7px",
                            actionButton("nextBtnFiltering", ">>", class = PrevNextBtnClass, style='padding:4px; font-size:80%')),
                  
                  tags$div( style="align: center;display:inline-block; vertical-align: top; padding: 7px",
                            uiOutput("filteringDone"))
         ),
         hr(),
         uiOutput("mv_Filtering"),
         uiOutput("stringBased_Filtering"),
         uiOutput("valid_Filtering")

)

