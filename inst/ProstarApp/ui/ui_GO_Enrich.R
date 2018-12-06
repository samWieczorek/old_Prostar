tabPanel("GO Analysis",
        value = "GOAnalysisTab",
        
        tags$div(tags$div( style="align: center;display:inline-block; vertical-align: top; padding: 7px",
                           actionButton("prevBtnGO", "<<", class = PrevNextBtnClass,style='padding:4px; font-size:80%')),
                 tags$div( style="align: center;display:inline-block; vertical-align: top;",
                           uiOutput("checkGOPanel" )),
                 tags$div( style="align: center;display:inline-block; vertical-align: top; padding: 7px",
                           actionButton("nextBtnGO", ">>", class = PrevNextBtnClass, style='padding:4px; font-size:80%'))
                 
                 
        ),
        tags$hr(),
        uiOutput("GOAnalysisMenu")
 
)