# tabPanel("Differential analysis",
#          value = "diffAnalysisTab",
#          uiOutput("anaDiffPanel")
#          
# )


tabPanel("Differential analysis",
         value = "diffAnalysisTab",
         
         tags$div(tags$div( style="align: center;display:inline-block; vertical-align: top; padding: 7px",
                            actionButton("prevBtnDiffAna", "<<", class = PrevNextBtnClass,style='padding:4px; font-size:80%')),
                  tags$div( style="align: center;display:inline-block; vertical-align: top;",
                            uiOutput("checkDiffAnaPanel" )),
                  tags$div( style="align: center;display:inline-block; vertical-align: top; padding: 7px",
                            actionButton("nextBtnDiffAna", ">>", class = PrevNextBtnClass, style='padding:4px; font-size:80%'))
                  
                  
         ),
         

         hr(),
         
         uiOutput("anaDiffPanel")
         
)
