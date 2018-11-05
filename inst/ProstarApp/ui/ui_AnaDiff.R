# tabPanel("Differential analysis",
#          value = "diffAnalysisTab",
#          uiOutput("anaDiffPanel")
#          
# )


tabPanel("Differential analysis",
         value = "diffAnalysisTab",
         uiOutput("checkDiffAnaPanel" ),
         actionButton("prevBtnDiffAna", "< Previous", class = PrevNextBtnClass),
         actionButton("nextBtnDiffAna", "Next >", class = PrevNextBtnClass),
         hr(),
         
         uiOutput("anaDiffPanel")
         
)
