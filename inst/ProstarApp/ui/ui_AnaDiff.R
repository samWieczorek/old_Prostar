tabPanel("Differential analysis",
         value = "diffAnalysisTab",
         uiOutput("checkDiffAnaPanel" ),
         actionButton("prevBtnDiffAna", "< Previous"),
         actionButton("nextBtnDiffAna", "Next >"),
         hr(),
         
         uiOutput("anaDiffPanel")
         
)
