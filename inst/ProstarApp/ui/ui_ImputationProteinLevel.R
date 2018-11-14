
# tabPanel("Imputation",
#         value = "imputationProteinLevelTabs",
#         uiOutput("proteinLevelImputationPanel")
# )


tabPanel("Miss. values imputation",
         value = "imputationProteinLevelTabs",
         
         uiOutput("checkProtImputPanel" ),
         actionButton("prevBtnProtImput", "< Previous", class = PrevNextBtnClass),
         actionButton("nextBtnProtImput", "Next >", class = PrevNextBtnClass),
         hr(),
         uiOutput("POV_imputation"),
         uiOutput("MEC_imputation"),
         uiOutput("Validate_ProtImput")

)





