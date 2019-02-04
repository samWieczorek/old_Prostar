
# tabPanel("Imputation",
#         value = "imputationProteinLevelTabs",
#         uiOutput("proteinLevelImputationPanel")
# )


tabPanel("Miss. values imputation",
         value = "imputationProteinLevelTabs",
         moduleProcessUI("moduleProcess_ProtImputation")
        
)





