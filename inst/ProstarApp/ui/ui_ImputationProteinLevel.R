tabPanel("Miss. values imputation",
        value = "imputationProteinLevelTabs",
        
        uiOutput("checkProtImputPanel" ),
        actionButton("prevBtnProtImput", "< Previous"),
        actionButton("nextBtnProtImput", "Next >"),
        hr(),
        
        
        
        
        
        uiOutput("POV_imputation"),
        uiOutput("MEC_imputation"),
        uiOutput("Validate_ProtImput")
)





