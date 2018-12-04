
# tabPanel("Imputation",
#         value = "imputationProteinLevelTabs",
#         uiOutput("proteinLevelImputationPanel")
# )


tabPanel("Miss. values imputation",
         value = "imputationProteinLevelTabs",
         
         tags$div(tags$div( style="align: center;display:inline-block; vertical-align: top; padding: 7px",
                            actionButton("prevBtnProtImput", "<<", class = PrevNextBtnClass,style='padding:4px; font-size:80%')),
                  tags$div( style="align: center;display:inline-block; vertical-align: top;",
                            uiOutput("checkProtImputPanel" )),
                  tags$div( style="align: center;display:inline-block; vertical-align: top; padding: 7px",
                            actionButton("nextBtnProtImput", ">>", class = PrevNextBtnClass, style='padding:4px; font-size:80%'))
   
                 
                  ),

        
         hr(),
         uiOutput("POV_imputation"),
         uiOutput("MEC_imputation"),
         uiOutput("Validate_ProtImput")

)





