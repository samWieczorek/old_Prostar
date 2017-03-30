tabPanel("Miss. values imputation",
         id = "tabPanelImputation",
         value = "imputation",
         sidebarCustom(),
         splitLayout(
             cellWidths = c(widthLeftPanel, widthRightPanel),
                     wellPanel(id = "sidebar_imputation",
                               height = "100%"
                               ,h4("Miss. values imputation options")
                               ,br()
                               ,uiOutput("chooseImputationMethod"),
                               uiOutput("chooseBasicImputationMethod"),
                               uiOutput("MVI_options"),
                               uiOutput("MVI_qmin_option"),
                               uiOutput("OnlyLAPALA_qmin_option"),
                               uiOutput("OnlyLAPALA_distribution_option"),
                               uiOutput("imp4pLAPALA_distribution_option"),
                               actionButton("perform.imputation.button",
                                            "Perform imputation"),
                               actionButton("ValidImputation", 
                                            "Save imputation",
                                            styleclass = "primary"),
                               br(), br(), br()
                               #uiOutput("warningImputationMethod"),
                               
                               ## progress bar
                               #br(),
                               #br(),
                               #uiOutput(outputId = "progressOne")
                     ),
                      
                             tagList(
                                 fluidRow(
                                     column(width = 12, uiOutput("showImputationPanel")))
                                     
                            ,
                         busyIndicator("Calculation in progress",wait = 0),
                         fluidRow(
                             column(width = 5, plotOutput("viewNAbyMean", height = "600px", width = "400px")),
                             column(width = 7, plotOutput("showImageNA"))
                         )
                     )

         )
)