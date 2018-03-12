tabPanel("Miss. values imputation",
         id = "tabPanelImputation",
         value = "imputation",
         sidebarCustom(),
         splitLayout(cellWidths = c(widthLeftPanel, widthRightPanel),
                     wellPanel(id = "sidebar_imputation",
                               height = "100%",
                               h4("Miss. values imputation options"),
                               br(),
                               uiOutput("ClassicalMV_chooseImputationMethod"),
                               uiOutput("ClassicalMV_Params"),

                               
                               checkboxInput("imputeLapala", "Impute Lapala", value = FALSE),
                               uiOutput("Lapala_chooseImputationMethod"),
                               uiOutput("Lapala_Params"),
                               
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
                         uiOutput("showImputationPanel"),
                         uiOutput("ClassicalMV_detQuant_impValues"),
                         dataTableOutput("TAB_ClassicalMV_detQuant_impValues"),
                         br(),
                         uiOutput("Lapala_detQuant_impValues"),
                         dataTableOutput("TAB_Lapala_detQuant_impValues"),
                                      busyIndicator(WaitMsgPlot,wait = 0),
                                      #imageOutput("viewNAbyMean"),
                                      fluidRow(
                                          column(width = 5, plotOutput("viewNAbyMean"
                                                                       , height = plotHeight, width = "400px"))
                                          ,column(width = 7, plotOutput("showImageNA"
                                          ))
                                      )
                     )      
                     
         )
)