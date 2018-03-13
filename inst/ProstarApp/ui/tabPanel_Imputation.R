tabPanel("Miss. values imputation",
         tabsetPanel(
             #"diffAnalysis_tabSetPanel",
             id = "Imputation_tabSetPanel",
             tabPanel("1 - Classical Missing values",
                      value = "Classical_MV",
                      sidebarCustom(),
                      splitLayout(cellWidths = c(widthLeftPanel, widthRightPanel),
                                  wellPanel(id = "sidebar_Imputation1",
                                            height = "100%",
                                            h4("Miss. values imputation options"),
                                            br(),
                                            #actionButton("reset.imputation.button", "Reset"),
                                            uiOutput("ClassicalMV_chooseImputationMethod"),
                                            uiOutput("ClassicalMV_Params"),
                                            actionButton("perform.imputationClassical.button",
                                                         "Perform classical MV imputation")
                                  ),
                                  tagList(
                                      busyIndicator(WaitMsgCalc,wait = 0),
                                      uiOutput("showImputationPanel"),
                                      uiOutput("ClassicalMV_detQuant_impValues"),
                                      dataTableOutput("TAB_ClassicalMV_detQuant_impValues"),
                                      fluidRow(
                                          column(width = 5, plotOutput("viewNAbyMean"
                                                                       , height = plotHeight, width = "400px"))
                                          ,column(width = 7, plotOutput("showImageNA"
                                          ))
                                      )
                                      
                                  )
                      )
             ),
             tabPanel("2 - LAPALA",
                      value = "LAPALA_MV",
                      sidebarCustom(),
                      splitLayout(cellWidths = c(widthLeftPanel, widthRightPanel),
                                  wellPanel(id = "sidebar_Imputation2",
                                            height = "100%",
                                            uiOutput("Lapala_chooseImputationMethod"),
                                            uiOutput("Lapala_Params"),
                                            actionButton("perform.imputationLAPALA.button","Perform imputation")
                                  ),
                                  tagList(
                                      busyIndicator(WaitMsgCalc,wait = 0),
                                      uiOutput("Lapala_detQuant_impValues"),
                                      
                                      dataTableOutput("TAB_Lapala_detQuant_impValues"),
                                      busyIndicator(WaitMsgPlot,wait = 0),
                                      fluidRow(
                                          column(width = 5, plotOutput("viewNAbyMean_LAPALA"
                                                                       , height = plotHeight, width = "400px"))
                                          ,column(width = 7, plotOutput("showImageNA_LAPALA")
                                          ))
                                  )
                      )
             ),
            tabPanel("4 - Validate & save",
                      value = "Imputation_ValidateAndSave",
                      sidebarCustom(),
                      splitLayout(cellWidths = c(widthLeftPanel, widthRightPanel),
                                  wellPanel(id = "sidebar_DiffAna4",
                                            height = "100%",
                                            busyIndicator(WaitMsgCalc,wait = 0),
                                            actionButton("ValidImputation", 
                                                         "Save imputation",
                                                         styleclass = "primary")
                                  ),
                                  tagList(
                                      #DT::dataTableOutput("showSelectedItems"),
                                      #             br()
                                      uiOutput("ImputationSaved")
                                  )
                      )
             ) # end tabPanel(title = "4 - Validate and Save", 
         ) # end tabsetPanel
)



