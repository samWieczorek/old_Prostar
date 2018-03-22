tabPanel("Miss. values imputation",
         tabsetPanel(
             #"diffAnalysis_tabSetPanel",
             id = "Imputation_tabSetPanel",
             
             tabPanel("1 - Partially Observed Values",
                      value = "Classical_MV",
                      #sidebarCustom(),
                      splitLayout(cellWidths = c(widthLeftPanel, widthRightPanel),
                                  wellPanel(id = "sidebar_Imputation1",
                                            height = "100%",
                                            h4("Miss. values imputation options"),
                                            br(),
                                            uiOutput("sidebar_imputation_step1")
                                            
                                  ),
                                  tagList(
                                      busyIndicator(WaitMsgCalc,wait = 0),
                                      htmlOutput("helpForImputation"),
                                      uiOutput("ClassicalMV_detQuant_impValues"),
                                      dataTableOutput("TAB_ClassicalMV_detQuant_impValues"),
                                      moduleMVPlotsUI("mvImputationPlots_MV"),
                                      uiOutput("ImputationStep1Done")
                                      )
                                      
                                  )
             ),
             tabPanel("2 - Missing on the Entire Condition",
                      value = "LAPALA_MV",
                      #sidebarCustom(),
                      splitLayout(cellWidths = c(widthLeftPanel, widthRightPanel),
                                  wellPanel(id = "sidebar_Imputation2",
                                            height = "100%",
                                            uiOutput("Lapala_chooseImputationMethod"),
                                            uiOutput("Lapala_Params"),
                                            actionButton("perform.imputationLAPALA.button","Perform imputation")
                                  ),
                                  tagList(
                                      htmlOutput("warningLapalaImputation"),
                                      busyIndicator(WaitMsgCalc,wait = 0),
                                      uiOutput("Lapala_detQuant_impValues"),
                                      dataTableOutput("TAB_Lapala_detQuant_impValues"),
                                      moduleMVPlotsUI("mvImputationPlots_LAPALA"),
                                      uiOutput("ImputationStep2Done")
                                      
                                  )
                      )
             ),
            tabPanel("3 - Validate & save",
                      value = "Imputation_ValidateAndSave",
                      #sidebarCustom(),
                      splitLayout(cellWidths = c(widthLeftPanel, widthRightPanel),
                                  wellPanel(id = "sidebar_Imputation3",
                                            height = "100%",
                                            busyIndicator(WaitMsgCalc,wait = 0),
                                            actionButton("ValidImputation", 
                                                         "Save imputation",
                                                         styleclass = "primary")
                                  ),
                                  tagList(
                                      #DT::dataTableOutput("showSelectedItems"),
                                      moduleMVPlotsUI("mvImputationPlots_Valid"),
                                      uiOutput("ImputationSaved")
                                  )
                      )
             ) # end tabPanel(title = "4 - Validate and Save", 
         ) # end tabsetPanel
)



