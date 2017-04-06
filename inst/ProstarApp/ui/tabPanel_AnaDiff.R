tabPanel("Differential analysis",
         tabsetPanel(
             "diffAnalysis_tabSetPanel",
             id = "diffAnalysis_tabSetPanel",
             tabPanel("1 - Volcano plot",
                      value = "DiffAnalysis_Volcanoplot",
                      sidebarCustom(),
                      splitLayout(cellWidths = c(widthLeftPanel, widthRightPanel),
                                  wellPanel(id = "sidebar_DiffAna1",
                                            height = "100%"
                                            ,h4("Differential analysis options")
                                            ,uiOutput("diffAnalysis_sidebarPanelTab1")
                                  ),
                                  conditionalPanel(id = "wellPanel_DifferentialAnalysisTab1",
                                                   condition = "true",
                                                   fluidRow(
                                                       column(width=6, uiOutput("nbSelectedItems")),
                                                       column(width=6, uiOutput("selectTooltipInfo"))),
                                                   DT::dataTableOutput("infosVolcanoTable"),
                                                   #plotOutput("volcanoplot", height="500px", width="600px")
                                                   highchartOutput("volcanoplot_rCharts", height="500px", width="600px")
                                                   
                                  )
                      )
             ),
             tabPanel("2 - p-value calibration",
                      value = "DiffAnalysis_Calibrate",
                      sidebarCustom(),
                      splitLayout(cellWidths = c(widthLeftPanel, widthRightPanel),
                                  wellPanel(id = "sidebar_DiffAna2",
                                            height = "100%"
                                            ,h4("Calibration")
                                            ,uiOutput("diffAnalysis_sidebarPanelTab2")
                                  ),
                                  conditionalPanel(id = "wellPanel_DifferentialAnalysisTab2",
                                                   condition = "true",
                                                   htmlOutput("errMsgCalibrationPlotAll"),
                                                   busyIndicator("Calculation in progress",wait = 0),
                                                   plotOutput("calibrationPlotAll"),
                                                   uiOutput("errMsgCalibrationPlot"),
                                                   busyIndicator("Calculation in progress",wait = 0),
                                                   plotOutput("calibrationPlot")
                                  )
                      )
             ),
             tabPanel("3 - FDR",
                      id = "DiffAnalysis_viewFDR",
                      value = "DiffAnalysis_viewFDR",
                      sidebarCustom(),
                      splitLayout(cellWidths = c(widthLeftPanel, widthRightPanel),
                                  wellPanel(id = "sidebar_DiffAna3",
                                            height = "100%"
                                            ,h4("Compute FDR")
                                            ,uiOutput("diffAnalysis_sidebarPanelTab3")
                                  ),
                                  
                                  conditionalPanel(id = "wellPanel_DifferentialAnalysisTab3",
                                                   condition = "true",
                                                   uiOutput("nbSelectedItemsStep3"),
                                                   hr(),
                                                   fluidRow(
                                                       column(width= 4, htmlOutput("equivPVal")),
                                                       column(width= 4, htmlOutput("showFDR"))
                                                   ),
                                                   DT::dataTableOutput("infosVolcanoTableStep3"),
                                                   #plotOutput("volcanoplot", height="500px", width="600px")
                                                   highchartOutput("volcanoplot_rCharts_Step3", height="500px", width="600px")
                                                   
                                                   
                                                   # plotOutput("volcanoplotStep3",height="500px",width="600px")
                                  )
                      )
             ), # end tabPanel(title = "3 - Visualize FDR"
             tabPanel("4 - Validate & save",
                      id = "panelDiffAnaSave",
                      value = "DiffAnalysis_ValidateAndSave",
                      sidebarCustom(),
                      splitLayout(cellWidths = c(widthLeftPanel, widthRightPanel),
                                  wellPanel(id = "sidebar_DiffAna4",
                                            height = "100%",
                                            actionButton("ValidDiffAna","Save diff analysis")
                                  ),
                                  conditionalPanel(id = "wellPanel_DifferentialAnalysisTab4",
                                                   condition = "true",
                                                   DT::dataTableOutput("showSelectedItems"),
                                                   br()
                                                   ,uiOutput("DiffAnalysisSaved")
                                  )
                      )
             ) # end tabPanel(title = "4 - Validate and Save", 
         ) # end tabsetPanel
)