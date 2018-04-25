tabPanel("Differential analysis",
         tabsetPanel(
             #"diffAnalysis_tabSetPanel",
             id = "diffAnalysis_tabSetPanel",
             tabPanel("1 - Global tuning",
                      value = "DiffAnalysis_GlobalTuning",
                      sidebarCustom(),
                      splitLayout(cellWidths = c(widthLeftPanel, widthRightPanel),
                                  wellPanel(
                                      id = "sidebar_DiffAna1",
                                            height = "100%"
                                            #,h4("Differential analysis global options")
                                            ,
                                      uiOutput("diffAnalysis_GlobalOptions_SB")
                                      
                                  ),
                                  tagList(
                                      busyIndicator("Building plot, please wait",wait = 0),
                                      highchartOutput("FoldChangePlot", height="500px", width="600px")
                                      
                                  )
                      )
             ),
             

             tabPanel("2 - Pairwise comparison",
                      value = "DiffAnalysis_PairewiseComparison",
                      sidebarCustom(),
                      splitLayout(cellWidths = c(widthLeftPanel, widthRightPanel),
                                  wellPanel(
                                      id = "sidebar_DiffAna2",
                                            height = "100%"
                                            ,uiOutput("newComparisonUI")
                                          ,h4("Comparison options")
                                            ,uiOutput("diffAnalysis_PairwiseComp_SB")
                                      ,actionButton("AnaDiff_perform.filtering.MV", "Perform")
                                  ),
                                  tagList(
                                       busyIndicator("Building plot, please wait",wait = 0),
                                       moduleVolcanoplotUI("volcano_Step1")
                                       )
                      )
             ),
             tabPanel("3 - p-value calibration",
                      value = "DiffAnalysis_Calibrate",
                      sidebarCustom(),
                      splitLayout(cellWidths = c(widthLeftPanel, widthRightPanel),
                                  wellPanel(
                                      id = "sidebar_DiffAna3",
                                            height = "100%"
                                            ,h4("Calibration")
                                            ,uiOutput("diffAnalysis_Calibration_SB")
                                  ),
                                  tagList(
                                      htmlOutput("errMsgCalibrationPlotAll"),
                                                   busyIndicator("Building plot, please wait",wait = 0),
                                                   plotOutput("calibrationPlotAll"),
                                                   uiOutput("errMsgCalibrationPlot"),
                                                   busyIndicator("Building plot, please wait",wait = 0),
                                                   plotOutput("calibrationPlot")
                                  )
                      )
             ),
             tabPanel("4 - FDR",
                      id = "DiffAnalysis_viewFDR",
                      value = "DiffAnalysis_viewFDR",
                      sidebarCustom(),
                      splitLayout(cellWidths = c(widthLeftPanel, widthRightPanel),
                                  wellPanel(id = "sidebar_DiffAna4",
                                            height = "100%"
                                            ,h4("Compute FDR")
                                            ,uiOutput("diffAnalysis_FDR_SB")
                                  ),
                                  
                                  tagList(
                                        fluidRow(
                                                column(width= 4, htmlOutput("equivPVal")),
                                                column(width= 4, htmlOutput("showFDR"))
                                                ),
                                        hr(),
                                        busyIndicator("Building plot, please wait",wait = 0),
                                        moduleVolcanoplotUI("volcano_Step2"),
                                        DT::dataTableOutput("showSelectedItems")
                                        )
                      )
             ), # end tabPanel(title = "3 - Visualize FDR"
             tabPanel("5 - Validate & save",
                      id = "panelDiffAnaSave",
                      value = "DiffAnalysis_ValidateAndSave",
                      sidebarCustom(),
                      splitLayout(cellWidths = c(widthLeftPanel, widthRightPanel),
                                  wellPanel(id = "sidebar_DiffAna5",
                                            height = "100%",
                                            busyIndicator(WaitMsgCalc,wait = 0),
                                            actionButton("ValidDiffAna","Save diff analysis")
                                  ),
                                  tagList(
                                      uiOutput("DiffAnalysisSaved")

                                  )
                      )
             ) # end tabPanel(title = "4 - Validate and Save", 
         ) # end tabsetPanel
)
