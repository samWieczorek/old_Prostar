

tabPanel("Descriptive statistics",
         value="DescriptiveStatisticsTab",
         tabsetPanel(id="DS_tabSetPanel",
                     #------------------------------------------------------------
                     tabPanel("Overview",
                              value = "DS_tabGeneral",
                              uiOutput("showOverviewDS")
                              
                     ),
                     
                     tabPanel(
                         "Miss. values",
                         value = "DS_tabOverviewMV",
                         uiOutput("plotsMissingV")
                         ),
                     
                     #-------------------------------------------------------------
                     tabPanel(title="Data explorer",
                              value = "DS_DataExplorer",
                               tagList(
                                       uiOutput("DS_sidebarPanel_tab"),
                                       uiOutput("tabToShow")
                                       )
                     ),
                     
                     tabPanel("Corr. matrix",
                              value="DS_tabCorrMatrix",
                              uiOutput("plotsCorM")
                              
                     ),
                     
                     tabPanel("Heatmap",
                              value="DS_tabHeatmap",
                              sidebarCustom(),
                              splitLayout(cellWidths = c(widthLeftPanel, widthRightPanel),
                                          wellPanel(id = "sidebar_heatmap",
                                                    uiOutput("DS_sidebarPanel_heatmap")
                                          ),
                                          tagList(
                                            uiOutput("DS_PlotHeatmap")
                                          )
                              )
                     ),
                     tabPanel("PCA",
                              value="DS_PCA",
                              uiOutput("pcaPlots")
                     ),
                     
                     #-----------------------------------------------------------
                     tabPanel("Intensity distr.",
                              value="DS_tabDensityplot",
                              uiOutput("IntensityStatsPlots")

                     ),
                     
                     #-----------------------------------------------------------
                     tabPanel("CV distr.", 
                              value="DS_tabDistVar",
                              uiOutput("distCVPlot")
                    
                            )
                     )
         )



