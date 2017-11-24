

tabPanel("Descriptive statistics",
         valueid="Descriptive statistics",
         tabsetPanel(id="DS_tabSetPanel",
                     #------------------------------------------------------------
                     tabPanel("Overview",
                              value = "DS_tabGeneral",
                              uiOutput("overviewNewData")
                     ),
                     
                     tabPanel(
                         "Miss. values",
                         value = "DS_tabOverviewMV",
                         
                         helpText("These barplots display the distribution of missing values in the dataset."),
                         busyIndicator("Building plot. Please wait...",wait = 0),
                         missingValuesPlotsUI("MVPlots_DS")
                         ),
                     
                     #-------------------------------------------------------------
                     tabPanel(title="Data explorer",
                              #id = "DS_DataExplorer",
                              sidebarCustom(),
                              splitLayout(cellWidths = c(widthLeftPanel, widthRightPanel),
                                          wellPanel(id = "sidebar_dataExplorer",
                                                    uiOutput("DS_sidebarPanel_tab")
                                          ),
                                          conditionalPanel(height = heightWellPanel,
                                                           condition = "true",
                                                           uiOutput("tabToShow")
                                          )
                              )
                     ),
                     
                     tabPanel("Corr. matrix",
                              value="DS_tabCorrMatrix",
                              sidebarCustom(),
                              
                              splitLayout(cellWidths = c(widthLeftPanel, widthRightPanel),
                                          wellPanel(id = "sidebar_Corrmatrix",
                                                    sliderInput("expGradientRate",
                                                                "Tune to modify the color gradient",
                                                                min = 0,max = 1,value = defaultGradientRate,step=0.01)
                                          ),
                                          tagList(
                                              busyIndicator("Building plot. Please wait...",wait = 0),
                                          highchartOutput("corrMatrix",width = plotWidth,height = plotHeight)
                                          )
                              )
                     ),
                     
                     tabPanel("Heatmap",
                              value="DS_tabHeatmap",
                              sidebarCustom(),
                              splitLayout(cellWidths = c(widthLeftPanel, widthRightPanel),
                                          wellPanel(id = "sidebar_heatmap",
                                                    uiOutput("DS_sidebarPanel_heatmap")
                                          ),
                                          conditionalPanel(id = "wellPanelHeatmap",
                                                           condition = "true",
                                                           width = 800,
                                                           # HTML("For this view, it is necessary that your dataset 
                                                           #     does not contains any NA lines. <br> Please check 
                                                           #     your data and use Filtering options or missing 
                                                           #     values imputation."),
                                                           busyIndicator("Building plot. Please wait...",wait = 0),
                                                           uiOutput("DS_PlotHeatmap")
                                          )
                              )
                     ),
                     
                     #-----------------------------------------------------------
                     tabPanel("Boxplot",
                              value="DS_tabBoxplot",
                              sidebarCustom(),
                              splitLayout(cellWidths = c(widthLeftPanel, widthRightPanel),
                                          wellPanel(id = "sidebar_boxplot",
                                                    #uiOutput("DS_sidebarPanel_Boxplot")
                                                    uiOutput("ChooseLegendForAxis_DS")
                                          ),
                                         tagList(
                                             busyIndicator("Building plot. Please wait...",wait = 0),
                                             moduleBoxplotUI("boxPlot_DS")
                                            # plotOutput("viewBoxPlot_DS",width = plotWidth,
                                             #                         height = plotHeight)
                                          )
                              )
                     ),
                     
                     
                     #-----------------------------------------------------------
                     tabPanel("Violinplot",
                              value="DS_tabViolinplot",
                              sidebarCustom(),
                              splitLayout(cellWidths = c(widthLeftPanel, widthRightPanel),
                                          wellPanel(id = "sidebar_Violonplot",
                                                    #uiOutput("DS_sidebarPanel_Violinplot")
                                                    uiOutput("ChooseLegendForAxisViolin_DS")
                                          ),
                                          tagList(
                                              busyIndicator("Building plot. Please wait...",wait = 0),
                                              plotOutput("viewViolinPlot_DS",width = plotWidth,
                                                                      height = plotHeight)
                                          )
                              )
                     ),
                     
                     #-----------------------------------------------------------
                     tabPanel("Densityplot",
                              value="DS_tabDensityplot",
                              sidebarCustom(),
                              splitLayout(cellWidths = c(widthLeftPanel, widthRightPanel),
                                          wellPanel(id = "sidebar_densityplot",
                                                    uiOutput("DS_sidebarPanel_Densityplot")
                                          ),
                                          tagList(
                                              busyIndicator("Building plot. Please wait...",wait = 0),
                                              moduleDensityplotUI("densityPlot_DS")
                                          # highchartOutput("viewDensityplot_DS",
                                          #                             width = plotWidth,
                                          #                             height = plotHeight)
                                          )
                              )
                     ),
                     
                     #-----------------------------------------------------------
                     tabPanel("CV distr.", 
                              value="DS_tabDistVar",
                              helpText("Display the condition-wise distributions of the log-intensity CV (Coefficient of Variation) 
                                of the protein/peptides."),
                                helpText("For better visualization, it is possible to zoom in by click-and-drag."),
                              busyIndicator("Building plot. Please wait...",wait = 0),
                              highchartOutput("viewDistCV",
                                         width = plotWidth,
                                         height = plotHeight)
                              )
                     )
         )



