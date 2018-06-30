

tabPanel("Descriptive statistics",
         value="DescriptiveStatisticsTab",
         tabsetPanel(id="DS_tabSetPanel",
                     #------------------------------------------------------------
                     tabPanel("Overview",
                              value = "DS_tabGeneral",
                              moduleDatasetOverviewUI("overview_DS")
                     ),
                     
                     tabPanel(
                         "Miss. values",
                         value = "DS_tabOverviewMV",
                         
                         helpText("These barplots display the distribution of missing values in the dataset."),
                         missingValuesPlotsUI("MVPlots_DS") 
                         ),
                     
                     #-------------------------------------------------------------
                     tabPanel(title="Data explorer",
                              value = "DS_DataExplorer",
                              sidebarCustom(),
                              splitLayout(cellWidths = c(widthLeftPanel, widthRightPanel),
                                          wellPanel(id = "sidebar_dataExplorer",
                                                    uiOutput("DS_sidebarPanel_tab")
                                          ),
                                          tagList(
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
                                          highchartOutput("corrMatrix",width = plotWidth,height = plotHeight) %>% withSpinner(type=spinnerType)
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
                                          tagList(
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
                                             moduleBoxplotUI("boxPlot_DS")
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
                                              plotOutput("viewViolinPlot_DS",width = plotWidth,
                                                                      height = plotHeight) %>% withSpinner(type=spinnerType)
                                          )
                              )
                     ),
                     
                     #-----------------------------------------------------------
                     tabPanel("Densityplot",
                              value="DS_tabDensityplot",
                              sidebarCustom(),
                              splitLayout(cellWidths = c(widthLeftPanel, widthRightPanel),
                                          wellPanel(id = "sidebar_densityplot",
                                                    tagList(
                                                      radioButtons("whichGroup2Color_DS",
                                                                   "Color lines",
                                                                   choices=list("By condition" = "Condition",
                                                                                "By replicate" = "Replicate")),
                                                      br()
                                                      #uiOutput("nShow_DS")
                                                      )
                                          ),
                                          tagList(
                                              moduleDensityplotUI("densityPlot_DS")
                                           )
                              )
                     ),
                     
                     #-----------------------------------------------------------
                     tabPanel("CV distr.", 
                              value="DS_tabDistVar",
                              helpText("Display the condition-wise distributions of the log-intensity CV (Coefficient of Variation) 
                                of the protein/peptides."),
                                helpText("For better visualization, it is possible to zoom in by click-and-drag."),
                              highchartOutput("viewDistCV",width = plotWidth, height = plotHeight) %>% withSpinner(type=spinnerType)
                              )
                     )
         )



