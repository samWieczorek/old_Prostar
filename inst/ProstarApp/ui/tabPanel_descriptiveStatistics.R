tabPanel("Descriptive statistics",
         #id="tabView",
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
                         fluidRow(
                             column(width = 4,
                                    highchartOutput("histoMV_Image_DS")
                             ),
                             column(width = 4,
                                    highchartOutput("histo_missvalues_per_lines_DS")),
                             column(width = 4,
                                    highchartOutput("histo_missvalues_per_lines_per_conditions_DS"))
                         )

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
                                          highchartOutput("corrMatrix",width = plotWidth,height = plotHeight)
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
                                                           busyIndicator("Calculation in progress",wait = 0),
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
                                                    uiOutput("DS_sidebarPanel_Boxplot")
                                          ),
                                          conditionalPanel(id = "wellPanelBoxplot",
                                                           condition = "true",
                                                           plotOutput("viewBoxPlot_DS",width = plotWidth,
                                                                      height = plotHeight)
                                          )
                              )
                     ),
                     
                     
                     #-----------------------------------------------------------
                     tabPanel("Violinplot",
                              value="DS_tabViolinplot",
                              sidebarCustom(),
                              splitLayout(cellWidths = c(widthLeftPanel, widthRightPanel),
                                          wellPanel(id = "sidebar_Violonplot",
                                                    uiOutput("DS_sidebarPanel_Violinplot")
                                          ),
                                          conditionalPanel(id = "wellPanelViolinplot",
                                                           condition = "true",
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
                                          tagList(highchartOutput("viewDensityplot_DS",
                                                                      width = plotWidth,
                                                                      height = plotHeight)
                                          )
                              )
                     ),
                     
                     #-----------------------------------------------------------
                     tabPanel("CV distr.", 
                              value="DS_tabDistVar",
                              p("Display the condition-wise distributions of the log-intensity CV (Coefficient of Variation) 
                                of the protein/peptides. For better visualization, it is advised to zoom in the [0,20] interval."),
                              highchartOutput("viewDistCV",
                                         width = plotWidth,
                                         height = plotHeight)
                              )
                     )
         )
