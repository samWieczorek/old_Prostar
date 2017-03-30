tabPanel("Descriptive statistics",
         #id="tabView",
         icon = icon("bar-chart-o"),
         tabsetPanel(id="DS_tabSetPanel",
                     #------------------------------------------------------------
                     tabPanel("Overview",
                              value = "DS_tabGeneral",
                              
                              uiOutput("overviewNewData")
                     ),
                     
                     tabPanel(
                         "Miss. values",
                         value = "DS_tabOverviewMV",
                         
                         helpText("Those bargraph plots display some information to
                                  view the distribution of missing values."),
                         fluidRow(
                             column(width = 4, 
                                    plotOutput("histoMV_Image_DS")
                             ),
                             column(width = 4, 
                                    plotOutput("histo.missvalues.per.lines_DS")),
                             column(width = 4, 
                                    plotOutput("histo.missvalues.per.lines.per.conditions_DS"))
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
                                          uiOutput("tabToShow")
                              )
                     ),
                     
                     tabPanel("Corr. matrix",
                              value="DS_tabCorrMatrix",
                              sidebarCustom(),
                              
                              splitLayout(cellWidths = c(widthLeftPanel, widthRightPanel),
                                          wellPanel(id = "sidebar_Corrmatrix",
                                                    sliderInput("expGradientRate",
                                                                "Tune to modify the gradient of color",
                                                                min = 2,max = 6,value = defaultGradientRate,step=0.05)
                                          ),
                                          tagList(
                                              plotOutput("corrMatrix",width = plotWidth,
                                                                      height = plotHeight)
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
                                          tagList(
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
                                          tagList(
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
                                                           plotOutput("viewDensityplot_DS",
                                                                      width = plotWidth,
                                                                      height = plotHeight)
                                          )
                              )
                     ),
                     
                     #-----------------------------------------------------------
                     tabPanel("CV distr.", 
                              value="DS_tabDistVar",
                              p("This graphics shows, for each condition, the distribution 
                                of the CV of the log-intensities."),
                              plotOutput("viewDistCV",
                                         width = plotWidth,
                                         height = plotHeight)
                              )
                     )
         )
