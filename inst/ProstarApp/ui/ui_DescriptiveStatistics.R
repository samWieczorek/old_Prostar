

tabPanel("Descriptive statistics",
         value="DescriptiveStatisticsTab",
         tabsetPanel(id="DS_tabSetPanel",
                     #------------------------------------------------------------
                     tabPanel("Overview",
                              value = "DS_tabGeneral",
                              tagList(
                                br(),
                                moduleStaticDataTableUI("overview_DS")
                                )
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
                               tagList(
                                       uiOutput("DS_sidebarPanel_tab"),
                                       uiOutput("tabToShow")
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
                     tabPanel("PCA",
                              value="DS_PCA",
                              tagList(
                                     uiOutput("WarningNA_PCA"),
                                     uiOutput("pcaOptions"),
                                            
                                     fluidRow(
                                       column(width=6,  plotOutput("pcaPlotVar")),
                                       column(width=6,  plotOutput("pcaPlotInd"))
                                     ),
                                     fluidRow(
                                        column(width=6,  highchartOutput("pcaPlotEigen")),
                                        column(width=6,  moduleStaticDataTableUI("PCAvarCoord"))
                                      )
                              )

                     ),
                     
                     #-----------------------------------------------------------
                     tabPanel("Intensity distr.",
                              value="DS_tabDensityplot",
                              # shinyWidgets::dropdownButton(
                              #   tags$div(
                              #     tags$div(style="display:inline-block; vertical-align: top;",
                              #              selectInput("whichGroup2Color",
                              #                          "Color lines",
                              #                          choices=list("By condition" = "Condition",
                              #                                       "By replicate" = "Replicate"), 
                              #                          selected=GetWhichGroup2Color(), width='150px')
                              #     ),
                              #     tags$div(style="display:inline-block; vertical-align: top;",
                              #              uiOutput("ChooseLegendForSamples")
                              #     )
                              #   ),
                              #   tooltip="Plots parameters",
                              #   style = "material-circle", icon = icon("gear"), status = "primary"
                              # ),
                              
                              tagList(
                                tags$div(

                                tags$div(style="display:inline-block; vertical-align: top;",
                                         selectInput("whichGroup2Color",
                                                     "Color lines",
                                                     choices=list("By condition" = "Condition",
                                                                  "By replicate" = "Replicate"),
                                                     selected=GetWhichGroup2Color(), width='150px')
                                ),
                                tags$div(style="display:inline-block; vertical-align: top;",
                                         uiOutput("ChooseLegendForSamples")
                                )
                              )
                              ),
                              fluidRow(
                                  column(width=6,moduleDensityplotUI("densityPlot_DS")),
                                   column(width=6, moduleBoxplotUI("boxPlot_DS"))
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



