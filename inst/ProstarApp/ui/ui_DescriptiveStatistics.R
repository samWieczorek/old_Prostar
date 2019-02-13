
uiOutput("showOverviewDS")
tabPanel("Descriptive statistics",
         value="DescriptiveStatisticsTab",
         tabsetPanel(id="DS_tabSetPanel",
                     #------------------------------------------------------------
                     tabPanel(title  ="Overview",
                               uiOutput("showOverviewDS")
                     ),
                     
                     tabPanel(title="Miss. values",
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

                     tabPanel(title="Corr. matrix",
                              value="DS_tabCorrMatrix",
                              uiOutput("plotsCorM")
                      ),

                     tabPanel(title="Heatmap",
                              value="DS_tabHeatmap",
                              tagList(
                                div(
                                  div(
                                    style="display:inline-block; vertical-align: middle; padding-right: 20px;",
                                    selectInput("distance","Distance",
                                                choices = G_heatmapDistance_Choices,
                                                selected = rv$PlotParams$heatmap.distance,
                                                width="150px")
                                  ),
                                  div(
                                    style="display:inline-block; vertical-align: middle;",
                                    selectInput("linkage","Linkage",
                                                choices=G_heatmapLinkage_Choices,
                                                selected=rv$PlotParams$heatmap.linkage,
                                                width="150px")
                                  ),

                                tags$hr(),
                                uiOutput("DS_PlotHeatmap")
                                )
                              )

                     ),
                     tabPanel(title="PCA",
                              value="DS_PCA",
                              tagList(
                                     uiOutput("WarningNA_PCA"),
                                     uiOutput("pcaOptions"),
                                     uiOutput("pcaPlots")
                              )

                     ),

                      #-----------------------------------------------------------
                      tabPanel("Intensity distr.",
                               value="DS_tabDensityplot",
                                uiOutput("IntensityStatsPlots")
                              
                       ),
                      
                      
                     # #-----------------------------------------------------------
                     tabPanel("CV distr.",
                              value="DS_tabDistVar",
                              helpText("Display the condition-wise distributions of the log-intensity CV (Coefficient of Variation)
                                of the protein/peptides."),
                                helpText("For better visualization, it is possible to zoom in by click-and-drag."),
                              uiOutput("distCVPlot")
                              )
                     )
         )



