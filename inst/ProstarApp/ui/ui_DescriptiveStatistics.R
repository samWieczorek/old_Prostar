

tabPanel("Descriptive statistics",
         value="DescriptiveStatisticsTab",
         tabsetPanel(id="DS_tabSetPanel",
                     #------------------------------------------------------------
                     tabPanel("Overview",
                              value = "DS_tabGeneral",
                              tagList(
                                br(),
                                mod_staticDT_ui("overview_DS")
                                )
                     ),
                     
                     tabPanel(
                         "Quantification nature",
                         value = "DS_tabOverviewMV",
                         uiOutput("plotsMVHistograms")
                         
                     ),
                     
                     #-------------------------------------------------------------
                     tabPanel(title="Data explorer",
                              value = "DS_DataExplorer",
                              mod_MSnSetExplorer_ui(id = 'test')
                     ),
                     
                     tabPanel("Corr. matrix",
                              value="DS_tabCorrMatrix",
                              checkboxInput('showDataLabels', 'Show labels', value=FALSE),
                              uiOutput("plotsCorM")
                             ),
                     
                     tabPanel("Heatmap",
                              value="DS_tabHeatmap",
                              uiOutput("plotsHeatmap")
                              
                     ),
                     tabPanel("PCA",
                              value="DS_PCA",
                              uiOutput("plotsPCA")

                     ),
                     
                     #-----------------------------------------------------------
                     tabPanel("Intensity distr.",
                              value="DS_tabDensityplot",
                              uiOutput("IntensityStatsPlots")
                              ),

                     
                     #-----------------------------------------------------------
                     tabPanel("CV distr.", 
                              value="DS_tabDistVar",
                              uiOutput("plotsDistCV")
                              )
                     )
         )



