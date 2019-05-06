source(file.path(".", "modules/Plots/moduleMSnSetExplorer.R"),  local = TRUE)$value
source(file.path(".", "modules/Plots/moduleGroupMVPlots.R"),  local = TRUE)$value
source(file.path(".", "modules/Plots/moduleCorrMatrix.R"),  local = TRUE)$value
source(file.path(".", "modules/Plots/moduleHeatmap.R"),  local = TRUE)$value
source(file.path(".", "modules/Plots/modulePCAPlots.R"),  local = TRUE)$value
source(file.path(".", "modules/Plots/moduleIntensityPlots.R"), local = TRUE)$value
source(file.path(".", "modules/Plots/moduleVarDistPlot.R"), local = TRUE)$value


moduleDescriptiveStatsUI <- function(id)
{
  ns <- NS(id)
  
  
  tabPanel("Descriptive statistics",
           value="DescriptiveStatisticsTab",
           tabsetPanel(id="DS_tabSetPanel",
                       #------------------------------------------------------------
                       tabPanel("Overview",
                                value = "DS_tabGeneral",
                                tagList(
                                  br(),
                                  moduleStaticDataTableUI(ns("overview_DS"))
                                )
                       ),
                       
                       #-------------------------------------------------------------
                       tabPanel(title="Data explorer",
                                value = "DS_DataExplorer",
                                MSnSetExplorerUI(ns('msnsetExplorer_DS'))
                       ),
                       
                       tabPanel(
                         "Miss. values",
                         value = "DS_tabOverviewMV",
                         missingValuesPlotsUI(ns("MVPlots_DS"))
                         ),
                       
                       
                       
                       tabPanel("Corr. matrix",
                                value="DS_tabCorrMatrix",
                                moduleCorrMatrixUI(ns("corrMatrixPlot_DS"))
                       ),
                       
                       tabPanel("Heatmap",
                                value="DS_tabHeatmap",
                                moduleHeatmapUI(ns("heatmap_DS"))
                                
                       ),
                       tabPanel("PCA",
                                value="DS_PCA",
                                modulePCAUI(ns("pca_DS"))
                                
                       ),
                       
                       #-----------------------------------------------------------
                       tabPanel("Intensity distr.",
                                value="DS_tabDensityplot",
                                moduleIntensityPlotsUI(ns('modIntensityPlots_DS'))
                       ),
                       
                       
                       #-----------------------------------------------------------
                       tabPanel("CV distr.", 
                                value="DS_tabDistVar",
                                moduleVarDistPlotUI(ns("modVarDistrib_DS"))
                       )
           )
  )
  
  


  
}



moduleDescriptiveStats <- function(input, output, session, dataIn){
  ns <- session$ns
  
  
  callModule(moduleStaticDataTable,"overview_DS", 
             table2show=reactive({GetDatasetOverview2(dataIn()$obj)}))
  
  callModule(module=MSnSetExplorer, 'msnsetExplorer_DS',
             dataIn = reactive({dataIn()}))
  
  callModule(missingValuesPlots, "MVPlots_DS", 
             dataIn = reactive({dataIn()}))
  
  callModule(moduleCorrMatrix, "corrMatrixPlot_DS",
             dataIn = reactive({dataIn()}))
  
  callModule(moduleHeatmap, "heatmap_DS",
             dataIn = reactive({dataIn()}))
  
  callModule(modulePCA, "pca_DS",
             dataIn = reactive({dataIn()}))

callModule(moduleIntensityPlots, "modIntensityPlots_DS",
             dataIn = reactive({dataIn()}))

callModule(moduleVarDistPlot, 'modVarDistrib_DS',
           dataIn = reactive({dataIn()}))
}