tabPanel("Export",
         value = "ExportTab",
         tabsetPanel(
           id = "Export_tabSetPanel",
           tabPanel("Process summary",
                    dataTableOutput("viewProcessingData")
                    ),
           tabPanel("Export to file",
                    value = "export",
                    helpText("Export format of the dataset and filename."),
                    
                    uiOutput("choosedataToExportMSnset"),
                    hr(),
                    uiOutput("exportOptions")
           ),
           
           tabPanel("Build report (Beta)",
                    tagList(
                     
                      bsCollapse(id = "collapseExport", open = c("Options"),
                                 bsCollapsePanel("Plots for data processing tools", 
                                                 tags$div(
                                                   tags$div( style="display:inline-block; vertical-align: middle;",
                                                             hidden(checkboxGroupInput("plotsFor.Original.protein", "Plots for original", choices=plots.dataProcessing$Original, selected=as.character(0)))),
                                                   tags$div( style="display:inline-block; vertical-align: middle;",
                                                             hidden(checkboxGroupInput("plotsFor.Original.peptide", "Plots for original", choices=plots.dataProcessing$Original, selected=as.character(0)))),
                                                   tags$div( style="display:inline-block; vertical-align: middle;",
                                                             hidden(checkboxGroupInput("plotsFor.Filtered.protein", "Plots for Filtered", choices=plots.dataProcessing$Filtered, selected=as.character(0)))),
                                                   tags$div( style="display:inline-block; vertical-align: middle;",
                                                             hidden(checkboxGroupInput("plotsFor.Filtered.peptide", "Plots for Filtered", choices=plots.dataProcessing$Filtered, selected=as.character(0)))),
                                                   
                                                   tags$div( style="display:inline-block; vertical-align: middle;",
                                                             hidden(checkboxGroupInput("plotsFor.Normalized.protein", "Plots for Normalized", choices=plots.dataProcessing$Normalized, selected=as.character(0)))),
                                                   tags$div( style="display:inline-block; vertical-align: middle;",
                                                             hidden(checkboxGroupInput("plotsFor.Normalized.peptide", "Plots for Normalized", choices=plots.dataProcessing$Normalized, selected=as.character(0)))),
                                                   tags$div( style="display:inline-block; vertical-align: middle;",
                                                             hidden(checkboxGroupInput("plotsFor.Aggregated.protein", "Plots for Aggregated", choices=plots.dataProcessing$Aggregated, selected=as.character(0)))),
                                                   tags$div( style="display:inline-block; vertical-align: middle;",
                                                             hidden(checkboxGroupInput("plotsFor.Aggregated.peptide", "Plots for Aggregated", choices=plots.dataProcessing$Aggregated, selected=as.character(0)))),
                                                   
                                                   tags$div( style="display:inline-block; vertical-align: middle;",
                                                             hidden(checkboxGroupInput("plotsFor.Imputed.peptide", "Plots for Imputed.peptide", choices=plots.dataProcessing$Imputed.peptide, selected=as.character(0)))),
                                                   tags$div( style="display:inline-block; vertical-align: middle;",
                                                             hidden(checkboxGroupInput("plotsFor.Imputed.protein", "Plots for Imputed.protein", choices=plots.dataProcessing$Imputed.protein, selected=as.character(0)))),
                                                   tags$div( style="display:inline-block; vertical-align: middle;",
                                                             hidden(checkboxGroupInput("plotsFor.HypothesisTest.protein", "Plots for hypothesisTest", choices=plots.dataProcessing$HypothesisTest, selected=as.character(0)))),
                                                   tags$div( style="display:inline-block; vertical-align: middle;",
                                                             hidden(checkboxGroupInput("plotsFor.HypothesisTest.peptide", "Plots for hypothesisTest", choices=plots.dataProcessing$HypothesisTest, selected=as.character(0))))
                                                 ),
                                                 style = "primary"),
                                 bsCollapsePanel("Plots for data mining tools", tagList(), style = "primary"),
                                 bsCollapsePanel("Options", 
                                                 tagList(
                                                   tags$div( style="display:inline-block; vertical-align: middle; padding-right: 40px;",
                                                             selectInput("sizePNGplots", "Size of images (PNG)", choices = c("1200 * 800"), width='150px')),
                                                 tags$div( style="display:inline-block; vertical-align: middle; padding-right: 40px;",
                                                           selectInput("resoPNGplots", "Resolution", choices = c(150), width='150px')),
                                                 radioButtons('format', 'Report document format', c('PDF', 'HTML', 'Word'),inline = TRUE)
                                                 ), style = "primary")
                                 
                      ),
                      
                      br(),
                      actionButton("generateReport", "Generate report"),
                      downloadButton('downloadReport', "Download report")
                    )
           )
           
         )
)
