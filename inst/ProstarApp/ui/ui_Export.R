tabPanel("Export",
         value = "ExportTab",
         tabsetPanel(
           id = "Export_tabSetPanel",
           tabPanel("Process summary",
                    tagList(
                      moduleStaticDataTableUI("viewProstarVersions"),
                    br(),br(),br(),
                    moduleStaticDataTableUI("viewProcessingData")
                    )
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
                                                   tags$div( style="display:inline-block; vertical-align: top;",
                                                             hidden(tags$div(id='treeFor_Original_protein',
                                                                             tagList(
                                                                               tags$p(tags$b("Original protein")),
                                                                               shinyTree("plotsFor_Original_protein",theme="proton", themeIcons = FALSE, themeDots = FALSE, checkbox = TRUE)
                                                                               )
                                                                             )
                                                                    )
                                                             ),
                                                   tags$div( style="display:inline-block; vertical-align: top;",
                                                             hidden(tags$div(id='treeFor_Original_peptide',
                                                                             tagList(
                                                                               tags$p(tags$b("Original peptide")),
                                                                                shinyTree("plotsFor_Original_peptide",theme="proton", themeIcons = FALSE, themeDots = FALSE, checkbox = TRUE)
                                                                                )
                                                                             )
                                                                    )
                                                             ),
                                                   
                                                   tags$div( style="display:inline-block; vertical-align: top;",
                                                             hidden(tags$div(id='treeFor_Filtered_protein',
                                                                             tagList(
                                                                               tags$p(tags$b("Filtered protein")),
                                                                               shinyTree("plotsFor_Filtered_protein",theme="proton", themeIcons = FALSE, themeDots = FALSE, checkbox = TRUE)
                                                                               )
                                                                             )
                                                                    )
                                                             ),
                                                   
                                                   tags$div( style="display:inline-block; vertical-align: top;",
                                                             hidden(tags$div(id='treeFor_Filtered_peptide',
                                                                             tagList(
                                                                               tags$p(tags$b("Filtered peptide")),
                                                                               shinyTree("plotsFor_Filtered_peptide",theme="proton", themeIcons = FALSE, themeDots = FALSE, checkbox = TRUE)
                                                                               )
                                                                             )
                                                                    )
                                                             ),

                                                   tags$div( style="display:inline-block; vertical-align: top;",
                                                             hidden(tags$div(id='treeFor_Normalized_protein',
                                                                             tagList(
                                                                               tags$p(tags$b("Normalized protein")),
                                                                               shinyTree("plotsFor_Normalized_protein",theme="proton", themeIcons = FALSE, themeDots = FALSE, checkbox = TRUE)
                                                                               )
                                                                             )
                                                                    )
                                                             ),
                                                   
                                                   tags$div( style="display:inline-block; vertical-align: top;",
                                                              hidden(tags$div(id='treeFor_Normalized_peptide',
                                                                              tagList(
                                                                                tags$p(tags$b("Normalized peptide")),
                                                                                shinyTree("plotsFor_Normalized_peptide",theme="proton", themeIcons = FALSE, themeDots = FALSE, checkbox = TRUE)
                                                                                )
                                                                              )
                                                                     )
                                                             ),

                                                   tags$div( style="display:inline-block; vertical-align: top;",
                                                             hidden(tags$div(id='treeFor_Aggregated_protein',
                                                                             tagList(
                                                                               tags$p(tags$b("Aggregated protein")),
                                                                               shinyTree("plotsFor_Aggregated_protein",theme="proton", themeIcons = FALSE, themeDots = FALSE, checkbox = TRUE)
                                                                               )
                                                                             )
                                                                    )
                                                             ),
                                                   
                                                   
                                                   tags$div( style="display:inline-block; vertical-align: top;",
                                                             hidden(tags$div(id='treeFor_Aggregated_peptide',
                                                                             tagList(
                                                                               tags$p(tags$b("Aggregated peptide")),
                                                                               shinyTree("plotsFor_Aggregated_peptide",theme="proton", themeIcons = FALSE, themeDots = FALSE, checkbox = TRUE)
                                                                               )
                                                                             )
                                                                    )
                                                             ),
                                                 
                                                   tags$div( style="display:inline-block; vertical-align: top;",
                                                             hidden(tags$div(id='treeFor_Imputed_protein',
                                                                             tagList(
                                                                               tags$p(tags$b("Imputed protein")),
                                                                               shinyTree("plotsFor_Imputed_protein",theme="proton", themeIcons = FALSE, themeDots = FALSE, checkbox = TRUE)
                                                                               )
                                                                             )
                                                                    )
                                                             ),
                                                   
                                                   tags$div( style="display:inline-block; vertical-align: top;",
                                                             hidden(tags$div(id='treeFor_Imputed_peptide',
                                                                             tagList(
                                                                               tags$p(tags$b("Imputed peptide")),
                                                                               shinyTree("plotsFor_Imputed_peptide",theme="proton", themeIcons = FALSE, themeDots = FALSE, checkbox = TRUE)
                                                                               )
                                                                             )
                                                                    )
                                                             ),
                                                 
                                                   tags$div( style="display:inline-block; vertical-align: top;",
                                                             hidden(tags$div(id='treeFor_HypothesisTest_protein',
                                                                             tagList(
                                                                               tags$p(tags$b("HypothesisTest protein")),
                                                                               shinyTree("plotsFor_HypothesisTest_protein",theme="proton", themeIcons = FALSE, themeDots = FALSE, checkbox = TRUE)
                                                                               )
                                                                             )
                                                                    )
                                                             ),
                                                   tags$div( style="display:inline-block; vertical-align: top;",
                                                              hidden(tags$div(id='treeFor_HypothesisTest_peptide',
                                                                              tagList(
                                                                                tags$p(tags$b("HypothesisTest peptide")),
                                                                                shinyTree("plotsFor_HypothesisTest_peptide",theme="proton", themeIcons = FALSE, themeDots = FALSE, checkbox = TRUE)
                                                                                )
                                                                              )
                                                                     )
                                                             )
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
                      actionButton("generateReport", "Generate report", class = actionBtnClass),
                      downloadButton('downloadReport', "Download report", class = actionBtnClass)
                    )
           )
           
         )
)
