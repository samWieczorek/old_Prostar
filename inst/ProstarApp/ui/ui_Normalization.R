tabPanel("Normalization",
         value = "Normalization",
         sidebarCustom(),
         splitLayout(cellWidths = c(widthLeftPanel, widthRightPanel),
                     wellPanel(id = "sidebar_Normalization"
                               ,height = "100%"
                               ,h4("Normalization options")
                               ,uiOutput("choose_Normalization_Test")
                               ,uiOutput("choose_normalizationType")
                               ,uiOutput("choose_normalizationScaling")
                               ,uiOutput("choose_normalizationQuantile")
                               ,uiOutput("choose_normalizationQuantileOther")
                               ,checkboxInput("plotOptions", "Show plot options", 
                                              value = FALSE)
                               ,actionButton("perform.normalization", 
                                             "Perform normalization", 
                                             width="170px")
                               ,br(),br()
                               ,actionButton("valid.normalization",
                                             "Save normalization",
                                             width="170px")
                     )
                     ,tagList(uiOutput("helpForNormalizationMethods"),
                                       #plotOutput("viewBoxPlotNorm")
                              busyIndicator("Building plot, please wait",wait = 0),
                              fluidRow(
                                           column(width=6, moduleDensityplotUI("densityPlot_Norm")),
                                           column(width=6, plotOutput("viewComparisonNorm_DS"))),
                                       moduleBoxplotUI("boxPlot_Norm")

                     )
         ),
         tags$head(
             tags$style(type="text/css", 
                        "#AbsolutePanelPlotOptions {
                        background-color:transparent;"
             )
             ),
         absolutePanel(id  = "AbsolutePanelPlotOptions",
                       top = 200,
                       right = 50,
                       width = "200px",
                       height = "50px",
                       draggable = TRUE,
                       fixed = FALSE,
                       cursor = "move",
                       uiOutput("AbsShowOptions")
         )
         )
