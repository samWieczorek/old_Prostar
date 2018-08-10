tabPanel("Normalization",
         value = "Normalization",
         sidebarCustom(),
         splitLayout(cellWidths = c(widthLeftPanel, widthRightPanel),
                     wellPanel(id = "sidebar_Normalization"
                               ,height = "100%"
                               ,uiOutput("choose_Normalization_Test")
                               ,uiOutput("choose_normalizationType")
                               ,uiOutput("choose_normalizationScaling")
                               ,uiOutput("choose_normalizationQuantile")
                               ,uiOutput("choose_normalizationQuantileOther")
                               ,actionButton("perform.normalization", "Perform normalization", width="170px")
                               ,br(),br()
                               ,actionButton("valid.normalization","Save normalization", width="170px")
                     )
                     ,tagList(
                       uiOutput("helpForNormalizationMethods"),
                       fluidRow(
                                column(width=6, moduleDensityplotUI("densityPlot_Norm") %>% withSpinner(type=spinnerType)),
                                column(width=6, plotOutput("viewComparisonNorm_DS") %>% withSpinner(type=spinnerType))),
                                moduleBoxplotUI("boxPlot_Norm") %>% withSpinner(type=spinnerType)
                                )
                    )

         )
