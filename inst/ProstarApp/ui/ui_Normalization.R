tabPanel("Normalization",
         value = "Normalization",
         tagList(
           fluidRow(
             column(width=2, uiOutput("choose_Normalization_Test")),
             column(width=2, uiOutput("choose_normalizationType")),
             column(width=2, uiOutput("choose_normalizationScaling")),
             column(width=2, uiOutput("choose_normalizationQuantile")),
             column(width=2, uiOutput("choose_normalizationQuantileOther")),
             column(width=2, actionButton("perform.normalization", "Perform normalization", width="170px"),
                              hidden(actionButton("valid.normalization","Save normalization", width="170px")))),
           uiOutput("helpForNormalizationMethods"),
           tags$hr(),
           fluidRow(
                    column(width=4, moduleDensityplotUI("densityPlot_Norm")),
                    column(width=4, plotOutput("viewComparisonNorm_DS") %>% withSpinner(type=spinnerType)),
                    column(width=4,moduleBoxplotUI("boxPlot_Norm") %>% withSpinner(type=spinnerType))
                    )
         )
)
