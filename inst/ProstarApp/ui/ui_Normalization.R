tabPanel("Normalization",
         value = "Normalization",
         tagList(
           
           
           div(
             div(
               style="display:inline-block; vertical-align: middle; padding-right: 20px;",
               selectInput("normalization.method","Normalization method", 
                           choices = normMethods, 
                           selected = rv$widgets$normalization$method,
                           width='200px')
             ),
             div(
               style="display:inline-block; vertical-align: middle;",
               hidden(selectInput("normalization.type", "Normalization type",  
                                  choices = c("overall", "within conditions"), 
                                  selected = rv$widgets$normalization$type,
                                  width='150px'))
             ),
             div(
               style="display:inline-block; vertical-align: middle; padding-right: 20px;",
               hidden(numericInput("spanLOESS", "Span",min=0, max = 1 , value = rv$widgets$normalization$spanLOESS,step = 0.1, width='100px')),
               uiOutput("choose_normalizationQuantile"),
               uiOutput("choose_normalizationScaling")
             ),
             div(
               style="display:inline-block; vertical-align: middle;",
               hidden(actionButton("perform.normalization", "Perform normalization", class = actionBtnClass, width="170px")),
               hidden(actionButton("valid.normalization","Save normalization", class = actionBtnClass, width="170px"))
             )
           ),
           uiOutput("helpForNormalizationMethods"),
           tags$hr(),
           fluidRow(
                    column(width=4, moduleDensityplotUI("densityPlot_Norm")),
                    column(width=4,moduleBoxplotUI("boxPlot_Norm")  %>% withSpinner(type=spinnerType)),
                    column(width=4,plotOutput("viewComparisonNorm_DS") %>% withSpinner(type=spinnerType))
                    )
         )
)
