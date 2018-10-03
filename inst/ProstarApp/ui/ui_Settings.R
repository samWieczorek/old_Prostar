tabPanel(title="Global settings",
         value="GlobalSettingsTab",
         # selectInput("settings_InteractivePlots",
         #             "Type of plots",
         #             choices = c("Interactive (nice but slower)" = "Interactive",
         #                     "Static (faster)" = "Static")),
         tabsetPanel(
           tabPanel("Miscallenous",
                    div(
              div(
             style="display:inline-block; vertical-align: middle; padding-right: 20px;",
             modulePopoverUI("modulePopover_numPrecision")
           ),
           div(
             style="display:inline-block; vertical-align: middle;",
             uiOutput("settings_nDigits_UI")
           )
         ),
         tagList(
           tags$div( style="display:inline-block; vertical-align: middle; padding-right: 40px;",
                     selectInput("sizePNGplots", "Size of images (PNG)", choices = c("1200 * 800"), width='150px')),
           tags$div( style="display:inline-block; vertical-align: middle; padding-right: 40px;",
                     selectInput("resoPNGplots", "Resolution", choices = c(150), width='150px'))
           )),
         tabPanel("Colors",
                  hidden(uiOutput("defineColorsUI"))
         )
         )
)
         