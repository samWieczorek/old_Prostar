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
         )),
         tabPanel("Colors",
                  hidden(uiOutput("defineColorsUI"))
         )
         )
)
         