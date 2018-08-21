<<<<<<< HEAD
tabPanel(title="Global settings",
         value="GlobalSettingsTab",
         # selectInput("settings_InteractivePlots",
         #             "Type of plots",
         #             choices = c("Interactive (nice but slower)" = "Interactive",
         #                     "Static (faster)" = "Static")),
         div(
           div(
             style="display:inline-block; vertical-align: middle; padding-right: 20px;",
             modulePopoverUI("modulePopover_numPrecision")
           ),
           div(
             style="display:inline-block; vertical-align: middle;",
             numericInput("settings_nDigits", "", value=3, min=0, width="100px")
           )
         )
          
)
=======
tabPanel(title="Global settings",
         value="GlobalSettingsTab",
         # selectInput("settings_InteractivePlots",
         #             "Type of plots",
         #             choices = c("Interactive (nice but slower)" = "Interactive",
         #                     "Static (faster)" = "Static")),
         div(
           div(
             style="display:inline-block; vertical-align: middle; padding-right: 20px;",
             modulePopoverUI("modulePopover_numPrecision")
           ),
           div(
             style="display:inline-block; vertical-align: middle;",
             numericInput("settings_nDigits", "", value=3, min=0, width="100px")
           )
         )
          
)
>>>>>>> ceee6a0719f73dbf86eb71708e3099eee6d98083
         