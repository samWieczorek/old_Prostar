tabPanel(title="Global settings",
         value="GlobalSettingsTab",
         # selectInput("settings_InteractivePlots",
         #             "Type of plots",
         #             choices = c("Interactive (nice but slower)" = "Interactive",
         #                     "Static (faster)" = "Static")),
         modulePopoverUI("modulePopover_numPrecision"),
         numericInput("settings_nDigits", "", value=3, min=0)
         
         
)
         