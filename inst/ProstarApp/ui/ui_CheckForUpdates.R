tabPanel("Check for updates",
                  value="checkForUpdatesTab",
                  br(),
                  br(),
                  uiOutput("baseVersions"),
                  dataTableOutput("tab_versions", width = '600px'),
                  uiOutput("update")
         )

